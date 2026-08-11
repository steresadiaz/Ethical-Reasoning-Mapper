# Ethical Reasoning Mapper (ERM) — Co-occurrence Heatmap
# Companion to erm_analysis.R: visualizes the pairwise associations between
# all 27 variables as a phi-coefficient (binary correlation) heatmap,
# ordered by dimension (ERC / MSC / SJ), with FDR-significant pairs marked.
#
# Usage: Rscript scripts/erm_cooccurrence_heatmap.R [coded_data.csv]
# Reads: outputs/tables/cooccurrence_significance.csv (run erm_analysis.R first)
# Writes: outputs/figures/cooccurrence_heatmap.png

library(tidyverse)
library(here)

args <- commandArgs(trailingOnly = TRUE)
coded_path <- if (length(args) >= 1) args[[1]] else here("data", "erm_example_data_v1.1.csv")
figures_dir <- here("outputs", "figures")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

erc_vars <- paste0("ERC0", 1:8)
msc_vars <- c(paste0("MSC0", 1:9), "MSC10")
sj_vars  <- paste0("SJ0", 1:9)
erm_vars <- c(erc_vars, msc_vars, sj_vars)

erm_data <- read_csv(coded_path, show_col_types = FALSE)

sig_path <- here("outputs", "tables", "cooccurrence_significance.csv")
sig <- if (file.exists(sig_path)) {
  read_csv(sig_path, show_col_types = FALSE)
} else {
  message("No cooccurrence_significance.csv found -- run erm_analysis.R first. Proceeding without significance markers.")
  tibble(variable_1 = character(), variable_2 = character(), p_adjusted = double())
}

# Phi coefficient (= Pearson correlation for two binary variables) for every
# pair. Variables with zero variance (e.g. a variable never coded present)
# have an undefined correlation -- these are set to NA and shown as blank
# cells rather than a spurious value.
mat <- suppressWarnings(cor(erm_data[erm_vars]))
diag(mat) <- NA

phi_long <- as.data.frame(mat) %>%
  rownames_to_column("variable_1") %>%
  pivot_longer(-variable_1, names_to = "variable_2", values_to = "phi")

# Mark cells significant after FDR correction (either pair order)
sig_filtered <- sig %>% filter(p_adjusted < 0.05) %>% select(variable_1, variable_2)
sig_pairs <- bind_rows(
  sig_filtered,
  sig_filtered %>% rename(variable_1 = variable_2, variable_2 = variable_1)
) %>%
  mutate(significant = TRUE)

phi_long <- phi_long %>%
  left_join(sig_pairs, by = c("variable_1", "variable_2")) %>%
  mutate(
    significant = replace_na(significant, FALSE),
    variable_1 = factor(variable_1, levels = erm_vars),
    variable_2 = factor(variable_2, levels = rev(erm_vars))
  )

dim_of <- function(v) case_when(
  v %in% erc_vars ~ "ERC", v %in% msc_vars ~ "MSC", TRUE ~ "SJ"
)
block_breaks <- c(length(erc_vars), length(erc_vars) + length(msc_vars)) + 0.5

p_heatmap <- ggplot(phi_long, aes(x = variable_1, y = variable_2, fill = phi)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(
    data = filter(phi_long, significant, variable_1 != as.character(variable_2)),
    label = "*", color = "white", size = 4, fontface = "bold", vjust = 0.75
  ) +
  scale_fill_gradient2(
    low = "#C1662E", mid = "#FBF7F2", high = "#1F6F5C", midpoint = 0,
    na.value = "#EFEFEF", limits = c(-0.55, 0.55),
    name = "Coeficiente\nphi"
  ) +
  geom_vline(xintercept = block_breaks, color = "#8C8C8C", linewidth = 0.5) +
  geom_hline(yintercept = 27 - block_breaks + 1, color = "#8C8C8C", linewidth = 0.5) +
  coord_fixed() +
  labs(
    title = "Co-ocurrencia entre variables ERM (coeficiente phi)",
    subtitle = "Ordenado por dimensión (ERC | MSC | SJ). * = significativo tras corrección FDR (p < .05)",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", color = "#262626", size = rel(1.15)),
    plot.subtitle = element_text(color = "#595959", size = rel(0.85)),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = rel(0.75), color = "#404040"),
    axis.text.y = element_text(size = rel(0.75), color = "#404040"),
    panel.grid = element_blank(),
    legend.title = element_text(face = "bold", color = "#404040", size = rel(0.85))
  )

ggsave(here(figures_dir, "cooccurrence_heatmap.png"), p_heatmap, width = 10, height = 9, dpi = 300)

message("Co-occurrence heatmap written to ", here(figures_dir, "cooccurrence_heatmap.png"))
