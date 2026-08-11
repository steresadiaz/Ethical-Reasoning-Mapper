# Ethical Reasoning Mapper (ERM) — Pre vs. Post Comparison
# Companion to erm_analysis.R: the main pipeline skips McNemar/Wilcoxon
# automatically whenever more than two timepoints are present (this corpus
# has three: Pre, Post, Continuo). This script isolates Pre vs. Post and
# runs the same tests Section 13/14 of the manual specify for the
# two-timepoint case.
#
# Usage: Rscript scripts/erm_pre_post_comparison.R [coded_data.csv]
# Writes: outputs/tables/mcnemar_pre_vs_post.csv, wilcoxon_pre_vs_post.csv,
#         outputs/figures/dimension_indices_pre_vs_post.png

library(tidyverse)
library(here)

args <- commandArgs(trailingOnly = TRUE)
coded_path <- if (length(args) >= 1) args[[1]] else here("data", "erm_example_data_v1.1.csv")
tables_dir  <- here("outputs", "tables")
figures_dir <- here("outputs", "figures")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

erc_vars <- paste0("ERC0", 1:8)
msc_vars <- c(paste0("MSC0", 1:9), "MSC10")
sj_vars  <- paste0("SJ0", 1:9)
erm_vars <- c(erc_vars, msc_vars, sj_vars)

erm_data <- read_csv(coded_path, show_col_types = FALSE) %>%
  mutate(
    ERC_index = rowSums(across(all_of(erc_vars)), na.rm = TRUE),
    MSC_index = rowSums(across(all_of(msc_vars)), na.rm = TRUE),
    SJ_index  = rowSums(across(all_of(sj_vars)),  na.rm = TRUE)
  ) %>%
  filter(timepoint %in% c("Pre", "Post")) %>%
  mutate(timepoint = factor(timepoint, levels = c("Pre", "Post")))

t1 <- "Pre"; t2 <- "Post"

paired <- erm_data %>%
  select(participant_id, dilemma_id, timepoint, all_of(erm_vars), ERC_index, MSC_index, SJ_index) %>%
  pivot_wider(
    id_cols = c(participant_id, dilemma_id),
    names_from = timepoint,
    values_from = c(all_of(erm_vars), ERC_index, MSC_index, SJ_index)
  ) %>%
  drop_na()

message("Paired Pre-Post observations (same participant x dilemma, both timepoints coded): ", nrow(paired))

# McNemar's test per variable (Cohen's-kappa-independent test for correlated
# binary proportions; McNemar, 1947)
codebook <- read_csv(here("codebook", "ERM_variable_dictionary_v1.1.csv"), show_col_types = FALSE) %>%
  rename(variable_label = variable)

mcnemar_results <- map_dfr(erm_vars, function(v) {
  col1 <- paired[[paste0(v, "_", t1)]]
  col2 <- paired[[paste0(v, "_", t2)]]
  tab <- table(factor(col1, levels = c(0, 1)), factor(col2, levels = c(0, 1)))
  n_pre  <- sum(col1)
  n_post <- sum(col2)
  result <- tryCatch(mcnemar.test(tab, correct = TRUE), error = function(e) NULL)
  if (is.null(result)) return(NULL)
  tibble(
    variable = v, n_pairs = nrow(paired),
    n_present_pre = n_pre, n_present_post = n_post,
    pct_pre = n_pre / nrow(paired), pct_post = n_post / nrow(paired),
    statistic = unname(result$statistic), p_value = result$p.value
  )
}) %>%
  mutate(p_adjusted = p.adjust(p_value, method = "BH")) %>%
  left_join(select(codebook, id, variable_label, dimension), by = c("variable" = "id")) %>%
  relocate(variable_label, dimension, .after = variable) %>%
  arrange(p_value)

write_csv(mcnemar_results, here(tables_dir, "mcnemar_pre_vs_post.csv"))

sig_mcnemar <- mcnemar_results %>% filter(p_adjusted < 0.05)
message(
  "McNemar (Pre vs Post): ", nrow(sig_mcnemar), " of ", nrow(mcnemar_results),
  " variables significant after FDR correction (p_adjusted < .05)."
)

# Wilcoxon signed-rank test per dimension index (Wilcoxon, 1945)
wilcoxon_results <- map_dfr(c("ERC_index", "MSC_index", "SJ_index"), function(idx) {
  x <- paired[[paste0(idx, "_", t1)]]
  y <- paired[[paste0(idx, "_", t2)]]
  result <- tryCatch(wilcox.test(x, y, paired = TRUE), error = function(e) NULL)
  if (is.null(result)) return(NULL)
  tibble(
    dimension_index = idx, n_pairs = nrow(paired),
    mean_pre = mean(x), mean_post = mean(y), mean_diff = mean(y - x),
    statistic = unname(result$statistic), p_value = result$p.value
  )
})

write_csv(wilcoxon_results, here(tables_dir, "wilcoxon_pre_vs_post.csv"))

# Figure: dimension indices, Pre vs Post only, same palette as the main pipeline
erm_palette <- c(ERC = "#1F6F5C", MSC = "#C1662E", SJ = "#9C7A1E")
erm_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", color = "#262626", size = rel(1.15)),
    axis.title = element_text(color = "#404040"),
    axis.text = element_text(color = "#595959"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#E5E5E5"),
    legend.title = element_text(face = "bold", color = "#404040")
  )

index_pre_post <- erm_data %>%
  group_by(timepoint) %>%
  summarise(ERC = mean(ERC_index), MSC = mean(MSC_index), SJ = mean(SJ_index), .groups = "drop") %>%
  pivot_longer(-timepoint, names_to = "dimension", values_to = "mean_index")

p_pre_post <- ggplot(index_pre_post, aes(x = timepoint, y = mean_index, color = dimension, group = dimension)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.6) +
  scale_color_manual(values = erm_palette) +
  labs(title = "ERM Dimension Indices: Pre vs. Post", x = "Timepoint", y = "Mean index", color = "Dimension") +
  erm_theme

ggsave(here(figures_dir, "dimension_indices_pre_vs_post.png"), p_pre_post, width = 6, height = 5, dpi = 300)

message("Pre vs Post comparison complete. Tables and figure written to ", tables_dir, " / ", figures_dir, ".")
