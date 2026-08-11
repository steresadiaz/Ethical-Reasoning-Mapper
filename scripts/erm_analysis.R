# Ethical Reasoning Mapper (ERM) — Analysis Pipeline
# Implements the R Workflow specified in Section 14 of the ERM Coding Manual v1.1
# Companion script for: Diaz Torres & Torres-Rivera (2026), ERM Coding Manual v1.1
#
# Usage:
#   Rscript scripts/erm_analysis.R [coded_data.csv] [pilot_double_coded.csv]
#   With no arguments, runs on the bundled example dataset (data/erm_example_data_v1.1.csv)
#   so the pipeline works out of the box; pass your own coded data as the first
#   argument for real use, and (optionally) a pilot double-coded file as the second.
#
# Writes: outputs/tables/*.csv, outputs/figures/*.png (created if missing)

# 0. Setup --------------------------------------------------------------

required_pkgs <- c("tidyverse", "here", "irr", "janitor", "binom")
missing_pkgs <- required_pkgs[!required_pkgs %in% rownames(installed.packages())]
if (length(missing_pkgs) > 0) {
  if (length(getOption("repos")) == 0 || getOption("repos")["CRAN"] == "@CRAN@") {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  }
  install.packages(missing_pkgs)
}

library(tidyverse)
library(here)
library(irr)
library(janitor)
library(binom)

args <- commandArgs(trailingOnly = TRUE)
coded_path  <- if (length(args) >= 1) args[[1]] else here("data", "erm_example_data_v1.1.csv")
pilot_path  <- if (length(args) >= 2) args[[2]] else here("data", "erm_pilot_example_v1.1.csv")
tables_dir  <- here("outputs", "tables")
figures_dir <- here("outputs", "figures")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

erc_vars <- paste0("ERC0", 1:8)
msc_vars <- c(paste0("MSC0", 1:9), "MSC10")
sj_vars  <- paste0("SJ0", 1:9)
erm_vars <- c(erc_vars, msc_vars, sj_vars)

meta_vars <- c("response_id", "participant_id", "timepoint", "dilemma_id", "response_text")

# 1. Import coded dataset -------------------------------------------------

if (!file.exists(coded_path)) {
  stop(
    "No coded dataset found at ", coded_path, ".\n",
    "Code your responses following Section 5 of the ERM manual, then pass the path as the ",
    "first argument (Rscript scripts/erm_analysis.R your_data.csv), using the same columns ",
    "as data/erm_example_data_v1.1.csv."
  )
}

erm_data <- read_csv(coded_path, show_col_types = FALSE)

missing_meta <- setdiff(meta_vars, names(erm_data))
missing_vars <- setdiff(erm_vars, names(erm_data))
if (length(missing_meta) > 0) stop("Missing required metadata columns: ", paste(missing_meta, collapse = ", "))
if (length(missing_vars) > 0) stop("Missing ERM variable columns: ", paste(missing_vars, collapse = ", "))

# Order timepoint chronologically (Pre < Post < Follow-up/Continuo), not
# alphabetically -- plain character/factor sorting would otherwise put
# "Continuo" before "Post" before "Pre" and silently invert every trend line.
# Unrecognized labels are appended afterwards in first-appearance order.
canonical_timepoints <- c("pretest", "pre", "baseline", "t1",
                          "posttest", "post", "t2",
                          "follow-up", "followup", "seguimiento", "continuo", "continuous", "t3")
timepoint_levels <- unique(erm_data$timepoint)
rank_timepoint <- match(tolower(timepoint_levels), canonical_timepoints)
rank_timepoint[is.na(rank_timepoint)] <- length(canonical_timepoints) + seq_len(sum(is.na(rank_timepoint)))
timepoint_levels <- timepoint_levels[order(rank_timepoint)]
erm_data <- erm_data %>% mutate(timepoint = factor(timepoint, levels = timepoint_levels))

# 2. Validate binary coding ------------------------------------------------

binary_check <- erm_data %>%
  summarise(across(all_of(erm_vars), ~ all(.x %in% c(0, 1, NA)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "is_binary")

non_binary <- binary_check %>% filter(!is_binary)
if (nrow(non_binary) > 0) {
  warning(
    "Non-binary values found in: ", paste(non_binary$variable, collapse = ", "),
    ". Review these columns before proceeding (Section 8: coding is 0/1 only)."
  )
}

na_report <- erm_data %>%
  summarise(across(all_of(erm_vars), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  filter(n_missing > 0)

if (nrow(na_report) > 0) {
  message("Missing (NA) values detected in some ERM variables:")
  print(na_report)
}

# 3. Dimension indices ------------------------------------------------------

erm_data <- erm_data %>%
  mutate(
    ERC_index = rowSums(across(all_of(erc_vars)), na.rm = TRUE),
    MSC_index = rowSums(across(all_of(msc_vars)), na.rm = TRUE),
    SJ_index  = rowSums(across(all_of(sj_vars)),  na.rm = TRUE)
  )

write_csv(erm_data, here(tables_dir, "erm_data_with_indices.csv"))

# 4. Variable-level frequencies ----------------------------------------------
# Wilson confidence intervals, per Section 13: proportions are binary-code
# rates, not continuous measurements.

freq_overall <- map_dfr(erm_vars, function(v) {
  x <- erm_data[[v]]
  x <- x[!is.na(x)]
  n <- length(x)
  k <- sum(x)
  ci <- binom.confint(k, n, conf.level = 0.95, methods = "wilson")
  tibble(variable = v, n = n, proportion_present = k / n, ci_lower = ci$lower, ci_upper = ci$upper)
}) %>%
  left_join(
    read_csv(here("codebook", "ERM_variable_dictionary_v1.1.csv"), show_col_types = FALSE) %>%
      rename(variable_label = variable),
    by = c("variable" = "id")
  )

write_csv(freq_overall, here(tables_dir, "frequencies_overall.csv"))

freq_by_timepoint <- erm_data %>%
  group_by(timepoint) %>%
  summarise(across(all_of(erm_vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(-timepoint, names_to = "variable", values_to = "proportion_present")

write_csv(freq_by_timepoint, here(tables_dir, "frequencies_by_timepoint.csv"))

freq_by_dilemma <- erm_data %>%
  group_by(dilemma_id) %>%
  summarise(across(all_of(erm_vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(-dilemma_id, names_to = "variable", values_to = "proportion_present")

write_csv(freq_by_dilemma, here(tables_dir, "frequencies_by_dilemma.csv"))

# 5. Co-occurrence patterns --------------------------------------------------

cooccurrence <- erm_data %>%
  select(all_of(erm_vars)) %>%
  as.matrix() %>%
  crossprod() %>%
  as.data.frame() %>%
  rownames_to_column("variable_1") %>%
  pivot_longer(-variable_1, names_to = "variable_2", values_to = "n_cooccurring") %>%
  filter(variable_1 != variable_2)

write_csv(cooccurrence, here(tables_dir, "cooccurrence_counts.csv"))

# Pairwise significance, FDR-corrected across all 351 comparisons (Section 13)
var_pairs <- combn(erm_vars, 2, simplify = FALSE)

cooccurrence_tests <- map_dfr(var_pairs, function(pair) {
  tab <- table(erm_data[[pair[1]]], erm_data[[pair[2]]])
  p_value <- if (all(dim(tab) == 2)) {
    tryCatch(fisher.test(tab)$p.value, error = function(e) NA_real_)
  } else {
    NA_real_
  }
  tibble(variable_1 = pair[1], variable_2 = pair[2], p_value = p_value)
}) %>%
  mutate(p_adjusted = p.adjust(p_value, method = "BH"))

write_csv(cooccurrence_tests, here(tables_dir, "cooccurrence_significance.csv"))

# 6. Participant profiles ----------------------------------------------------

participant_profiles <- erm_data %>%
  group_by(participant_id) %>%
  summarise(
    n_responses = n(),
    mean_ERC_index = mean(ERC_index, na.rm = TRUE),
    mean_MSC_index = mean(MSC_index, na.rm = TRUE),
    mean_SJ_index  = mean(SJ_index,  na.rm = TRUE),
    across(all_of(erm_vars), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

write_csv(participant_profiles, here(tables_dir, "participant_profiles.csv"))

# 7. Longitudinal comparisons -------------------------------------------------
# McNemar's test (binary variables) and Wilcoxon signed-rank (dimension
# indices) for the two-timepoint case (Section 13). Pairs are matched on
# participant_id x dilemma_id; rows without a match at both timepoints are
# dropped. For >2 timepoints, use a mixed-effects model instead (Section 13).

timepoints <- sort(unique(erm_data$timepoint))

if (length(timepoints) == 2) {
  t1 <- timepoints[1]
  t2 <- timepoints[2]

  paired <- erm_data %>%
    select(participant_id, dilemma_id, timepoint, all_of(erm_vars),
           ERC_index, MSC_index, SJ_index) %>%
    pivot_wider(
      id_cols = c(participant_id, dilemma_id),
      names_from = timepoint,
      values_from = c(all_of(erm_vars), ERC_index, MSC_index, SJ_index)
    ) %>%
    drop_na()

  mcnemar_results <- map_dfr(erm_vars, function(v) {
    col1 <- paired[[paste0(v, "_", t1)]]
    col2 <- paired[[paste0(v, "_", t2)]]
    tab <- table(factor(col1, levels = c(0, 1)), factor(col2, levels = c(0, 1)))
    result <- tryCatch(mcnemar.test(tab, correct = TRUE), error = function(e) NULL)
    if (is.null(result)) return(NULL)
    tibble(variable = v, n_pairs = nrow(paired), statistic = unname(result$statistic),
           p_value = result$p.value)
  })
  write_csv(mcnemar_results, here(tables_dir, "mcnemar_longitudinal.csv"))

  wilcoxon_results <- map_dfr(c("ERC_index", "MSC_index", "SJ_index"), function(idx) {
    x <- paired[[paste0(idx, "_", t1)]]
    y <- paired[[paste0(idx, "_", t2)]]
    result <- tryCatch(wilcox.test(x, y, paired = TRUE), error = function(e) NULL)
    if (is.null(result)) return(NULL)
    tibble(dimension_index = idx, n_pairs = nrow(paired), statistic = unname(result$statistic),
           p_value = result$p.value)
  })
  write_csv(wilcoxon_results, here(tables_dir, "wilcoxon_longitudinal.csv"))
} else {
  message(
    "Longitudinal tests (McNemar / Wilcoxon) skipped: exactly two timepoints are required, ",
    "found ", length(timepoints), "."
  )
}

# 8. Inter-rater reliability ---------------------------------------------------
# Cohen's kappa for two coders (Cohen, 1960); Krippendorff's alpha for more
# than two coders, detected automatically from the coder<N>_<VAR> columns
# present in the pilot file (Section 10).

if (file.exists(pilot_path)) {
  pilot_data <- read_csv(pilot_path, show_col_types = FALSE)

  compute_reliability <- function(var, data) {
    coder_cols <- grep(paste0("^coder[0-9]+_", var, "$"), names(data), value = TRUE)
    if (length(coder_cols) < 2) return(NULL)
    ratings <- data %>% select(all_of(coder_cols)) %>% drop_na()
    if (nrow(ratings) < 2) return(NULL)

    if (length(coder_cols) == 2) {
      result <- tryCatch(kappa2(ratings, weight = "unweighted"), error = function(e) NULL)
      if (is.null(result)) return(NULL)
      tibble(variable = var, method = "cohen_kappa", n_coders = 2L, n = nrow(ratings),
             estimate = result$value, p_value = result$p.value)
    } else {
      result <- tryCatch(kripp.alpha(t(as.matrix(ratings)), method = "nominal"), error = function(e) NULL)
      if (is.null(result)) return(NULL)
      tibble(variable = var, method = "krippendorff_alpha", n_coders = length(coder_cols),
             n = nrow(ratings), estimate = result$value, p_value = NA_real_)
    }
  }

  irr_results <- map_dfr(erm_vars, compute_reliability, data = pilot_data) %>%
    mutate(meets_threshold = estimate >= 0.70)

  write_csv(irr_results, here(tables_dir, "interrater_reliability.csv"))

  below_threshold <- irr_results %>% filter(!meets_threshold)
  if (nrow(below_threshold) > 0) {
    message(
      "Variables below the kappa/alpha >= .70 threshold (Section 10): ",
      paste(below_threshold$variable, collapse = ", ")
    )
  }
} else {
  message(
    "No pilot double-coded file found at ", pilot_path,
    " — skipping inter-rater reliability. Pass one as the second argument, using ",
    "coder1_<VAR>/coder2_<VAR> (or coder3_<VAR>, ...) columns per Section 10 of the manual."
  )
}

# 9. Visualization --------------------------------------------------------

index_by_timepoint <- erm_data %>%
  group_by(timepoint) %>%
  summarise(
    ERC = mean(ERC_index, na.rm = TRUE),
    MSC = mean(MSC_index, na.rm = TRUE),
    SJ  = mean(SJ_index,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-timepoint, names_to = "dimension", values_to = "mean_index")

# Palette shared with the manual (teal / terracotta / ochre) so figures and
# the manual read as one visual system.
erm_palette <- c(ERC = "#1F6F5C", MSC = "#C1662E", SJ = "#9C7A1E")

erm_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", color = "#262626", size = rel(1.15)),
    axis.title = element_text(color = "#404040"),
    axis.text = element_text(color = "#595959"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#E5E5E5"),
    legend.title = element_text(face = "bold", color = "#404040"),
    legend.position = "right"
  )

p_indices <- ggplot(index_by_timepoint, aes(x = timepoint, y = mean_index, color = dimension, group = dimension)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.6) +
  scale_color_manual(values = erm_palette) +
  labs(title = "ERM Dimension Indices by Timepoint", x = "Timepoint", y = "Mean index", color = "Dimension") +
  erm_theme

ggsave(here(figures_dir, "dimension_indices_by_timepoint.png"), p_indices, width = 7, height = 5, dpi = 300)

p_freq <- freq_overall %>%
  ggplot(aes(x = reorder(variable, proportion_present), y = proportion_present, fill = dimension)) +
  geom_col(width = 0.75) +
  scale_fill_manual(values = erm_palette) +
  coord_flip() +
  labs(title = "ERM Variable Frequencies", x = NULL, y = "Proportion of responses coded present",
       fill = "Dimension") +
  erm_theme

ggsave(here(figures_dir, "variable_frequencies.png"), p_freq, width = 8, height = 9, dpi = 300)

message("ERM analysis complete. Tables written to ", tables_dir, "; figures written to ", figures_dir, ".")
