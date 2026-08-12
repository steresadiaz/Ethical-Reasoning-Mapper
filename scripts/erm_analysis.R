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

# Shared visual style for every figure in this script -- a blue/purple
# family, used both categorically (ERC/MSC/SJ) and as a diverging scale
# (heatmap). Keeping it in one place means every plot restyles together.
erm_palette <- c(ERC = "#1F5C8B", MSC = "#7B4FA6", SJ = "#4FA3C4")
erm_diverging <- list(low = "#7B4FA6", mid = "#F7F5FB", high = "#1F5C8B")

erm_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", color = "#262626", size = rel(1.15)),
    plot.subtitle = element_text(color = "#595959", size = rel(0.85)),
    axis.title = element_text(color = "#404040"),
    axis.text = element_text(color = "#595959"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#E5E5E5"),
    legend.title = element_text(face = "bold", color = "#404040"),
    legend.position = "right"
  )

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

# Order timepoint chronologically, not alphabetically -- plain character/
# factor sorting would otherwise put e.g. "Continuo" before "Post" before
# "Pre" and silently invert every trend line. The default below assumes the
# common convention Pre < Post < Follow-up/Continuo (a wave collected AFTER
# the post-assessment). If your "Continuo"/"Follow-up" wave instead falls
# BETWEEN pre and post (e.g. ongoing coursework collected during the term),
# edit canonical_timepoints accordingly -- and check the message this prints
# every run against your actual study design; don't assume it's right.
# Unrecognized labels are appended afterwards in first-appearance order.
canonical_timepoints <- c("pretest", "pre", "baseline", "t1",
                          "posttest", "post", "t2",
                          "follow-up", "followup", "seguimiento", "continuo", "continuous", "t3")
timepoint_levels <- unique(erm_data$timepoint)
rank_timepoint <- match(tolower(timepoint_levels), canonical_timepoints)
rank_timepoint[is.na(rank_timepoint)] <- length(canonical_timepoints) + seq_len(sum(is.na(rank_timepoint)))
timepoint_levels <- timepoint_levels[order(rank_timepoint)]
erm_data <- erm_data %>% mutate(timepoint = factor(timepoint, levels = timepoint_levels))
message(
  "Timepoint order (chronological, as inferred): ",
  paste(timepoint_levels, collapse = " -> "),
  ". If this is wrong for your study design, edit canonical_timepoints in this script."
)

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
    SJ_index  = rowSums(across(all_of(sj_vars)),  na.rm = TRUE),
    # Supplementary composite (0-27) = ERC_index + MSC_index + SJ_index.
    # ERC/MSC/SJ capture conceptually different things (reasoning components,
    # morally salient considerations, sources of justification) and should
    # normally be reported and analyzed separately, not collapsed into one
    # number. This sum is provided for readers who want a single overall
    # count, but per Section 10's Validation Status (no expert-panel, pilot,
    # or inter-rater validation yet), report it as a descriptive "density of
    # ERM components identified" -- NOT as a validated "argumentative
    # complexity" construct.
    erm_density_index = ERC_index + MSC_index + SJ_index
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

# Dimension indices by dilemma. If data/dilemmas.csv exists (columns:
# dilemma_id, phase, question) it's joined in for readable labels; this
# file is study-specific and optional, so its absence only drops the labels,
# not the analysis.
dilemma_dict_path <- here("data", "dilemmas.csv")

index_by_dilemma <- erm_data %>%
  group_by(dilemma_id) %>%
  summarise(
    n = n(),
    ERC = mean(ERC_index, na.rm = TRUE),
    MSC = mean(MSC_index, na.rm = TRUE),
    SJ  = mean(SJ_index,  na.rm = TRUE),
    .groups = "drop"
  )

if (file.exists(dilemma_dict_path)) {
  dilemma_dict <- read_csv(dilemma_dict_path, show_col_types = FALSE)
  index_by_dilemma <- index_by_dilemma %>%
    left_join(dilemma_dict, by = "dilemma_id") %>%
    relocate(phase, question, .after = dilemma_id)
} else {
  message(
    "No dilemma dictionary found at ", dilemma_dict_path,
    " -- dilemma-by-dilemma outputs will use bare IDs only. Add a ",
    "data/dilemmas.csv (columns: dilemma_id, phase, question) for readable labels."
  )
}

write_csv(index_by_dilemma, here(tables_dir, "indices_by_dilemma.csv"))

p_dilemma <- index_by_dilemma %>%
  pivot_longer(c(ERC, MSC, SJ), names_to = "dimension", values_to = "mean_index") %>%
  mutate(dilemma_id = factor(dilemma_id, levels = rev(sort(unique(dilemma_id))))) %>%
  ggplot(aes(x = dilemma_id, y = mean_index, fill = dimension)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = erm_palette) +
  coord_flip() +
  labs(title = "ERM Dimension Indices by Dilemma", x = NULL, y = "Mean index", fill = "Dimension") +
  erm_theme

ggsave(here(figures_dir, "dimension_indices_by_dilemma.png"), p_dilemma, width = 8, height = 8, dpi = 300)

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

# Co-occurrence heatmap: phi coefficient (= Pearson correlation for two
# binary variables) for every pair, ordered by dimension, with
# FDR-significant pairs marked. Variables with zero variance (never coded
# present) have an undefined correlation -- shown as blank cells, not a
# spurious value.
phi_mat <- suppressWarnings(cor(erm_data[erm_vars]))
diag(phi_mat) <- NA

phi_long <- as.data.frame(phi_mat) %>%
  rownames_to_column("variable_1") %>%
  pivot_longer(-variable_1, names_to = "variable_2", values_to = "phi")

sig_filtered <- cooccurrence_tests %>% filter(p_adjusted < 0.05) %>% select(variable_1, variable_2)
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

block_breaks <- c(length(erc_vars), length(erc_vars) + length(msc_vars)) + 0.5

p_heatmap <- ggplot(phi_long, aes(x = variable_1, y = variable_2, fill = phi)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(
    data = filter(phi_long, significant, variable_1 != as.character(variable_2)),
    label = "*", color = "white", size = 4, fontface = "bold", vjust = 0.75
  ) +
  scale_fill_gradient2(
    low = erm_diverging$low, mid = erm_diverging$mid, high = erm_diverging$high, midpoint = 0,
    na.value = "#EFEFEF", limits = c(-0.55, 0.55),
    name = "Coeficiente\nphi"
  ) +
  geom_vline(xintercept = block_breaks, color = "#8C8C8C", linewidth = 0.5) +
  geom_hline(yintercept = length(erm_vars) - block_breaks + 1, color = "#8C8C8C", linewidth = 0.5) +
  coord_fixed() +
  labs(
    title = "Co-ocurrencia entre variables ERM (coeficiente phi)",
    subtitle = "Ordenado por dimensión (ERC | MSC | SJ). * = significativo tras corrección FDR (p < .05)",
    x = NULL, y = NULL
  ) +
  erm_theme +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = rel(0.75)),
    axis.text.y = element_text(size = rel(0.75)),
    panel.grid = element_blank()
  )

ggsave(here(figures_dir, "cooccurrence_heatmap.png"), p_heatmap, width = 10, height = 9, dpi = 300)

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
# indices), matched on participant_id x dilemma_id; rows without a match at
# both timepoints are dropped. For >2 timepoints, a mixed-effects model is
# recommended instead (Section 13) -- but if the corpus has more than two
# timepoints AND includes "Pre" and "Post" specifically, this step still
# runs that one comparison in addition, since it's the most common request.

dimension_indices <- c("ERC_index", "MSC_index", "SJ_index", "erm_density_index")

# Matched-pairs rank-biserial correlation, the standard effect size for a
# Wilcoxon signed-rank test: r = (V+ - V-) / (V+ + V-), computed from the
# signed ranks of the non-zero differences (ties/zeroes dropped, matching
# what wilcox.test() itself excludes).
rank_biserial <- function(x, y) {
  d <- y - x
  d <- d[d != 0]
  if (length(d) == 0) return(NA_real_)
  r <- rank(abs(d))
  v_pos <- sum(r[d > 0])
  v_total <- sum(r)
  (2 * v_pos - v_total) / v_total
}

run_paired_tests <- function(data, t1, t2, label) {
  # --- Response-level pairing (participant_id x dilemma_id) -----------------
  # EXPLORATORY ONLY: a participant who answered N dilemmas contributes N
  # pairs here, so these are not independent observations -- do not use this
  # as the confirmatory test (see the participant-level block below, which
  # is). Kept because it's useful for per-variable McNemar tests, where the
  # unit of interest genuinely is the response, not the participant.
  paired <- data %>%
    filter(timepoint %in% c(t1, t2)) %>%
    select(participant_id, dilemma_id, timepoint, all_of(erm_vars),
           all_of(dimension_indices)) %>%
    pivot_wider(
      id_cols = c(participant_id, dilemma_id),
      names_from = timepoint,
      values_from = c(all_of(erm_vars), all_of(dimension_indices))
    ) %>%
    drop_na()

  mcnemar_results <- map_dfr(erm_vars, function(v) {
    col1 <- paired[[paste0(v, "_", t1)]]
    col2 <- paired[[paste0(v, "_", t2)]]
    tab <- table(factor(col1, levels = c(0, 1)), factor(col2, levels = c(0, 1)))
    result <- tryCatch(mcnemar.test(tab, correct = TRUE), error = function(e) NULL)
    if (is.null(result)) return(NULL)
    tibble(
      variable = v, n_pairs = nrow(paired),
      n_present_t1 = sum(col1), n_present_t2 = sum(col2),
      statistic = unname(result$statistic), p_value = result$p.value
    )
  }) %>%
    mutate(p_adjusted = p.adjust(p_value, method = "BH"))

  wilcoxon_by_response <- map_dfr(dimension_indices, function(idx) {
    x <- paired[[paste0(idx, "_", t1)]]
    y <- paired[[paste0(idx, "_", t2)]]
    result <- tryCatch(wilcox.test(x, y, paired = TRUE), error = function(e) NULL)
    if (is.null(result)) return(NULL)
    tibble(
      dimension_index = idx, n_pairs = nrow(paired),
      mean_t1 = mean(x), mean_t2 = mean(y), mean_diff = mean(y - x),
      statistic = unname(result$statistic), p_value = result$p.value,
      effect_size_r = rank_biserial(x, y)
    )
  })

  # --- Participant-level pairing (CONFIRMATORY) ------------------------------
  # Each participant's responses are averaged within timepoint first, so
  # every participant contributes exactly one Pre value and one Post value --
  # this is what "n = <number of participants>" in the manuscript should
  # refer to, and is the analysis that should be reported as confirmatory.
  participant_level <- data %>%
    filter(timepoint %in% c(t1, t2)) %>%
    group_by(participant_id, timepoint) %>%
    summarise(across(all_of(dimension_indices), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    pivot_wider(id_cols = participant_id, names_from = timepoint, values_from = all_of(dimension_indices)) %>%
    drop_na()

  wilcoxon_by_participant <- map_dfr(dimension_indices, function(idx) {
    x <- participant_level[[paste0(idx, "_", t1)]]
    y <- participant_level[[paste0(idx, "_", t2)]]
    result <- tryCatch(wilcox.test(x, y, paired = TRUE), error = function(e) NULL)
    if (is.null(result)) return(NULL)
    tibble(
      dimension_index = idx, n_participants = nrow(participant_level),
      mean_t1 = mean(x), mean_t2 = mean(y), mean_diff = mean(y - x),
      n_increased = sum(y > x), n_decreased = sum(y < x), n_unchanged = sum(y == x),
      statistic = unname(result$statistic), p_value = result$p.value,
      effect_size_r = rank_biserial(x, y)
    )
  })

  write_csv(mcnemar_results, here(tables_dir, paste0("mcnemar_", label, ".csv")))
  write_csv(wilcoxon_by_response, here(tables_dir, paste0("wilcoxon_", label, "_by_response.csv")))
  write_csv(wilcoxon_by_participant, here(tables_dir, paste0("wilcoxon_", label, "_by_participant.csv")))

  index_by_tp <- data %>%
    filter(timepoint %in% c(t1, t2)) %>%
    group_by(timepoint) %>%
    summarise(ERC = mean(ERC_index), MSC = mean(MSC_index), SJ = mean(SJ_index), .groups = "drop") %>%
    pivot_longer(-timepoint, names_to = "dimension", values_to = "mean_index")

  p <- ggplot(index_by_tp, aes(x = timepoint, y = mean_index, color = dimension, group = dimension)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2.6) +
    scale_color_manual(values = erm_palette) +
    labs(title = paste0("ERM Dimension Indices: ", t1, " vs. ", t2),
         x = "Timepoint", y = "Mean index", color = "Dimension") +
    erm_theme
  ggsave(here(figures_dir, paste0("dimension_indices_", label, ".png")), p, width = 6, height = 5, dpi = 300)

  list(paired = paired, mcnemar = mcnemar_results,
       wilcoxon_by_response = wilcoxon_by_response,
       wilcoxon_by_participant = wilcoxon_by_participant)
}

timepoints_present <- levels(droplevels(erm_data$timepoint))

if (length(timepoints_present) == 2) {
  invisible(run_paired_tests(erm_data, timepoints_present[1], timepoints_present[2], "longitudinal"))
} else {
  message(
    "Longitudinal tests on the full timepoint set skipped: exactly two timepoints are ",
    "required, found ", length(timepoints_present), " (", paste(timepoints_present, collapse = ", "), ")."
  )
}

if (all(c("Pre", "Post") %in% timepoints_present) && length(timepoints_present) != 2) {
  message(
    "Corpus has ", length(timepoints_present), " timepoints; running an additional ",
    "Pre vs. Post-only comparison..."
  )
  pp <- run_paired_tests(erm_data, "Pre", "Post", "pre_vs_post")
  n_sig <- sum(pp$mcnemar$p_adjusted < 0.05, na.rm = TRUE)
  message(
    "McNemar (Pre vs Post, response-level, exploratory): ", n_sig, " of ", nrow(pp$mcnemar),
    " variables significant after FDR correction (p_adjusted < .05)."
  )
  message(
    "Wilcoxon (Pre vs Post, participant-level, CONFIRMATORY, n = ",
    pp$wilcoxon_by_participant$n_participants[1], " participants):"
  )
  print(pp$wilcoxon_by_participant %>% select(dimension_index, mean_t1, mean_t2, p_value, effect_size_r))
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

p_indices <- ggplot(index_by_timepoint, aes(x = timepoint, y = mean_index, color = dimension, group = dimension)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.6) +
  scale_color_manual(values = erm_palette) +
  labs(
    title = "ERM Dimension Indices by Timepoint",
    subtitle = "Descriptive overview only. Pre and Post share the same dilemma set (D01–D10) and are the\nvalid paired comparison (see *_pre_vs_post outputs); Continuo uses a different, more complex\ndilemma set (D11–D20) and is not a matched timepoint -- see indices_by_dilemma for that breakdown.",
    x = "Timepoint", y = "Mean index", color = "Dimension"
  ) +
  erm_theme

ggsave(here(figures_dir, "dimension_indices_by_timepoint.png"), p_indices, width = 8.5, height = 5.5, dpi = 300)

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
