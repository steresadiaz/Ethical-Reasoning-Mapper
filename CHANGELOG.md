# Changelog

All notable changes to the Ethical Reasoning Mapper (ERM) manual and its
companion materials are documented here. See `docs/Ethical_Reasoning_Mapper_v1.1.pdf`,
Section 16 (Version History) and Appendix F (Changelog) for the full,
narrative version of this history.

## [1.1] — 2026-08-04

### Added
- Alma Delia Torres-Rivera as co-author (title page and suggested citation).
- **Companion Materials** section pointing to this repository and its versioned releases.
- `Validity Considerations` subsection (Section 10): content-validity procedure via expert panel review.
- Three new decision trees in Appendix B: ERC02 vs. ERC03; MSC02 vs. MSC04; MSC05 vs. SJ04.
- Rule 9 (Section 9): review and promotion procedure for SJ09 ("Other Explicit Source").
- Statistical specification in Section 13 / 14: Wilson confidence intervals for
  frequencies, McNemar's test and the Wilcoxon signed-rank test for
  longitudinal comparisons, false discovery rate correction (Benjamini &
  Hochberg, 1995) for pairwise co-occurrence tests, and guidance on
  mixed-effects models for clustered/repeated responses.
- `scripts/erm_analysis.R`: implements all of the above — Wilson CIs,
  McNemar/Wilcoxon longitudinal tests, FDR-corrected co-occurrence
  significance, and automatic Cohen's kappa (2 coders) / Krippendorff's
  alpha (>2 coders) selection for inter-rater reliability.
- `codebook/ERM_variable_dictionary_v1.1.{csv,xlsx}`: machine-readable variable dictionary.
- `data/erm_example_data_v1.1.csv`: synthetic example dataset (16 responses, all 27 variables) for demonstration.

### Changed
- Minimum pilot sample size specified (10–15% of the corpus, subject to a
  minimum of 30 coded responses), with methodological rationale (Bujang &
  Baharum, 2017; O'Connor & Joffe, 2020).
- Inter-rater reliability estimators specified precisely: unweighted Cohen's
  kappa for two coders (Cohen, 1960); Krippendorff's alpha, nominal level,
  for more than two coders or missing ratings (Krippendorff, 2018; Hayes &
  Krippendorff, 2007). The κ/α ≥ .70 threshold is now explicitly framed as a
  convention (Landis & Koch, 1977), not a fixed statistical criterion.
- Ongoing monitoring (Section 10): added a re-piloting trigger when a
  checkpoint reliability estimate falls below threshold.
- Expanded the References section with nine new, verified citations
  supporting the above additions.

## [1.0] — 2026-08

### Added
- Initial complete public release, prepared for open deposit.
- Sources of Justification variable table (SJ01–SJ09), previously referenced but unspecified.
- Sections 9–16 (Coding Rules, Quality Control, Worked Examples, Variable
  Dictionary, Data Outputs, R Workflow, File Structure, Version History).
- Appendices A–F (Complete Codebook, Decision Trees, Annotated Responses,
  Example Dataset, R Scripts, Changelog).
- References section and title-page metadata for open deposit.

### Changed
- Harmonized dimension labels (e.g., "Normative Considerations" relabeled
  "Morally Salient Considerations (MSC)") for consistency with Sections 3 and 7.
