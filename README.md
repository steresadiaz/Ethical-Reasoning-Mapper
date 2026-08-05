# Ethical Reasoning Mapper (ERM) — Companion Materials

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.21808174.svg)](https://doi.org/10.5281/zenodo.21808174) &nbsp;
[![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/) &nbsp;
[![CI](https://github.com/steresadiaz/Ethical-Reasoning-Mapper/actions/workflows/ci.yml/badge.svg)](https://github.com/steresadiaz/Ethical-Reasoning-Mapper/actions/workflows/ci.yml)

This repository contains the reproducible companion materials for the
**Ethical Reasoning Mapper (ERM)**, a standardized coding framework for the
empirical analysis of ethical reasoning expressed in open-ended responses to
ethical dilemmas.

The manual itself — the stable, citable reference — is archived on Zenodo
with its own DOI. **This repository is versioned in lockstep with the
manual**: the `v1.1` release corresponds exactly to manual version 1.1.

- **Manual (Zenodo, stable PDF/DOCX + DOI):** https://doi.org/10.5281/zenodo.21808174
- **This repository:** https://github.com/steresadiaz/Ethical-Reasoning-Mapper
- **Version 1.1 release (matches the manual):** https://github.com/steresadiaz/Ethical-Reasoning-Mapper/releases/tag/v1.1

> For reproducibility, cite and download the **`v1.1` release**, not the
> default branch. Materials on the default branch may move ahead of what a
> given manual version describes.

## What's here

- `codebook/ERM_variable_dictionary_v1.1.csv` / `.xlsx` — the complete
  27-variable dictionary (8 Ethical Reasoning Components, 10 Morally
  Salient Considerations, 9 Sources of Justification) in machine-readable
  form, matching Section 8 / Appendix A of the manual.
- `data/erm_example_data_v1.1.csv` — a synthetic example dataset (16
  responses, all 27 variables) for testing the pipeline and learning the
  expected data structure (Section 5). **Synthetic, not real participant
  data.**
- `scripts/erm_analysis.R` — the production-ready R analysis pipeline
  (Section 14 / Appendix E): import, validation, dimension indices,
  variable-level frequencies with confidence intervals, co-occurrence
  patterns (FDR-corrected significance), participant profiles, longitudinal
  comparisons (McNemar / Wilcoxon), inter-rater reliability (Cohen's kappa /
  Krippendorff's alpha), and figures.
- `docs/Ethical_Reasoning_Mapper_v1.1.pdf` — a copy of the manual matching
  this release, for convenience; the archival copy of record is on Zenodo.
- `CITATION.cff` — machine-readable citation metadata (GitHub's "Cite this
  repository" widget reads this file).
- `CHANGELOG.md` — version history for the manual and these materials.
- `CONTRIBUTING.md` — how to report a coding ambiguity, propose a codebook
  change, share pilot/IRR results, or fix the R script.
- `CODE_OF_CONDUCT.md` — Contributor Covenant v2.1.
- `SECURITY.md` — how to report data-privacy or citation-integrity issues.
- `.github/` — issue templates, PR template, and a CI workflow that runs
  `scripts/erm_analysis.R` against the example dataset on every push/PR.

## Quick start

```r
install.packages(c("tidyverse", "here", "irr", "janitor", "binom"))
```

```bash
# Runs on the bundled synthetic example dataset by default
Rscript scripts/erm_analysis.R

# Or on your own coded data:
Rscript scripts/erm_analysis.R path/to/your_coded_data.csv path/to/pilot_double_coded.csv
```

Outputs are written to `outputs/tables/*.csv` and `outputs/figures/*.png`
(created automatically; not tracked in this repository).

Your coded data must follow the column layout in
`data/erm_example_data_v1.1.csv`: metadata columns
(`response_id`, `participant_id`, `timepoint`, `dilemma_id`, `response_text`)
followed by the 27 binary (0/1) ERM variables. See Section 5 and Section 9
of the manual for the full coding specification and rules.

## How to cite

This work has two citable objects, related on Zenodo as
*"is supplemented by"* (manual → software) and *"is supplement to"*
(software → manual):

1. **The manual** (Technical note) — cite this for the coding framework and
   methodology.
2. **This repository / release** (Software) — cite this if you reuse or
   adapt the code, dictionary, or example dataset.

See `CITATION.cff` for the machine-readable record, and the manual's own
title page for its suggested citation.

## Contributing

Bug reports, codebook feedback, and reliability results are welcome — see
`CONTRIBUTING.md` for how, and `CODE_OF_CONDUCT.md` for community standards.
For data-privacy or citation-integrity reports, see `SECURITY.md` instead of
opening a public issue.

## License

All materials in this repository (code, data, and documentation) are
licensed under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).
See `LICENSE`.

## Authors

- Sofía Teresa Díaz Torres — Universidad Nacional Autónoma de México (UNAM)
- Alma Delia Torres-Rivera — Instituto Politécnico Nacional (IPN)
