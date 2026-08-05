# Contributing to the Ethical Reasoning Mapper (ERM)

Thanks for your interest in the ERM. This repository holds the *companion
materials* for the manual (R script, variable dictionary, example dataset) —
the manual's methodology itself is versioned separately and archived on
Zenodo (see `README.md`). This file covers how to contribute to what's here.

## Ways to contribute

### 1. Report a coding ambiguity or propose a codebook change
The manual is explicit that it is proposed pending empirical validation
(Section 10, "Validation Status of This Version"). If you hit a case the
codebook, decision trees (Appendix B), or Rule 7 don't resolve:

- Open an issue using the **Codebook question** template.
- If you've coded a batch of responses and repeatedly used SJ09 ("Other
  Explicit Source") for the same kind of source, that's exactly the signal
  Rule 9 (Section 9) asks for — tell us, with examples, so it can be
  considered for a new SJ variable in a future revision.

### 2. Share pilot or inter-rater reliability results
If you run a pilot on your own corpus (Section 10), we'd like to hear about
it, positive or negative — this is how the framework moves from "proposed"
to "validated." Open an issue with your κ/α results per variable (no raw
response text needed) using the **Codebook question** template.

### 3. Fix or extend `scripts/erm_analysis.R`
Bug reports and pull requests are welcome. Please:
- Keep the tidyverse style already used in the script (pipes, `snake_case`).
- If you add a new analysis step, update `README.md`'s "What the script does"
  list and, where relevant, cite the statistical method the way the rest of
  the script does (see the comments citing Cohen 1960, McNemar 1947, etc.).
- Test against `data/erm_example_data_v1.1.csv` before opening a PR — CI runs
  the same check automatically (see `.github/workflows/ci.yml`).

### 4. Documentation and typos
Small fixes (README clarity, typos, broken links) are welcome directly as a
PR — no need to open an issue first.

## What NOT to contribute here

- Changes to the manual's methodology, coding rules, or variable definitions
  themselves are made by the authors in the manual (deposited on Zenodo), not
  through this repository. Use the **Codebook question** issue template to
  *propose* changes; the authors will incorporate accepted proposals into a
  future manual version and changelog entry (Appendix F).
- Real, identifiable participant response data. `data/` in this repository
  is for synthetic/example data only — see `.gitignore` and the Data Privacy
  note in `README.md`.

## Pull request process

1. Fork the repository and create a branch from `main`.
2. Make your change. For R script changes, run it locally against the
   example dataset to confirm it still completes without errors.
3. Open a PR against `main` using the PR template. Describe what changed and
   why; link any related issue.
4. CI must pass before merge. A maintainer will review and may ask for
   changes.
5. Versioned releases (e.g., `v1.2`) are cut by the authors once a set of
   accepted changes is ready, following the same process documented in
   Appendix F of the manual.

## Code of Conduct

This project follows the [Contributor Covenant](CODE_OF_CONDUCT.md). By
participating, you agree to abide by its terms.

## Questions

Open an issue, or contact the authors directly (see `README.md`).
