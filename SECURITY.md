# Security Policy

This repository contains a research coding manual's companion materials: an
R analysis script, a variable dictionary, and a synthetic example dataset.
It has no server component, no authentication, and does not process
real participant data — so "security" here mostly means **data integrity and
research-data privacy**, not the usual software attack surface. That said,
a few things are worth reporting if you find them.

## What to report here

- **Accidentally committed real/identifiable data.** If you find anything in
  this repository's history that looks like real participant responses
  rather than the synthetic example dataset (`data/erm_example_data_v1.1.csv`,
  which is explicitly synthetic — see its `response_text` column), please
  report it immediately and privately (see below). We will remove it and
  purge it from history as fast as GitHub allows.
- **A dependency-related vulnerability** in the R packages the script
  installs (`tidyverse`, `here`, `irr`, `janitor`, `binom`) that materially
  affects a user running `scripts/erm_analysis.R` on their own machine.
- **Citation or integrity issues**: e.g., a fabricated or materially
  misattributed reference, or a DOI/version mismatch between this repository
  and the archived manual on Zenodo.

## What NOT to report here

General bugs in `scripts/erm_analysis.R` that don't involve data exposure or
integrity (wrong output, a crash on malformed input, etc.) — please use a
regular [GitHub issue](../../issues) instead; there's no need for a private
report.

## How to report

Email **steresadiaz@gmail.com** with a description of the issue. Please do
not open a public issue for data-privacy reports (real/identifiable data
found in the repo) — email us so we can remove it before it's more widely
visible.

We aim to acknowledge reports within a few days. Given this is a small
academic project maintained by its authors rather than an organization,
please be patient with response times.

## Scope

This policy covers this repository only. It does not cover the manual's
content or methodology (see `README.md` for how to propose changes to those)
or any third-party service (GitHub, Zenodo) this repository depends on —
report issues with those platforms directly to them.
