# Changelog — SpeciesRichness

## Module 1 · Bootstrap reproducible R project
Date: 2025-08-23

Summary (for orchestrator)
- Established a clean, OS‑agnostic R project skeleton with a pinned environment using `renv`.
- Added a robust bootstrap script and Makefile to reproduce the environment deterministically.
- Verified successful bootstrap run on user machine (`make init`) with `renv::status()` reporting no issues.

What was added
- Project structure: `R/`, `scripts/`, `tests/testthat/`, `data-raw/`, `inst/extdata/`, `.github/`.
- Hygiene files:
  - `.gitignore` (R/RStudio, renv, targets, data, build artifacts).
  - `.Rprofile` with CRAN mirror, renv cache, renv activation, and `RENV_CONFIG_PAK_ENABLED=TRUE`.
- Bootstrap: `scripts/00_bootstrap.R`
  - Idempotent `renv` initialization/activation.
  - Installs baseline toolchain via `renv::install()` and snapshots lockfile.
  - Safe `testthat` harness creation via `usethis::use_testthat(edition = 3)` with fallback.
- Makefile: `init`, `lock`, `clean`
  - Uses `Rscript --vanilla` with fallback to `R --vanilla -q` for environments where `Rscript` is problematic.

Baseline toolchain (pinned via renv)
- `pak`, `targets`, `tarchetypes`, `testthat`, `devtools`, `usethis`,
  `readr`, `data.table`, `jsonlite`, `httr`, `curl`, `here`, `cli`, `glue`.

State after user bootstrap run
- `make init` ran successfully on the user side.
- `renv.lock` exists and is tracked in Git.
- `renv/activate.R` present; `renv::status()` reports project is in a consistent state.
- `tests/testthat.R` present (minimal harness).
- Repository status is clean.

Notes
- Makefile and bootstrap are idempotent and safe to re‑run.
- `pak` is enabled via `RENV_CONFIG_PAK_ENABLED`, so renv prefers pak for installs when available.

Next
- Ready for Module 2 instructions from the orchestrator.

