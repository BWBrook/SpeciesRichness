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

## Module 2 · Package + ingest + targets
Date: 2025-08-23

Summary (for orchestrator)
- Converted repo into a minimal R package with `DESCRIPTION`, MIT license, and roxygen-ready sources.
- Implemented robust Ecological Register (Dryad) ingest utilities and a minimal `targets` pipeline producing required objects.
- Verified locally by user: `devtools::check()` green; `tar_make()` builds `counts_dt`, `per_inv_sn`, and writes `inst/extdata/co_sample_flat.csv`.

Core changes
- Package skeleton: `DESCRIPTION`, `LICENSE`/`LICENSE.md`, `.Rbuildignore` (ignores `_targets`, `docs`, `scripts`, `data-raw`, `renv`, etc.).
- Ingest utilities:
  - `fetch_ecoregister_zip()` now fetches the data `.gz` via Dryad `file_stream` id with retries, User-Agent, and gzip validation; supports local override via `ECOREG_DATA_PATH`.
  - `read_ecoregister_counts()` reads either a direct `.gz` or a `.zip` member; cleans and filters counts; returns long-form `data.table`.
  - `summarise_per_inventory()` computes per-sample S and N.
  - `build_co_sample_flat()` writes the flat CSV expected by legacy harness.
- Targets pipeline (`_targets.R`):
  - Uses `targets::tar_source("R")` to source package functions in dev without installation.
  - Targets: `dryad_zip` (path to data `.gz`), `counts_dt`, `per_inv_sn`, `co_sample_flat` (file).
- Tests: synthetic unit test for parsing using temporary gz/zip; `R.utils` in Suggests.
- data.table NSE globals declared; “unused Imports” silenced via internal helper.

Notes
- Quarto message during check is benign (version probe on Windows); can be ignored.
- Local data override works: set `ECOREG_DATA_PATH` to a valid `.gz` to avoid network.

State after user run
- `devtools::check()` passed (0 errors, 0 warnings, 0 notes after ignores).
- `tar_make()` succeeded with 4 targets completed; artifacts verified.

### Module 2 · Follow‑up tweaks
Date: 2025-08-23

- Cleaned taxon labels to avoid literal " NA" suffixes when species/subspecies are missing.
- Aggregated duplicate rows within `(sample_id, taxon)` to stabilize S/N and downstream outputs.
- Added targets: `distinct_count_classes` and `eligible_inventories` (>= 4 count classes).
- Enabled optional fast caching (`tar_option_set(format = "qs")` when `qs` is installed).
- Extended `.Rbuildignore` to ignore `_targets/`, `inst/extdata/co_sample_flat.csv`, and `data-raw/cache/`.

### Added
Date: 2025-08-24
- Root `CHANGELOG.md` (Keep a Changelog style).
- `docs/PIPELINE.md` with current DAG and usage notes.
- `docs/AGENT_NOTES.md` session log for agent edits.
- `docs/DEVELOPMENT.qmd` with local setup and workflows.
- `scripts/bootstrap.R` to restore/install and document/test.
- `metadata/data_manifest.csv` to track small example artefacts.
- Minimal CI scaffold at `.github/workflows/check.yml` (manual dispatch only).
- Vendor CEGS estimators: `cegs_ml()` and `cegs_ld()` with internal helpers.
- Unit tests for parity with `Alroy/` originals.
- Benchmark utilities: `topk_fingerprint()`, `nearest_neighbour()`, `tail_histogram()`, `tail_nll()`.
- Targets: `eligible_ids`, `topk_matrix`, `nearest_pair`, `cegs_scores`, and `cegs_summary`.
- `inst/CITATION` (moves citation metadata into standard location for R CMD check).
- `vignettes/cegs-benchmark.Rmd` vignette showing end-to-end benchmark usage.

### Changed
- `_targets.R`: removed `library()` and `tar_source()`, added `import::from()`/`import::here()`, and set deterministic `tar_option_set(seed = 1L)`.
- `_targets.R`: replaced `data.table:::.()` alias with `list()` to avoid hidden imports.
- R ingest helpers: switched `stopifnot()`/`stop()` to `rlang::abort()`; default output path now via `here::here()`.
- `.Rbuildignore` and `.gitignore` updated to include `.lintr`, `.quarto/`, and `docs/_site/`.
- `DESCRIPTION`: added `stats4` to Imports.
- `.Rbuildignore`: now ignores top-level `CITATION.cff` to silence non-standard CITATION NOTE.
- `DESCRIPTION`: added `knitr`/`rmarkdown` to Suggests and `VignetteBuilder: knitr` for vignettes.

### Fixed
- Improved pipeline portability by avoiding implicit package attachment and by using explicit imports.
- 

## Module 3 · Full estimator panel & CEGS priors
Date: 2025-11-27

Summary
- Added unified richness wrapper `estimate_richness_all()` covering Chao1/ACE/iChao1, Chao–Bunge, logseries, poilog, iNEXT coverage, preseqR extrapolations, and CEGS ML/LD (NA-on-failure semantics).
- Added CEGS parameter wrapper `fit_cegs_params()` to expose scale/shape/AICc for prior construction.
- Switched Dryad fetch to v2 API download endpoint to avoid 403s; cached gz stays under `data-raw/cache/`.
- Built real-data panels from the Ecological Register (≈180–200 inventories) for richness estimators and CEGS parameters under `data/processed/`.

Highlights
- Panel: `data/processed/ecoregister_richness_panel_full.csv` (most estimators populated; Chao–Bunge returns NA where breakaway fails).
- CEGS params: `data/processed/ecoregister_cegs_params.csv` plus summary/correlation tables for priors.
- Ingest fix for non-attached data.table usage; parallel panel builders for faster real-data runs.

Next
- Expand panel size; tune breakaway cutoff handling; add biome/region metadata when available.
