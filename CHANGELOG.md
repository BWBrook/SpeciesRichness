# Changelog

All notable changes to this project will be documented in this file.

The format follows Keep a Changelog and this project adheres to semantic versioning during development.

## [Unreleased]
### Added
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

### Changed
- `_targets.R`: removed `library()` and `tar_source()`, added `import::from()`/`import::here()`, and set deterministic `tar_option_set(seed = 1L)`.
- `_targets.R`: replaced `data.table:::.()` alias with `list()` to avoid hidden imports.
- R ingest helpers: switched `stopifnot()`/`stop()` to `rlang::abort()`; default output path now via `here::here()`.
- `.Rbuildignore` and `.gitignore` updated to include `.lintr`, `.quarto/`, and `docs/_site/`.
- `DESCRIPTION`: added `stats4` to Imports.

### Fixed
- Improved pipeline portability by avoiding implicit package attachment and by using explicit imports.


---

Historical notes prior to this file live in `docs/CHANGELOG.md` (kept for reference).
