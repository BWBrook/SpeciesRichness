# SpeciesRichness

Species richness estimation toolkit and reproducible pipelines built on the Ecological Register.

## What’s inside

- Ingest: `fetch_ecoregister_zip()` (Dryad API) and `read_ecoregister_counts()` produce long-form counts; `summarise_per_inventory()` gives `S_obs`/`N`.
- Estimators: `estimate_richness_all()` wraps Chao1/ACE/iChao1, Chao–Bunge, logseries/poilog, iNEXT coverage, preseqR extrapolations, and CEGS ML/LD.
- CEGS params: `fit_cegs_params()` exposes scale/shape/AICc for prior-building.
- Pipelines: `_targets.R` builds ingest outputs, CEGS benchmark, and sample richness / CEGS parameter panels.
- Data outputs (ignored by git): `data/processed/ecoregister_richness_panel_full.csv`, `data/processed/ecoregister_cegs_params.csv`.

## Quick start

```bash
make init          # restore renv + base toolchain
make test          # run unit tests
```

Data fetch (optional; uses Dryad API):

```r
source("scripts/renv_init.R")
devtools::load_all()
zip_path <- fetch_ecoregister_zip()  # cached under data-raw/cache/
dt <- read_ecoregister_counts(zip_path)
panel <- estimate_richness_all(split(dt$count, dt$sample_id)[[1]])
```

Pipeline (targets):

```r
source("scripts/renv_init.R")
targets::tar_make()                          # full pipeline
targets::tar_make(names = "richness_panel")  # sample richness panel
targets::tar_make(names = "cegs_params_panel")
```

## Outputs

- `data/processed/ecoregister_richness_panel_full.csv`: richness estimators for ~180 inventories (sampled; adjust in `_targets.R`).
- `data/processed/ecoregister_cegs_params.csv`: CEGS ML/LD parameters for ~200 inventories.
- Additional summaries/correlations under `data/processed/`.

## Notes

- Set `ECOREG_DATA_PATH` to a local `.gz` to skip network fetch.
- Optional dependencies (installed via renv): vegan, iNEXT, SpadeR/SPECIES, preseqR, breakaway, sads, poilog.
- All heavy computations return `NA` on estimator failure to keep pipelines resilient.
