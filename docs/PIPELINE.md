# Pipeline

Overview of the `{targets}` pipeline and how to work with it.

## Targets

- `dryad_zip`: file target. Path to the Ecological Register `.gz` (either downloaded or user-provided via `ECOREG_DATA_PATH`).
- `counts_dt`: in-memory. Long-form counts `data.table` with `sample_id`, `taxon`, `count`.
- `distinct_count_classes`: in-memory. Per `sample_id` count-class cardinality.
- `eligible_inventories`: in-memory. Filtered inventories with `>= 4` count classes.
- `per_inv_sn`: in-memory. Per-inventory richness `S` and individuals `N`.
- `co_sample_flat`: file target. CSV written under `inst/extdata/co_sample_flat.csv`.

### CEGS benchmark (added)
- `eligible_ids`: vector of eligible `sample_id`s.
- `topk_matrix`: k=10 scale-invariant fingerprints per inventory.
- `nearest_pair`: nearest neighbour pairing by Euclidean distance.
- `cegs_scores`: neighbour tail log-likelihoods for CEGS-ML/LD.
- `cegs_summary`: basic vs 10x-margin win rates (LD vs ML).

## Options

- Seed: `tar_option_set(seed = 1L)` for deterministic behavior.
- Optional fast storage: if `qs` is installed, `format = "qs"` is enabled.

## Visualize the DAG

```r
targets::tar_visnetwork()
```

## Build instructions

Do not run commands here automatically. From the RStudio Console:

```r
# Full pipeline
targets::tar_make()

# Specific targets
targets::tar_make(names = c("dryad_zip", "co_sample_flat"))
```
