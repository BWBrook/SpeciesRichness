library(targets)
# Source package R/ functions for dev workflow without requiring installation
tarchetypes::tar_source("R")
tar_option_set(packages = c("data.table","cli","glue"))

list(
  tar_target(dryad_zip, fetch_ecoregister_zip(version = "v20250703"), format = "file"),
  tar_target(counts_dt, read_ecoregister_counts(dryad_zip)),
  tar_target(per_inv_sn, summarise_per_inventory(counts_dt)),
  tar_target(co_sample_flat, build_co_sample_flat(counts_dt), format = "file")
)
