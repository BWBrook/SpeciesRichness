library(targets)
# Source package R/ functions for dev workflow without requiring installation
tar_source("R")
tar_option_set(packages = c("data.table","cli","glue"))
if (requireNamespace("qs", quietly = TRUE)) {
  tar_option_set(format = "qs")
}

list(
  tar_target(dryad_zip, fetch_ecoregister_zip(version = "v20250703"), format = "file"),
  tar_target(counts_dt, read_ecoregister_counts(dryad_zip)),
  tar_target(distinct_count_classes, counts_dt[, .(n_classes = data.table::uniqueN(count)), by = sample_id]),
  tar_target(eligible_inventories, distinct_count_classes[n_classes >= 4L]),
  tar_target(per_inv_sn, summarise_per_inventory(counts_dt)),
  tar_target(co_sample_flat, build_co_sample_flat(counts_dt), format = "file")
)
