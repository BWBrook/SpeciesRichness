## targets pipeline
## - Explicit imports via import::from()/import::here() per AGENTS.md
## - Deterministic seed; no library() or tar_source() usage

import::from("targets", tar_target, tar_option_set)
import::here(read_ecoregister_counts, summarise_per_inventory, build_co_sample_flat, .from = "R/ingest_ecoregister.R")
import::here(fetch_ecoregister_zip, .from = "R/fetch_ecoregister.R")
import::here(topk_fingerprint, nearest_neighbour, tail_histogram, tail_nll, .from = "R/benchmark_cegs.R")
import::here(cegs_ml, cegs_ld, .from = "R/cegs.R")
import::here(estimate_richness_all, fit_cegs_params, .from = "R/estimate_richness_all.R")

# Global options for targets
tar_option_set(seed = 1L)
if (isTRUE(requireNamespace("qs", quietly = TRUE))) {
  tar_option_set(format = "qs")
}

list(
  tar_target(dryad_zip, fetch_ecoregister_zip(version = "v20250703"), format = "file"),
  tar_target(counts_dt, read_ecoregister_counts(dryad_zip)),
  tar_target(distinct_count_classes, counts_dt[, list(n_classes = data.table::uniqueN(count)), by = sample_id]),
  tar_target(eligible_inventories, distinct_count_classes[n_classes >= 4L]),
  tar_target(per_inv_sn, summarise_per_inventory(counts_dt)),
  tar_target(co_sample_flat, build_co_sample_flat(counts_dt), format = "file"),

  # --- CEGS benchmark (pair eligible inventories and score neighbour tails) ---
  tar_target(eligible_ids, eligible_inventories$sample_id),
  tar_target(topk_matrix, {
    ids <- eligible_ids
    k <- 10L
    M <- matrix(NA_real_, nrow = length(ids), ncol = k, dimnames = list(ids, paste0("k", seq_len(k))))
    split_counts <- split(counts_dt$count, counts_dt$sample_id)
    for (sid in ids) {
      v <- split_counts[[sid]]
      if (length(v)) M[sid, ] <- topk_fingerprint(as.integer(v), k = k)
    }
    M
  }),
  tar_target(nearest_pair, {
    nn <- nearest_neighbour(topk_matrix)
    data.table::data.table(focal = names(nn), neighbour = unname(nn))[!is.na(neighbour)]
  }),
  tar_target(cegs_scores, {
    split_counts <- split(counts_dt$count, counts_dt$sample_id)
    res <- lapply(seq_len(nrow(nearest_pair)), function(i) {
      fi <- nearest_pair$focal[i]; ni <- nearest_pair$neighbour[i]
      n_focal <- as.integer(split_counts[[fi]])
      n_nei   <- as.integer(split_counts[[ni]])
      if (!length(n_focal) || !length(n_nei)) return(NULL)

      th <- tail_histogram(n_nei, k = 10L, max_x = 4096L)
      fit_ml <- try(cegs_ml(n_focal), silent = TRUE)
      fit_ld <- try(cegs_ld(n_focal), silent = TRUE)
      pm <- if (is.list(fit_ml) && length(fit_ml$fitted_SAD)) fit_ml$fitted_SAD else NA
      pd <- if (is.list(fit_ld) && length(fit_ld$fitted_SAD)) fit_ld$fitted_SAD else NA

      data.table::data.table(
        focal = fi, neighbour = ni,
        ll_cegs_ml = if (is.numeric(pm)) tail_nll(th$s, th$u, pm) else NA_real_,
        ll_cegs_ld = if (is.numeric(pd)) tail_nll(th$s, th$u, pd) else NA_real_
      )
    })
    data.table::rbindlist(res, use.names = TRUE, fill = TRUE)
  }),
  tar_target(cegs_summary, {
    x <- cegs_scores
    dec <- function(a, b) {
      o <- sum(a < b, na.rm = TRUE)
      u <- sum(a > b, na.rm = TRUE)
      data.table::data.table(o = o, u = u, p = o/(o+u))
    }
    basic <- dec(x$ll_cegs_ld, x$ll_cegs_ml)
    margin <- dec(x$ll_cegs_ld, x$ll_cegs_ml - log(10))
    list(basic = basic, margin10 = margin)
  }),

  # --- Richness / CEGS panels (deterministic subset of inventories) ---
  tar_target(panel_ids, {
    ids <- sort(unique(counts_dt$sample_id))
    head(ids, 180L)
  }),
  tar_target(split_counts, split(counts_dt$count, counts_dt$sample_id)),
  tar_target(richness_panel, {
    res <- lapply(panel_ids, function(sid) {
      counts <- as.integer(split_counts[[sid]])
      out <- try(estimate_richness_all(counts), silent = TRUE)
      if (inherits(out, "try-error")) return(NULL)
      out$sample_id <- sid
      out
    })
    data.table::rbindlist(res, fill = TRUE)
  }),
  tar_target(richness_panel_csv, {
    dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
    p <- file.path("data/processed", "ecoregister_richness_panel_full.csv")
    data.table::fwrite(richness_panel, p)
    p
  }, format = "file"),

  tar_target(cegs_params_panel, {
    res <- lapply(panel_ids, function(sid) {
      counts <- as.integer(split_counts[[sid]])
      out <- try(fit_cegs_params(counts), silent = TRUE)
      if (inherits(out, "try-error")) return(NULL)
      out$sample_id <- sid
      out
    })
    data.table::rbindlist(res, fill = TRUE)
  }),
  tar_target(cegs_params_csv, {
    dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
    p <- file.path("data/processed", "ecoregister_cegs_params.csv")
    data.table::fwrite(cegs_params_panel, p)
    p
  }, format = "file")
)
