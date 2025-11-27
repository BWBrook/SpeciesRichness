#!/usr/bin/env Rscript
# Bootstraps a minimal toolchain for this project.

# Be explicit about CRAN mirror and renv cache for non-interactive runs
options(repos = c(CRAN = "https://cloud.r-project.org"))
Sys.setenv(RENV_CONFIG_USE_CACHE = "TRUE")

message(">> Initialising renv (idempotent)...")
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
if (!file.exists("renv/activate.R")) {
  renv::init(bare = TRUE)  # creates renv/ and renv.lock for current R
} else {
  renv::activate()
}

# Core packages (install via renv so they’re pinned)
pkgs <- c(
  "pak",          # fast resolver/installer
  "targets", "tarchetypes",
  "testthat", "devtools", "usethis",
  "readr", "data.table", "jsonlite", "httr", "curl",
  "here", "cli", "glue",
  # rich estimator suite (Suggests)
  "vegan", "iNEXT", "SpadeR", "SPECIES", "preseqR", "breakaway",
  "sads", "poilog"
)
renv::install(pkgs)

# Snapshot lockfile
renv::snapshot(prompt = FALSE)

# Minimal testing scaffold (robust to non-package projects)
if (!file.exists("tests/testthat.R")) {
  ok <- FALSE
  if (requireNamespace("usethis", quietly = TRUE)) {
    ok <- isTRUE(tryCatch({ usethis::use_testthat(edition = 3); TRUE }, error = function(e) FALSE))
  }
  if (!ok) {
    dir.create("tests/testthat", recursive = TRUE, showWarnings = FALSE)
    writeLines("# bootstrap placeholder for testthat\nif (requireNamespace(\"testthat\", quietly = TRUE)) {\n  testthat::test_that(\"bootstrap placeholder\", {\n    testthat::expect_true(TRUE)\n  })\n}", "tests/testthat.R")
  }
}

message(">> Done. renv.lock created; packages installed.")
