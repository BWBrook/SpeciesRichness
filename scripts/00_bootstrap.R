#!/usr/bin/env Rscript
# Bootstraps a minimal toolchain for this project.

message(">> Initialising renv (bare)...")
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
renv::init(bare = TRUE)  # creates renv/ and renv.lock for current R

# Ensure CRAN mirror is stable
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Core packages (install via renv so they’re pinned)
pkgs <- c(
  "pak",          # fast resolver/installer
  "targets", "tarchetypes",
  "testthat", "devtools", "usethis",
  "readr", "data.table", "jsonlite", "httr", "curl",
  "here", "cli", "glue"
)
renv::install(pkgs)

# Snapshot lockfile
renv::snapshot(prompt = FALSE)

# Minimal testing scaffold
if (!file.exists("tests/testthat.R")) {
  usethis::use_testthat(edition = 3)
}

message(">> Done. renv.lock created; packages installed.")

