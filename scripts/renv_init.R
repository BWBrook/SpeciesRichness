# Shared renv bootstrap for CLI/CI commands
if (!file.exists('renv/activate.R')) {
  stop('renv/activate.R not found; run scripts/00_bootstrap.R first')
}
source('renv/activate.R')

# Normalise library paths to absolute so subdir working dirs (e.g., testthat)
# still resolve packages correctly.
try({
  .libPaths(normalizePath(.libPaths(), winslash = '/', mustWork = FALSE))
  root <- tryCatch(here::here(), error = function(e) getwd())
  lib_candidate <- if (grepl('^/', .Library)) .Library else file.path(root, .Library)
  new_lib <- normalizePath(lib_candidate, winslash = '/', mustWork = FALSE)
  if (bindingIsLocked('.Library', baseenv())) {
    unlockBinding('.Library', baseenv()); assign('.Library', new_lib, envir = baseenv()); lockBinding('.Library', baseenv())
  } else {
    assign('.Library', new_lib, envir = baseenv())
  }
}, silent = TRUE)
