fix_libpaths <- function() {
  try({
    .libPaths(normalizePath(.libPaths(), winslash = "/", mustWork = FALSE))
    root <- tryCatch(here::here(), error = function(e) getwd())
    lib_candidate <- if (grepl("^/", .Library)) .Library else file.path(root, .Library)
    new_lib <- normalizePath(lib_candidate, winslash = "/", mustWork = FALSE)
    locked <- bindingIsLocked(".Library", baseenv())
    if (locked) {
      unlockBinding(".Library", baseenv())
      assign(".Library", new_lib, envir = baseenv())
      lockBinding(".Library", baseenv())
    } else {
      assign(".Library", new_lib, envir = baseenv())
    }
    if (isTRUE(getOption("speciesrichness.debug_legacy", FALSE))) {
      message("fix_libpaths: locked=", locked, " new_lib=", new_lib, " libPaths=", paste(.libPaths(), collapse = ";"))
    }
  }, silent = TRUE)
}
fix_libpaths()

default_alroy <- tryCatch(here::here("Alroy"), error = function(e) file.path(getwd(), "Alroy"))
alroy_dir <- Sys.getenv("ALROY_DIR", unset = default_alroy)
have_alroy <- dir.exists(alroy_dir)

# Optional runtime check for needed third-party deps used by originals
need_pkgs <- c("sads", "poilog", "minpack.lm", "stats4")
dep_checks <- vapply(need_pkgs, function(p) isTRUE(requireNamespace(p, quietly = TRUE)), FUN.VALUE = logical(1))
have_deps <- all(dep_checks)

legacy_available <- have_alroy && have_deps
legacy_skip_reason <- NULL

if (legacy_available) {
  r_files <- list.files(alroy_dir, pattern = "[.]R$", full.names = TRUE)
  for (f in sort(r_files)) sys.source(f, envir = topenv())  # define originals in test env
} else {
  if (!have_alroy) legacy_skip_reason <- sprintf("Alroy directory not found at '%s'", alroy_dir)
  else if (!have_deps) legacy_skip_reason <- "richness deps (sads/poilog/minpack.lm) missing"
}

if (isTRUE(getOption("speciesrichness.debug_legacy", FALSE))) {
  message("legacy helper: have_alroy=", have_alroy,
          " have_deps=", have_deps,
          " legacy_available=", legacy_available,
          " reason=", legacy_skip_reason,
          " dep_checks=", paste(names(dep_checks), dep_checks, sep = "=", collapse = ", "),
          " libPaths=", paste(.libPaths(), collapse = ";"), 
          " .Library=", .Library,
          " getwd=", getwd())
}
