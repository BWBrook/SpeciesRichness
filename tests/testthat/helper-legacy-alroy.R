alroy_dir <- Sys.getenv("ALROY_DIR", unset = "Alroy")
have_alroy <- dir.exists(alroy_dir)

# Optional runtime check for needed third-party deps used by originals
need_pkgs <- c("sads", "poilog", "minpack.lm", "stats4")
have_deps <- all(vapply(need_pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1)))

if (have_alroy && have_deps) {
  r_files <- list.files(alroy_dir, pattern = "[.]R$", full.names = TRUE)
  for (f in sort(r_files)) sys.source(f, envir = topenv())  # define originals in test env
} else {
  testthat::skip_if_not(have_alroy, "Alroy/ not available; skipping legacy equivalence tests.")
  testthat::skip_if_not(have_deps,  "richness deps (sads/poilog/minpack.lm) missing; skipping.")
}
