# Bootstrap script for local development (run manually in RStudio)

if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
renv::activate()
renv::restore(prompt = FALSE)

if (requireNamespace("pak", quietly = TRUE)) {
  pak::pak()  # optional resolver/installer
}

if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::document()
  devtools::test()
}

message("Bootstrap complete. You can now run targets::tar_make().")

