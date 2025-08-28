# Small helper to install required packages and set up renv for the project
# Usage:
#   Rscript webapp/install_packages.R
# or open the project in R and run: source("webapp/install_packages.R")

pkgs <- c(
  "shiny", "shinythemes", "shinyjs", "DT", "dplyr",
  "cluster", "pheatmap", "qgraph", "Rtsne", "plotly"
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installing package: ", pkg)
    install.packages(pkg, repos = "https://cloud.r-project.org", dependencies = TRUE)
  } else {
    message(pkg, " is already available (base/installed)")
  }
}

# Ensure renv is available
if (!requireNamespace("renv", quietly = TRUE)) {
  message("Installing 'renv' from CRAN...")
  install.packages("renv", repos = "https://cloud.r-project.org")
}

# Load renv
library(renv)

proj_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
# If no renv lockfile or renv infrastructure exists, initialize renv non-interactively
if (!file.exists(file.path(proj_root, "renv.lock")) && !dir.exists(file.path(proj_root, "renv"))) {
  message("Initializing renv in project (non-interactive)...")
  # bare = TRUE avoids snapshotting the current state automatically
  renv::init(bare = TRUE)
} else {
  message("renv already initialised or lockfile exists; activating renv...")
  renv::activate()
}

# Try to install packages using renv (preferred). Fall back to install.packages on error.
for (p in pkgs) {
  message("Installing/ensuring: ", p)
  tryCatch(
    {
      renv::install(p)
    },
    error = function(e) {
      message("renv::install failed for ", p, ": ", e$message)
      message("Falling back to install.packages() for ", p)
      install_if_missing(p)
    }
  )
}

# Snapshot the project library to create/update renv.lock
message("Creating/updating renv lockfile (renv::snapshot)...")
renv::snapshot(prompt = FALSE)

message("All done. To use the project environment:")
message("  - Open the project folder in RStudio (renv activates automatically), or")
message("  - In a new R session run: renv::activate()")
message("You can re-create the environment later with: renv::restore()")
