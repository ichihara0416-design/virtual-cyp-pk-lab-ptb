# run_all.R
# Run the complete reproducible analysis from the Analysis directory.
# Generated files are written to outputs/, figures/, and tables/.

options(stringsAsFactors = FALSE)

# Find the directory containing this script.
.this_file <- NULL
for (.i in rev(seq_along(sys.frames()))) {
  .candidate <- sys.frames()[[.i]]$ofile
  if (!is.null(.candidate)) {
    .this_file <- .candidate
    break
  }
}
if (is.null(.this_file)) {
  .args <- commandArgs(trailingOnly = FALSE)
  .file_arg <- grep("^--file=", .args, value = TRUE)
  if (length(.file_arg)) .this_file <- sub("^--file=", "", .file_arg[1])
}

.analysis_dir <- if (!is.null(.this_file)) {
  dirname(normalizePath(.this_file, winslash = "/", mustWork = TRUE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

.old_wd <- getwd()
on.exit(setwd(.old_wd), add = TRUE)
setwd(.analysis_dir)

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)
dir.create("figures", showWarnings = FALSE, recursive = TRUE)
dir.create("tables",  showWarnings = FALSE, recursive = TRUE)

.scripts <- c(
  "R/02_reference_calibration.R",
  "R/03_fit_all_PK.R",
  "R/04_behavior_and_residual.R",
  "R/05_external_transportability.R",
  "R/06_variability_and_app.R",
  "R/07_monte_carlo.R",
  "R/08_manuscript_outputs.R",
  "R/99_QA.R"
)

cat("Virtual CYP-PK Lab PTB analysis\n")
cat("Working directory:", getwd(), "\n\n")

for (.script in .scripts) {
  cat("---- Running", .script, "----\n")
  source(.script, echo = FALSE, chdir = FALSE)
  cat("---- Completed", .script, "----\n\n")
}

cat("All analysis scripts completed successfully.\n")
cat("Generated folders: outputs/, figures/, tables/\n")
