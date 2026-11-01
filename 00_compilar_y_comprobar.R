# Reproducible Research — Peer Assessment 1
# Run this script from the root directory of YOUR fork.

packages <- c("knitr", "markdown", "lattice")
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

cat("Working directory:", getwd(), "\n")
if (!file.exists("PA1_template.Rmd")) {
  stop("PA1_template.Rmd not found. Use Session > Set Working Directory > To Source File Location.")
}
if (!file.exists("activity.zip") && !file.exists("activity.csv")) {
  stop("Dataset not found. Open the folder of your fork containing activity.zip.")
}

# This is the format explicitly requested by the assignment.
knitr::knit2html("PA1_template.Rmd", force_v1 = TRUE)

needed <- c("PA1_template.Rmd", "PA1_template.md", "PA1_template.html")
for (f in needed) {
  cat(if (file.exists(f)) "[OK]" else "[MISSING]", f, "\n")
}

figures <- if (dir.exists("figure")) {
  list.files("figure", pattern = "\\.png$", full.names = TRUE)
} else {
  character(0)
}
cat("\nPNG figures:", length(figures), "\n")
if (length(figures)) print(figures)

if (!all(file.exists(needed)) || length(figures) < 4) {
  stop("Incomplete submission: please check the RStudio Console output above.")
}
cat("\nSUCCESS: source, Markdown, HTML and figures are ready to commit.\n")
