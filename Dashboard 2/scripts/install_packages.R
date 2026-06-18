required_packages <- c(
  "bslib",
  "dplyr",
  "ggplot2",
  "glue",
  "htmltools",
  "leaflet",
  "plotly",
  "purrr",
  "readxl",
  "scales",
  "sf",
  "shiny",
  "stringr"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) == 0) {
  message("All required packages are already installed.")
} else {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
