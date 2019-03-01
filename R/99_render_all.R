library(here)

rmarkdown::render(
  input = here("R", "02_plot-data.Rmd"), 
  output_format = "github_document",
  output_file = here("documents", "02_plot-data.md")
)