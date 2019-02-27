library(here)

rmarkdown::render(
  input = here("R", "02_plot-data.Rmd"), 
  output_format = c("html_document", "github_document"),
  output_dir = here("documents")
)