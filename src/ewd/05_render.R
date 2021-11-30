rmarkdown::render(
  input = here::here("src/ewd/02_dashboard.Rmd"),
  output_file = here::here("content/ewd/index.html")
)
