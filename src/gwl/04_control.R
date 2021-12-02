library(here)
library(fs)
library(tidyverse)
library(lubridate)
library(sf)
library(leaflet)
library(leafpop)
library(leafem)
library(htmltools)
library(htmlwidgets)
library(plotly)

# run download

# loop over preprocessor and write dashboards

# write dashboards
write_dashboard <- function(x) {
  dir_out = sprintf("/Users/richpauloo/Documents/GitHub/wg/content/gsa-%s", x)
  dir_delete(dir_out)
  dir_create(dir_out)
  rmarkdown::render(input       = here("src/gwl/03_dashboard.Rmd"), 
                    output_file = path(dir_out, "index.html"),
                    params      = list(AOI = x)
  )
}

id <- unique(gwl$GSA_ID)[20]

preprocessed <- f_preprocess(id)
write_dashboard(id)
