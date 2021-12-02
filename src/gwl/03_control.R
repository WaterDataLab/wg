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

# helper functions
source(here("src/functions/f_gwl_helpers.R"))
source(here("src/functions/f_calculate_water_year.R"))
source(here("src/functions/f_gwl_preprocess.R"))

# download data
source(here("src/gwl/01_download.R"))

# build all dashboards
# id <- 84 # small iterable

ids_select <- c(84) # 219

for(i in seq_along(ids_select)){
  id <- ids_select[i]
  cat("Preprocessing data for GSA_ID", id, "...")
  preprocessed <- f_gwl_preprocess(id)
  cat("Done.\n")
  
  cat("Writing dashboard for GSA_ID", id, "...")
  f_write_dashboard(id)
  cat("Done.\n")
  
  cat("Zipping data for GSA_ID", id, "...")
  file_data <- here(glue::glue("content/gsa-{id}/gsa-{id}.csv"))
  file_zip  <- str_replace(file_data, ".csv", ".zip")
  
  # write csv, zip it up, and rm csv
  gwl %>% filter(GSA_ID == id) %>% write_csv(file_data)
  zip(zipfile = file_zip, files = file_data, extras = "-j")
  cat("Done.\n")
  
  cat("Cleaning up...")
  unlink(file_data)
  cat("Done.\n")
}
