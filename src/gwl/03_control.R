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
# id <- unique(gwl$GSA_ID)[20]

ids_select <- c(84, 219)
  
preprocessed <- f_gwl_preprocess(id)
f_write_dashboard(id)

# data csv and zip files
file_data <- here(glue::glue("content/gsa-{id}/gsa-{id}.csv"))
file_zip  <- str_replace(file_data, ".csv", ".zip")

# write csv, zip it up, and rm csv
gwl %>% filter(GSA_ID == id) %>% write_csv(file_data)
zip(zipfile = file_zip, files = file_data, extras = "-j")
unlink(file_data)
