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

ids_select <- c(85) # 219

for(i in seq_along(ids_select)){
  
  id <- ids_select[i]
  
  cat("Starting pipeline for GSA_ID", id, "[", i, "/", length(ids_select), "].\n")
  
  cat("  Preprocessing data...")
  preprocessed <- f_gwl_preprocess(id)
  cat("done.\n")
  
  cat("  Zipping data...")
  file_data <- here(glue::glue("content/gsa-{id}/gsa-{id}.csv"))
  file_zip  <- str_replace(file_data, ".csv", ".zip")
  file_s3   <- glue::glue("s3://wg-gwl/gsa-{id}/gsa-{id}.zip")
  file_sign <- here("presigned_url.txt")
  
  # write csv, zip it up, and rm csv
  preprocessed$maoi %>% write_csv(file_data)
  zip(zipfile = file_zip, files = file_data, extras = "-j")
  cat("done.\n")
  
  # move to s3 and generate presigned URL
  cat("  Pushing to S3 and generating presigned URL...")
  cmd_push <- glue::glue("aws s3 cp {file_zip} {file_s3}")
  # presigned url valid for a week (604800 seconds) written to a file
  cmd_sign <- glue::glue("aws s3 presign {file_s3} --expires-in 604800 > {file_sign}")
  system(cmd_push)
  system(cmd_sign)
  cat("done.\n")
  
  cat("  Writing dashboard...")
  presigned_url <- readLines(file_sign)
  f_write_dashboard(id)
  cat("done.\n")
  
  cat("  Encrypting dashboard...")
  # TODO: encryption
  # f_encrypt_dashboard(id)
  cat("done.\n")
  
  cat("  Cleaning up...")
  unlink(file_data); unlink(file_zip); unlink(file_sign)
  cat("done.\n\n")
}
