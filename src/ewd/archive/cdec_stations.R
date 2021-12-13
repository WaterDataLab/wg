# all 2,893 CDEC stations (which likely include other injection ASR wells) are
# in the `ids` object, and come from a scrape-able page. 

# the metadata pages are not as scrape-able, so it's harder to extract + compile 
# individual station IDs, duration codes, and variables at each location

library(tidyverse)
library(httr)
library(rvest)

url <- "https://cdec.water.ca.gov/dynamicapp/staMeta?station_id="
ids <- read_html(url) %>% 
  html_nodes("#main_content a") %>% 
  html_attr("href") %>% 
  str_extract_all("=.*") %>% 
  str_remove_all("=")

path_data = "dynamicapp/req/CSVDataServlet" # data 
path_meta = "dynamicapp/staMeta"            # station metadata

id = "MDB"
query_meta = list(station_id = id)

url <- httr::modify_url("https://cdec.water.ca.gov", 
                        path  = path_meta, 
                        query = query_meta)
browseURL(url)

resp <- httr::GET(url)
resp$headers
