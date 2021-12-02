st <- Sys.time()

# increase timeout to download larger data
options(timeout = 1000)

# tempfiles to hold downloaded data
tf1 <- tempfile()
tf2 <- tempfile()
tf3 <- tempfile()

# urls for GSA polygons from SGMA viewer, and periodic groundwater level measurement database
url_gsa <- "https://sgma.water.ca.gov/portal/service/gsadocument/exclusivegsa"
url_gwl <- "https://data.cnra.ca.gov/dataset/dd9b15f5-6d08-4d8c-bace-37dc761a9c08/resource/c51e0af9-5980-4aa3-8965-e9ea494ad468/download/periodic_gwl_bulkdatadownload.zip"
url_wyt <- "https://data.cnra.ca.gov/dataset/806ce291-645b-4646-8e15-9295b7740f5a/resource/b8fae043-4458-40f1-935c-4748157cbf92/download/sgma_wyt_dataset.csv"

# download files
cat("Downloading GSA and GWL urls...")
walk2(c(url_gsa, url_gwl), c(tf1, tf2), ~download.file(.x, .y))
cat("Done.\n")

# read gsa polygons
cat("Reading GSA polygons...")
unzip(tf1)
gsa <- st_read("GSA_Master.shp") %>% 
  st_transform(3310) %>% 
  rmapshaper::ms_simplify(keep_shapes = TRUE)
cat("Done.\n")

# read gwl data and make spatial
cat("Reading groundwater level measurements, stations, perforations...")
files_meas <- c("measurements.csv", "stations.csv", "perforations.csv")
gwl <- files_meas %>% 
  map(~read_csv(unzip(tf2, .x))) %>% 
  reduce(left_join, "SITE_CODE") %>% 
  select(-MONITORING_PROGRAM.x, MONITORING_PROGRAM = MONITORING_PROGRAM.y) %>% 
  # remove old measurements and nonsense above land surface measurements
  filter(MSMT_DATE >= lubridate::ymd("1960-01-01") & GSE_GWE >= 0) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4269, remove = FALSE) %>% 
  st_transform(3310) %>% 
  # add GSA data and remove: stations outside of GSAs
  st_join(gsa) %>% 
  filter(!is.na(GSA_Name)) %>%
  st_drop_geometry() %>% 
  # only retain sites with at least 3 measurements
  group_by(SITE_CODE) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n >= 3)
  
cat("Done.\n")

# TODO: download and join HUC8 boundaries to filter in the next step

# water year types from SGMA portal
cat("Downloading water year types...")
wyt <- read_csv(url_wyt)
cat("Done.\n")

# clean up
files_rm <- c(dir_ls(here(), regexp = "GSA_Master"), 
              here(files_meas),
              c(tf1, tf2, tf3))
walk(files_rm, ~unlink(.x))

Sys.time() - st; rm(st)
