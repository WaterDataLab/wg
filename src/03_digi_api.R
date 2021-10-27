library(httr2)
library(tidyverse)

# API url 
url <- "https://remotemanager.digi.com/ws/v1"

# query params
sensor_id <- "00010000-00000000-03566100-72541226"
sensor_params <- "cl1/cval"

# perform request with auth header at full url path
resp <- request(url) %>% 
  req_user_agent("wadl-digi") %>% 
  req_auth_basic(Sys.getenv("DIGI_USER"), Sys.getenv("DIGI_PASSWORD")) %>% 
  req_url_path_append("streams/rollups", sensor_id, sensor_params) %>% 
  req_perform()

# TODO: wrap resp in function for more general queries
# TODO: add query params for start and end datetime to permit caching old data and refreshing 
# with small daily updates
# https://www.digi.com/resources/documentation/digidocs/90001437-13/default.htm#reference/r_ex_ws_get_rollup_data_for_a_stream.htm%3FTocPath%3DWeb%2520services%2520reference%7Cv1%252Fstreams%7C_____11


# extract results ---------------------------------------------------------

l <- resp %>% 
  resp_body_json() %>% 
  pluck("list")

df <- tibble(value = map_dbl(l, "value"),
             dt    = map_chr(l, "timestamp")) %>% 
  mutate(dt = lubridate::ymd_hms(dt))

# plot
df %>% 
  # remove one crazy date in 1970
  filter(dt > lubridate::ymd("2019-01-01")) %>% 
  ggplot(aes(dt, value)) +
  geom_point(pch = 21)
