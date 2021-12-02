# find water year given year
f_calculate_water_year <- function(date){
  yr = lubridate::year(date)
  mh = lubridate::month(date)
  yr = ifelse(mh %in% 1:9, yr, yr + 1)
  return(yr)
}
