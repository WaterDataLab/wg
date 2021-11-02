f_transform_flow_volume <- function(flow_df, date_start, 
                                    date_end, af = FALSE,
                                    monthly_rollup = FALSE){
  if(monthly_rollup == FALSE){
    volume <- flow_df %>% 
      filter(obs_date >= date_start & obs_date <= date_end) %>% 
      mutate(volume_gal = value * 15) %>% 
      group_by(sensor_number) %>% 
      summarise(volume_gal_sum = sum(volume_gal, na.rm = TRUE)) %>% 
      ungroup()
  }
  if(monthly_rollup == TRUE){
    volume <- flow_df %>% 
      filter(obs_date >= date_start & obs_date <= date_end) %>% 
      mutate(volume_gal = value * 15,
             year_month = paste(lubridate::month(obs_date, label = TRUE),
                                lubridate::year(obs_date))) %>% 
      group_by(year_month, sensor_number) %>% 
      summarise(volume_gal_sum = sum(volume_gal, na.rm = TRUE)) %>% 
      ungroup() 
  }
  if(af == TRUE){
    volume <- volume %>% 
      mutate(volume_af_sum = volume_gal_sum * 3.06889e-6) %>% 
      select(-volume_gal_sum)
  }

  return(volume)
}

f_transform_flow_volume(flow, "2021-01-28", "2021-05-28", af = TRUE, monthly_rollup = TRUE)
