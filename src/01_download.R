library(tidyverse)
library(here)
library(fs)
library(httr)
library(patchwork)

path = "dynamicapp/req/CSVDataServlet"

query = list(Stations = "MDB",
             Start = "2021-01-01",
             End = Sys.Date())

# flow to aquifers A, B
sensor_nums <- 291:292

# flow API response
resp_flow <- map(sensor_nums, 
                 ~f_cdec_api(path = path, 
                             query = c(query, dur_code = "E", SensorNums = .x)))
flow <- map_df(resp_flow, "content") %>%
  janitor::clean_names() %>% 
  mutate(date = as.Date(date_time),
         sensor_number = factor(sensor_number),
         value = as.numeric(value),
         value = ifelse(value < 0, 0, value))

# battery life API response 
resp_bat <- f_cdec_api(path = path,
                       query = c(query, dur_code = "H", SensorNums = 14))
bat <- resp_bat$content %>% 
  janitor::clean_names() %>% 
  mutate(date = as.Date(date_time),
         value = as.numeric(value),
         value = ifelse(value < 0, 0, value))


# visualize ---------------------------------------------------------------

p_flow <- flow %>% 
  group_by(date, sensor_number) %>% 
  summarise(mean_value = mean(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  ggplot(aes(date, mean_value, color = sensor_number)) +
  geom_line(key_glyph = "timeseries") +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m-%d",
               guide = guide_axis(angle = 45)) +
  rcartocolor::scale_color_carto_d(palette = "Vivid") +
  labs(y = "gallons / minute", x = "", 
       title = "Mean daily injection rate at ASR wells", 
       color = "Sensor number") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = c(0.9, 0.8), 
        legend.background = element_rect(fill = "white", color = "white"))

p_bat <- bat %>% 
  group_by(date) %>% 
  summarise(mean_value = mean(value, na.rm = TRUE)) %>% 
  ungroup() %>%
  ggplot(aes(date, mean_value)) +
  geom_line(key_glyph = "timeseries") +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m-%d",
               guide = guide_axis(angle = 45)) +
  labs(y = "Volts", x = "", 
       title = "Mean daily battery voltage at ASR sensors 291, 292") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())


p <- p_flow + p_bat + plot_layout(nrow = 2)
ggsave(here("figures/p_daily_values.png"), height = 8, width = 10)
