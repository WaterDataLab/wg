# set up project
library(fs)
library(here)
library(tidyverse)

# create dir structure 
c("data_output", "src", "figures", "tables") %>% 
  walk(~dir_create(here(.x)))

dir_create(here("src/functions"))
