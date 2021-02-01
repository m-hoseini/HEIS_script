library(tidyverse) 
library(knitr)
library(kableExtra)

list_HH <- list.files(path = "exported", pattern = "^HH.*\\Rds$")

HH <- NULL
for (year in list_HH) {
  HH <- assign(year, readRDS(paste0("exported/",year))) %>% 
    mutate(year = parse_number(year)) %>%
    bind_rows(HH)
  rm(list=year)
}

HH %>%  group_by(year, province) %>%
  summarize(car_ownership=weighted.mean(vehicle,weight)*100) %>%
  pivot_wider(province, names_from="year", values_from="car_ownership")