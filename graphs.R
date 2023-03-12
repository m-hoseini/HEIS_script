library(tidyverse)
library(data.table)
library(readxl)

setwd("D:/HEIS/")

rm(list = ls())

EXP <- readRDS("exported/EXP98.rds") |>  mutate(year=98)
for (year in 97:90) {
  EXP <- assign(paste0("EXP",year), readRDS(paste0("exported/EXP",year,".rds"))) |> 
    mutate(year = year) |>
    bind_rows(EXP)
}

exp <- EXP |>
   mutate(  Table = case_when(
    table == 1 ~ "food",
    table == 2 ~ "tobacco",
    table == 3 ~ "clothing",
    table == 4 ~ "housing",
    table == 5 ~ "appliances",
    table == 6 ~ "health",
    table == 7 ~ "transport",
    table == 8 ~ "communication",
    table == 9 | table == 11 ~ "recreation & restaurant",
    table == 12 ~ "miscellaneous",
    table == 13 ~ "durables",
    table == 14 ~ "investment",
    TRUE ~ NA_character_)) |>
  group_by(Table, year) |>
  summarise(value_r=sum(Value_r,na.rm = T), value=sum(Value,na.rm = T))
    
ggplot(exp, aes(x=year, y=log(value_r), color=Table, shape=Table)) +
  geom_point() + geom_line() +
  facet_wrap(~Table, scales = "free_y")

for (year in 91:98) {
  assign(paste0("IND",year), readRDS(paste0("exported/IND",year,".rds")))
}

exp <- EXP |>
  filter(gcode %/% 10 %in% c(6222)) |>
  mutate(table = gcode  ) |>
  group_by(table, year, urban) |>
  summarise(value_r=sum(Value_r,na.rm = T), value=sum(Value,na.rm = T)) |>
  mutate(table=as.factor(table))

ggplot(exp, aes(x=year, y=value_r, color=table, shape=table)) +
  geom_point() + geom_line() +
  facet_wrap(~urban, scales = "free_y")

