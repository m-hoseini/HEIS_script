library(tidyverse)
library(readxl)

rm(list = ls())

list_RAW <- list.files(path = "exported", pattern = "^HEIS.*\\Rdata$")

L <- data.frame(Global=1)
for (year in list_RAW) {
  y <- paste0("Y",parse_number(year))
  print(parse_number(year))
  load(paste0("./exported/",year))
  P <- bind_rows(mget(ls(pattern = "*P3S.*"))) |>
    # filter(is.na(value_r)&!is.na(value)) |>
    # select(Global) |>
    filter(is.na(Global)&!is.na(code)) |>
    select(Global, code) |>
    distinct() |> 
    add_row(Global = 1)
  P[[y]] <- 1 
  L <- full_join(L,P)
  rm(list=ls(pattern = parse_number(year)))
}

CPIglobal <- read_excel("CPI/CPI_Global.xlsx") |> 
  rename(cpicode=1, Global=2)  |>
  mutate(cpicode=as.numeric(cpicode),
         Global=as.numeric(Global)) 

GlobaltoID <- read_excel("Codes.xlsx", sheet = "CodeToCPI", range = "I2:J2086") |> 
  distinct(Global,ID) |> filter(!is.na(Global)) |> mutate(Global=as.integer(Global))

L <- L |> 
  arrange(Global) |>
  #left_join(CPIglobal) |>
  left_join(GlobaltoID) |>
  select(ID,everything()) |>
  write_csv("CPI_problems.csv",na="")
