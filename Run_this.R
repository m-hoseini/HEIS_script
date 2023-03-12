rm(list = ls())
Folder <- "C:/Users/Mohammad/Dropbox/HEIS/"

source(paste0(Folder,"script/mergeCPI.r"))

for (i in c(76:99)) {
  Folder <- "C:/Users/Mohammad/Dropbox/HEIS/"
  print(i)
  source(paste0(Folder,"script/",i,".R"))
}


### Generate Household-level file
rm(list = ls())

list_HH <- list.files(path = "exported", pattern = "^HH.*\\Rds$")

HH <- NULL
for (year in list_HH) {
  HH <- assign(year, readRDS(paste0("exported/",year))) |> 
    filter(!is.na(weight)) |>
    mutate(year = parse_number(year)) |>
    bind_rows(HH)
  rm(list=year)
}
saveRDS(IND,"HH.rds")

### Generate individual level file
rm(list = ls())
list_IND <- list.files(path = "exported", pattern = "^IND.*\\Rds$")
IND <- NULL
for (year in list_IND) {
  IND <- assign(year, readRDS(paste0("exported/",year))) |> 
    filter(!is.na(weight)) |>
    mutate(year = parse_number(year)) |>
    bind_rows(IND)
  rm(list=year)
}
saveRDS(IND,"IND.rds")

### Generate item level file
list_EXP <- list.files(path = "exported", pattern = "^EXP.*\\Rds$")
EXP <- NULL
for (year in list_EXP) {
  EXP <- assign(year, readRDS(paste0("exported/",year))) |> 
    mutate(year = parse_number(year)) |>
    bind_rows(EXP)
  rm(list=year)
}
saveRDS(EXP,"EXP.rds")
