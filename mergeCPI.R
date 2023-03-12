library(tidyverse)
library(data.table)
library(readxl)

# rm(list = ls())

# Folder <- "C:/Users/Mohammad/Dropbox/HEIS/"

UsedCodes <- read_excel(paste0(Folder,"Codes.xlsx"), sheet = "CodeToCPI", range = "J2:J2085") |> 
  distinct(ID) |> filter(!is.na(ID)) |> arrange(ID) |> mutate(f=1)

GlobaltoID <- read_excel(paste0(Folder,"Codes.xlsx"), sheet = "CodeToCPI", range = "I2:J2087") |> 
  distinct(Global,ID) |> filter(!is.na(Global)) 

CPI95 <- read_excel(paste0(Folder,"CPI_raw.xlsx"), sheet = "B95_92-95") |> 
  mutate(weight=pmax(weight,0.001)) |>
  group_by(ID) |>
  summarize(across(c(`1392/01`:`1399/12`),~ weighted.mean(.,weight))) |>
  ungroup() |>
  pivot_longer(-ID, names_to = "date", values_to = "cpi95")
  
CPI90_90 <- read_excel(paste0(Folder,"CPI_raw.xlsx"), sheet = "B90_90-95") |> 
  mutate(weight=pmax(weight,0.001)) |>
  group_by(ID) |>
  summarize(across(c(`1390/01`:`1395/12`),~ weighted.mean(.,weight))) |>
  pivot_longer(c(`1390/01`:`1395/12`), names_to = "date", values_to = "CPI90") |>
  ungroup()

CPI90_83 <- read_excel(paste0(Folder,"CPI_raw.xlsx"), sheet = "B90_83-89") |> 
  group_by(ID) |>
  summarize(across(c(`1383/01`:`1389/12`), mean)) |>
  pivot_longer(c(`1383/01`:`1389/12`), names_to = "date", values_to = "CPI83") |>
  ungroup()

CPI90_76 <- read_excel(paste0(Folder,"CPI_raw.xlsx"), sheet = "B90_76-82") |> 
  group_by(ID) |>
  summarize(across(c(`1376/01`:`1382/12`), mean)) |>
  pivot_longer(c(`1376/01`:`1382/12`), names_to = "date", values_to = "CPI76") |>
  ungroup()

CPI90_69 <- read_excel(paste0(Folder,"CPI_raw.xlsx"), sheet = "B90_69-75") |> 
  group_by(ID) |>
  summarize(across(c(`1369/01`:`1375/12`), mean)) |>
  pivot_longer(c(`1369/01`:`1375/12`), names_to = "date", values_to = "CPI69") |>
  ungroup()

CPI90_63 <- read_excel(paste0(Folder,"CPI_raw.xlsx"), sheet = "B90_63-68") 
for (i in 1363:1368) {
  CPI90_63[,paste0(i,"/",str_pad(1:12,2,pad ="0"))] <- CPI90_63[,paste0(i)]
}
CPI90_63 <- CPI90_63 |> 
  group_by(ID) |>
  summarize(across(c(`1363/01`:`1368/12`), mean)) |>
  pivot_longer(c(`1363/01`:`1368/12`), names_to = "date", values_to = "CPI63") |>
  ungroup()

CPI90 <- CPI90_90 |>
  bind_rows(CPI90_83) |>
  bind_rows(CPI90_76) |>
  bind_rows(CPI90_69) |>
  bind_rows(CPI90_63) |>
  select(ID, date, starts_with("CPI"))

IDs <- unique(CPI90$ID)

CPI90 <- CPI90 |>
  pivot_wider(date, names_from = "ID", values_from = starts_with("CPI")) |>
  mutate(bY=as.integer(str_sub(date,3,4)))

CPI <- CPI90 |> select(date,bY)
for (i in IDs) {
  CPI[CPI90$bY > 89, paste0("cpi_", i)] <- get(paste0("CPI90_",i),CPI90)[CPI90$bY > 89]
  CPI[CPI90$bY %in% 83:89, paste0("cpi_", i)] <- get(paste0("CPI83_",i),CPI90)[CPI90$bY %in% 83:89]
  CPI[CPI90$bY %in% 76:82, paste0("cpi_", i)] <- get(paste0("CPI76_",i),CPI90)[CPI90$bY %in% 76:82]
  CPI[CPI90$bY %in% 69:75, paste0("cpi_", i)] <- get(paste0("CPI69_",i),CPI90)[CPI90$bY %in% 69:75]
  CPI[CPI90$bY %in% 63:68, paste0("cpi_", i)] <- get(paste0("CPI63_",i),CPI90)[CPI90$bY %in% 63:68]
}

### finding missing codes in each year
CPI |> 
  filter(bY %in% 63:68) |>
  summarise_all(mean) |>
  pivot_longer(everything()) |>
  filter(is.na(value)) |>
  mutate(ID=str_sub(name,5,11)) |> 
  left_join(UsedCodes) |>
  filter(!is.na(f)) |>
  select(ID) |>
  write.table("clipboard",sep ="\t")

code83 <- read_excel(paste0(Folder,"CPI_raw.xlsx"), sheet = "code changes", range = "C1:D36")

for (n in 1:nrow(code83) )  {
  i <- code83$C95[n]
  j <- code83$C83[n]
  r <- get(paste0("CPI90_",i),CPI90)[CPI90$date == "1390/01"]/get(paste0("CPI90_",j),CPI90)[CPI90$date == "1390/01"]
  CPI[CPI90$bY %in% 83:89, paste0("cpi_", i)] <- r*get(paste0("CPI83_",j),CPI90)[CPI90$bY %in% 83:89]
  CPI90[CPI90$bY %in% 83:89, paste0("CPI83_", i)] <- get(paste0("cpi_", i),CPI)[CPI90$bY %in% 83:89] # fill CPI90 for getting ratio in 76
}

code76 <- read_excel(paste0(Folder,"CPI_raw.xlsx"), sheet = "code changes", range = "E1:F116")

for (n in 1:nrow(code76) )  {
  i <- code76$C95[n]
  j <- code76$C76[n]
  r <- get(paste0("CPI83_",i),CPI90)[CPI90$date == "1383/01"]/get(paste0("CPI83_",j),CPI90)[CPI90$date == "1383/01"]
  CPI[CPI90$bY %in% 76:82, paste0("cpi_", i)] <- r*get(paste0("CPI76_",j),CPI90)[CPI90$bY %in% 76:82]
  CPI90[CPI90$bY %in% 76:82, paste0("CPI76_", i)] <- get(paste0("cpi_", i),CPI)[CPI90$bY %in% 76:82] # fill CPI90 for getting ratio in 69
}

code69 <- read_excel(paste0(Folder,"CPI_raw.xlsx"), sheet = "code changes", range = "G1:H134")

for (n in 1:nrow(code69) )  {
  i <- code69$C95[n]
  j <- code69$C69[n]
  r <- get(paste0("CPI76_",i),CPI90)[CPI90$date == "1376/01"]/get(paste0("CPI76_",j),CPI90)[CPI90$date == "1376/01"]
  CPI[CPI90$bY %in% 69:75, paste0("cpi_", i)] <- r*get(paste0("CPI69_",j),CPI90)[CPI90$bY %in% 69:75]
  CPI90[CPI90$bY %in% 69:75, paste0("CPI69_", i)] <- get(paste0("cpi_", i),CPI)[CPI90$bY %in% 69:75] # fill CPI90 for getting ratio in 63
}

code63 <- read_excel(paste0(Folder,"CPI_raw.xlsx"), sheet = "code changes", range = "I1:J364")

for (n in 1:nrow(code63) )  {
  i <- code63$C95[n]
  j <- code63$C63[n]
  r <- get(paste0("CPI69_",i),CPI90)[CPI90$date == "1369/01"]/get(paste0("CPI69_",j),CPI90)[CPI90$date == "1369/01"]
  CPI[CPI90$bY %in% 63:68, paste0("cpi_", i)] <- r*get(paste0("CPI63_",j),CPI90)[CPI90$bY %in% 63:68]
}

CPI <- CPI |>
  arrange(date) |> select(-bY) |>
  pivot_longer(starts_with("cpi_"),names_to = "ID", names_prefix = "cpi_", values_to = "cpi", values_drop_na = T) |>
  full_join(CPI95) |>
  group_by(ID) |>
  mutate(ratio = mean(cpi95/cpi,na.rm=T)) |>
  ungroup() |>
  mutate(ratio = ifelse(is.nan(ratio),mean(cpi95/cpi,na.rm=T),ratio),
         cpi = ifelse(is.na(cpi),cpi95/ratio, cpi),
         year = as.integer(str_sub(date,1,4)),
         month = as.integer(str_sub(date,6,7))) |>
  select(year, month, ID, cpi) |>
  filter(!is.na(ID))

# for additional item in 1395
CPI <- CPI |>
  pivot_wider(c(year,month), names_from = "ID", names_prefix = "v", values_from = "cpi") |>
  mutate(ratio = mean(ifelse(year==1392&month==1,v127124/v000200,NA), na.rm = T),
         v127124 = ifelse(is.na(v127124),ratio*v000200,v127124)) |>
  select(-ratio) |>
  pivot_longer(-c(year,month), names_to = "ID", names_prefix = "v", values_to = "cpi", values_drop_na = T)

GlobaltoID |>
  left_join(CPI) |>
  select(year, month, Global, cpi) |>
  arrange(Global, year, month) |>
  group_by(Global) |>
  mutate(cpi_y= reduce(map(0:11, ~ lag(cpi/12, .)), `+`),
         Global = as.integer(Global)) |>
  # write.table("clipboard", sep = "\t")
  saveRDS("CPI.rds")

read_excel(paste0(Folder,"Codes.xlsx"), sheet = "item", range = "G1:AY2600") |> 
  filter(!is.na(Global)) |> 
  select(-DYCOL00) |>
  mutate(across(Global:R63, as.integer)) |>
  saveRDS("Global.rds")

Province <- c(Markazi="00", Gilan="01", Mazandaran="02", AzarbaijanSharghi="03", AzarbaijanGharbi="04",
              Kermanshah="05", Kouzestan="06", Fars="07", Kerman="08", KhorasanRazavi="09",
              Esfahan="10", SistanBalouchestan="11", Kordestan="12", Hamedan="13", CharmahalBakhtiari="14",
              Lorestan="15", Ilam="16", KohkilouyeBoyerahamad="17", Boushehr="18", Zanjan="19", 
              Semnan="20", Yazd="21", Hormozgan="22", Tehran="23", Ardebil="24")

read_excel(paste0(Folder,"population65-75.xlsx"), sheet = "weight") |> 
  filter(!is.na(Code)) |> 
  mutate(province = fct_recode(as.factor(Code), !!!Province)) |>
  select(province, R_U, starts_with("Pop")) |>
  saveRDS("pop63_75.rds")
