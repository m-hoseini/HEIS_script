library(tidyverse)
library(data.table)
library(readxl)

rm(list = ls())

load("./RAW/87.RData")
#############################################################
Province <- c(Markazi="00", Gilan="01", Mazandaran="02", AzarbaijanSharghi="03", AzarbaijanGharbi="04",
              Kermanshah="05", Kouzestan="06", Fars="07", Kerman="08", KhorasanRazavi="09",
              Esfahan="10", SistanBalouchestan="11", Kordestan="12", Hamedan="13", CharmahalBakhtiari="14",
              Lorestan="15", Ilam="16", KohkilouyeBoyerahamad="17", Boushehr="18", Zanjan="19", 
              Semnan="20", Yazd="21", Hormozgan="22", Tehran="23", Ardebil="24", Qom="25", Qazvin="26",
              Golestan="27", KhorasanShomali="28", KhorasanJonoubi="29")

R87_weight <- Sum_R87 |> 
  select(ADDRESS, weight) |>
  rename(Address = ADDRESS) |> 
  mutate_if(is.character, as.numeric)
R87Data <- R87Data |> 
  rename(month = MahMorajeh, khanevartype = NoeKhn) |>
  mutate(province = fct_recode(as.factor(substr(Address, 2, 3)), !!!Province)) |>
  left_join(R87_weight)

U87_weight <- Sum_U87 |>
  select(ADDRESS, weight) |>
  rename(Address = ADDRESS) |>
  mutate_if(is.character, as.numeric)
U87Data <- U87Data |> 
  rename(month = MahMorajeh, khanevartype = NoeKhn) |>
  mutate(province = fct_recode(as.factor(substr(Address, 2, 3)), !!!Province)) |>
  left_join(U87_weight)

##############################
# Part 1

relation <- c(head="1", spouse="2", child="3", childinlaw="4", grandchild="5", parent="6", sibling="7", relative="8", nonrelative="9")
gender <- c(Male="1", Female="2")
literacy <- c(literate="1", illiterate="2", NULL="", NULL="-")
yesno <- c(Yes="1", No="2", NULL="", NULL="-")
education <- c(Elemantry="1", Secondary="2", HighSchool="3", Diploma="4", College="5", Bachelor="6", Master="7", PhD="8", Other="9")
occupation <- c(employed="1", unemployed="2", IncomeWOJob="3", Student="4", Housewife="5", Other="6")
marital <- c(Married ="1", Widowed="2", Divorced="3", Single="4")


R87P1 <- R87P1 |> 
  rename(
    member = DYCOL01,
    relation = DYCOL03,
    gender = DYCOL04,
    age = DYCOL05,
    literacy = DYCOL06,
    studying = DYCOL07,
    degree = DYCOL08,
    occupationalst = DYCOL09,
    maritalst = DYCOL10) |> 
  mutate(across(where(is.character), as.integer),
         across(c(relation,gender,literacy,studying,degree,occupationalst,maritalst), as.factor),
         relation = fct_recode(relation, !!!relation), 
         gender = fct_recode(gender, !!!gender),
         literacy = fct_recode(literacy, !!!literacy), 
         studying = fct_recode(studying, !!!yesno),
         occupationalst = fct_recode(occupationalst, !!!occupation),
         maritalst = fct_recode(maritalst, !!!marital),
         education = fct_relevel(as.factor(case_when(
           str_sub(degree,1,1)=="1"| ( studying == "No" & degree %in% c("210","211","212","213") ) ~ "Elemantry",
           ( degree %in% c("214","215","216","217","220","221","222") ) | 
             ( studying == "No" & degree %in% c("311","312","321","323","31A","31a","32A","32a","316","325","31B","32B","31b","32b","318","327") ) | 
             (str_sub(degree,1,1)=="2" & studying == "Yes") ~ "Secondary", 
           ( studying == "Yes" & degree %in% c("311","312","321","323","31A","31a","32A","32a","316","325","31B","32B","31b","32b","318","327") ) ~ "HighSchool", 
           degree %in% c("313","314","315","322","324","300","301","317","326","302","319","328","303") ~ "Diploma", 
           degree %in% c("521","522") ~ "College",
           degree %in% c("511","512") ~ "Bachelor",
           degree %in% c("513","514","516","517") ~ "Master",
           degree %in% c("601","602","603","604") ~ "PhD",
           str_sub(degree,1,1)=="9"| degree %in% c("515","510","520","310","605","606") ~ "Other",
           TRUE ~ NA_character_)), 
           "Elemantry","Secondary","HighSchool","Diploma","College","Bachelor","Master","PhD"))

U87P1 <- U87P1 |> 
  rename(
    member = DYCOL01,
    relation = DYCOL03,
    gender = DYCOL04,
    age = DYCOL05,
    literacy = DYCOL06,
    studying = DYCOL07,
    degree = DYCOL08,
    occupationalst = DYCOL09,
    maritalst = DYCOL10) |> 
  mutate(across(where(is.character), as.integer),
         across(c(relation,gender,literacy,studying,degree,occupationalst,maritalst), as.factor),
         relation = fct_recode(relation, !!!relation), 
         gender = fct_recode(gender, !!!gender),
         literacy = fct_recode(literacy, !!!literacy), 
         studying = fct_recode(studying, !!!yesno),
         occupationalst = fct_recode(occupationalst, !!!occupation),
         maritalst = fct_recode(maritalst, !!!marital),
         education = fct_relevel(as.factor(case_when(
           str_sub(degree,1,1)=="1"| ( studying == "No" & degree %in% c("210","211","212","213") ) ~ "Elemantry",
           ( degree %in% c("214","215","216","217","220","221","222") ) | 
             ( studying == "No" & degree %in% c("311","312","321","323","31A","31a","32A","32a","316","325","31B","32B","31b","32b","318","327") ) | 
             (str_sub(degree,1,1)=="2" & studying == "Yes") ~ "Secondary", 
           ( studying == "Yes" & degree %in% c("311","312","321","323","31A","31a","32A","32a","316","325","31B","32B","31b","32b","318","327") ) ~ "HighSchool", 
           degree %in% c("313","314","315","322","324","300","301","317","326","302","319","328","303") ~ "Diploma", 
           degree %in% c("521","522") ~ "College",
           degree %in% c("511","512") ~ "Bachelor",
           degree %in% c("513","514","516","517") ~ "Master",
           degree %in% c("601","602","603","604") ~ "PhD",
           str_sub(degree,1,1)=="9"| degree %in% c("515","510","520","310","605","606") ~ "Other",
           TRUE ~ NA_character_)), 
           "Elemantry","Secondary","HighSchool","Diploma","College","Bachelor","Master","PhD"))


#######################################################################
# Part 2
tenure <- c(OwnedEstateLand="1", OwnedEstate="2", Rent="3", Mortgage="4", Service="5", Free="6", Other="7")
material <- c(MetalBlock="1", BrickWood="2", Cement="3", Brick="4", Wood="5", WoodKesht="6", KeshtGel="7", Other="8")
fuel <- c(Oil="1", Gasoline="2", LiquidGas="3", NaturalGas="4", Electricity="5", Wood="6", AnimalOil="7", Coke="8", Other="9", None="10" )
fuel1 <- c(Oil="11", Gasoline="12", LiquidGas="13", NaturalGas="14", Electricity="15", Wood="16", AnimalOil="17", Coke="18", Other="19", None="20" )
fuel2 <- c(Oil="21", Gasoline="22", LiquidGas="23", NaturalGas="24", Electricity="25", Wood="26", AnimalOil="27", Coke="28", Other="29", None="30" )

R87P2 <- R87P2 |>
  rename(
    tenure = DYCOL01,
    room = DYCOL03,
    space = DYCOL04,
    construction = DYCOL05,
    material = DYCOL06,
    vehicle = DYCOL07,
    motorcycle = DYCOL08,
    bicycle = DYCOL09,
    radio = DYCOL10,
    radiotape = DYCOL11,
    TVbw = DYCOL12,
    TV = DYCOL13,
    VHS_VCD_DVD = DYCOL14,
    computer = DYCOL15,
    cellphone = DYCOL16,
    freezer = DYCOL17,
    refridgerator = DYCOL18,
    fridge = DYCOL19,
    stove = DYCOL20,
    vacuum = DYCOL21,
    washingmachine = DYCOL22,
    sewingmachine = DYCOL23,
    fan = DYCOL24,
    evapcoolingportable = DYCOL25,
    splitportable = DYCOL26,
    dishwasher = DYCOL27,
    none = DYCOL28,
    pipewater = DYCOL29,
    electricity = DYCOL30,
    pipegas = DYCOL31,
    telephone = DYCOL32,
    internet  = DYCOL33,
    bathroom = DYCOL34,
    kitchen = DYCOL35,
    evapcooling = DYCOL36,
    centralcooling = DYCOL37,
    centralheating = DYCOL38,
    package = DYCOL39,
    split = DYCOL40,
    wastewater = DYCOL41,
    cookingfuel = DYCOL42,
    heatingfuel = DYCOL43,
    waterheatingfuel = DYCOL44,
    celebration_m = DYCOL45,
    celebration_y = DYCOL46,
    mourning_m = DYCOL47,
    mourning_y = DYCOL48,
    house_maintenance_m = DYCOL49,
    house_maintenance_y = DYCOL50,
    pilgrimage_m = DYCOL51,
    pilgrimage_y = DYCOL52,
    travel_abroad_m = DYCOL53,
    travel_abroad_y = DYCOL54,
    surgery_m = DYCOL55,
    occasions_other_m = DYCOL56,
    occasions_other_y = DYCOL57,
    none_2 = DYCOL58) |>
  mutate(across(where(is.character), as.integer),
         across(c(tenure,material,cookingfuel,heatingfuel,waterheatingfuel), as.factor),
         tenure = fct_recode(tenure, !!!tenure), 
         material = fct_recode(material, !!!material),
         cookingfuel = fct_recode(cookingfuel, !!!fuel), 
         heatingfuel = fct_recode(heatingfuel, !!!fuel1),
         waterheatingfuel = fct_recode(waterheatingfuel, !!!fuel2),
         across(c(vehicle:wastewater,celebration_m:occasions_other_y), ~!is.na(.x)))

U87P2 <- U87P2 |>
  rename(
    tenure = DYCOL01,
    room = DYCOL03,
    space = DYCOL04,
    construction = DYCOL05,
    material = DYCOL06,
    vehicle = DYCOL07,
    motorcycle = DYCOL08,
    bicycle = DYCOL09,
    radio = DYCOL10,
    radiotape = DYCOL11,
    TVbw = DYCOL12,
    TV = DYCOL13,
    VHS_VCD_DVD = DYCOL14,
    computer = DYCOL15,
    cellphone = DYCOL16,
    freezer = DYCOL17,
    refridgerator = DYCOL18,
    fridge = DYCOL19,
    stove = DYCOL20,
    vacuum = DYCOL21,
    washingmachine = DYCOL22,
    sewingmachine = DYCOL23,
    fan = DYCOL24,
    evapcoolingportable = DYCOL25,
    splitportable = DYCOL26,
    dishwasher = DYCOL27,
    none = DYCOL28,
    pipewater = DYCOL29,
    electricity = DYCOL30,
    pipegas = DYCOL31,
    telephone = DYCOL32,
    internet  = DYCOL33,
    bathroom = DYCOL34,
    kitchen = DYCOL35,
    evapcooling = DYCOL36,
    centralcooling = DYCOL37,
    centralheating = DYCOL38,
    package = DYCOL39,
    split = DYCOL40,
    wastewater = DYCOL41,
    cookingfuel = DYCOL42,
    heatingfuel = DYCOL43,
    waterheatingfuel = DYCOL44,
    celebration_m = DYCOL45,
    celebration_y = DYCOL46,
    mourning_m = DYCOL47,
    mourning_y = DYCOL48,
    house_maintenance_m = DYCOL49,
    house_maintenance_y = DYCOL50,
    pilgrimage_m = DYCOL51,
    pilgrimage_y = DYCOL52,
    travel_abroad_m = DYCOL53,
    travel_abroad_y = DYCOL54,
    surgery_m = DYCOL55,
    occasions_other_m = DYCOL56,
    occasions_other_y = DYCOL57,
    none_2 = DYCOL58) |>
  mutate(across(where(is.character), as.integer),
         across(c(tenure,material,cookingfuel,heatingfuel,waterheatingfuel), as.factor),
         tenure = fct_recode(tenure, !!!tenure), 
         material = fct_recode(material, !!!material),
         cookingfuel = fct_recode(cookingfuel, !!!fuel), 
         heatingfuel = fct_recode(heatingfuel, !!!fuel1),
         waterheatingfuel = fct_recode(waterheatingfuel, !!!fuel2),
         across(c(vehicle:wastewater,celebration_m:occasions_other_y), ~!is.na(.x)))

###################################
# Part 3, Table 1
month <- R87Data |>
  bind_rows(U87Data) |>
  select(Address,month) |>
  mutate(year = 1387)

CPI <- readRDS("CPI.rds")


Global <- readRDS("Global.rds") |>  # Global code for items
  select(Global, code = "C87") |>
  filter(!is.na(code))

GlobalM <- Global |> filter(Global %/% 1e6 == 1)
GlobalY <- Global |> filter(Global %/% 1e6 == 2)

# Part 3, Table 1

R87P3S01 <- R87p3S01 |>
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    gram = DYCOL03,
    kilogram = DYCOL04,
    price = DYCOL05,
    value = DYCOL06) |>
  mutate( 
    across(c(price,value,kilogram),  ~ as.numeric(as.character(.x)) ),
    table = 1L) |> 
  left_join(GlobalM) |>
  left_join(month) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

rm(R87p3S01)

U87P3S01 <- U87P3S01 |>
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    gram = DYCOL03,
    kilogram = DYCOL04,
    price = DYCOL05,
    value = DYCOL06) |>
  mutate( 
    across(c(price,value,kilogram),  ~ as.numeric(as.character(.x)) ),
    table = 1L) |> 
  left_join(GlobalM) |>
  left_join(month) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 2

R87P3S02 <- R87P3S02 |>
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    gram = DYCOL03,
    kilogram = DYCOL04,
    price = DYCOL05,
    value = DYCOL06) |>
  mutate( 
    table = 2L) |> 
  left_join(GlobalM) |>
  left_join(month) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U87P3S02 <- U87P3S02 |>
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    gram = DYCOL03,
    kilogram = DYCOL04,
    price = DYCOL05,
    value = DYCOL06) |>
  mutate( 
    table = 2L) |> 
  left_join(GlobalM) |>
  left_join(month) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 3

R87P3S03 <- R87P3S03 |>
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) |>
  mutate( 
    table = 3L) |> 
  left_join(GlobalM) |>
  left_join(month) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U87P3S03 <- U87P3S03 |>
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) |>
  mutate( 
    table = 3L) |> 
  left_join(GlobalM) |>
  left_join(month) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 4

R87P3S04 <- R87P3S04 |>
  rename(
    code = DYCOL01,
    mortgage = DYCOL02,
    purchased = DYCOL03,
    value = DYCOL04) |>
  mutate( 
    table = 4L) |> 
  left_join(GlobalM) |>
  left_join(month) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)


U87P3S04 <- U87P3S04 |>
  rename(
    code = DYCOL01,
    mortgage = DYCOL02,
    purchased = DYCOL03,
    value = DYCOL04 ) |>
  mutate( 
    value = as.numeric(as.character(value)),
    table = 4L) |> 
  left_join(GlobalM) |>
  left_join(month) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)


# Part 3, Table 5

R87P3S05 <- R87P3S05 |>
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) |>
  mutate( 
    value = as.numeric(as.character(value)),
    table = 5L) |> 
  left_join(GlobalM) |>
  left_join(month) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U87P3S05 <- U87P3S05 |>
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) |>
  mutate( 
    table = 5L) |> 
  left_join(GlobalM) |>
  left_join(month) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 6

R87P3S06 <- R87P3S06 |>
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) |>
  mutate( 
    table = 6L) |> 
  left_join(GlobalM) |>
  left_join(month) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U87P3S06 <- U87P3S06 |>
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) |>
  mutate( 
    table = 6L) |> 
  left_join(GlobalM) |>
  left_join(month) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 7

R87P3S07 <- R87P3S07 |>
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) |>
  mutate(
    table = 7L) |> 
 left_join(GlobalM) |>
  left_join(month) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U87P3S07 <- U87P3S07 |>
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) |>
  mutate(
    table = 7L) |> 
 left_join(GlobalM) |>
  left_join(month) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 8

R87P3S08 <- R87P3S08 |>
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) |>
  mutate( 
          table = 8L) |> 
 left_join(GlobalM) |>
  left_join(month) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)


U87P3S08 <- U87P3S08 |>
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) |>
  mutate( 
          table = 8L) |> 
 left_join(GlobalM) |>
  left_join(month) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 9

R87P3S09 <- R87P3S09 |>
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) |>
  mutate( 
    table = 9L) |> 
  left_join(GlobalM) |>
  left_join(month) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U87P3S09 <- U87P3S09 |>
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) |>
  mutate( 
    table = 9L) |> 
  left_join(GlobalM) |>
  left_join(month) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 10

R87P3S10 <- R87P3S10 |>
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) |>
  mutate(table = 10L) |>
  mutate_if(is.character, as.numeric)

U87P3S10 <- U87P3S10 |>
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) |>
  mutate(table = 10L) |>
  mutate_if(is.character, as.numeric)

# Part 3, Table 11

R87P3S11 <- R87P3S11 |>
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) |>
  mutate(
         table = 11L) |> 
  left_join(GlobalM) |>
  left_join(month) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U87P3S11 <- U87P3S11 |>
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) |>
  mutate(
         table = 11L) |> 
  left_join(GlobalM) |>
  left_join(month) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 12

R87P3S12 <- R87P3S12 |>
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) |>
  mutate( 
         table = 12L) |> 
  left_join(GlobalM) |>
  left_join(month) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U87P3S12 <- U87P3S12 |>
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) |>
  mutate( 
         table = 12L) |> 
  left_join(GlobalM) |>
  left_join(month) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 13

R87P3S13 <- R87P3S13 |>
  rename(
    code = DYCOL01,
    insured_loan = DYCOL02,
    purchased = DYCOL03,
    cost = DYCOL04,
    sell = DYCOL05) |>
  mutate( 
    cost = replace_na(as.numeric(cost),0),
    sell = replace_na(as.numeric(sell),0),
    value = cost - sell,
    table = 13L) |> 
  left_join(GlobalY) |>
  left_join(month) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi_y) |>
  select(Address:Global, value_r)

U87P3S13 <- U87P3S13 |>
  rename(
    code = DYCOL01,
    insured_loan = DYCOL02,
    purchased = DYCOL03,
    cost = DYCOL04,
    sell = DYCOL05) |>
  mutate( 
    cost = replace_na(as.numeric(as.character(cost)),0),
    sell = replace_na(as.numeric(as.character(sell)),0),
    value = cost - sell,
    table = 13L) |> 
  left_join(GlobalY) |>
  left_join(month) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi_y) |>
  select(Address:Global, value_r)

# Part 3, Table 14

R87P3S14<- R87P3S14 |>
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    cost = DYCOL03,
    sell = DYCOL04) |>
  mutate(
    cost = replace_na(as.numeric(as.character(cost)),0),
    sell = replace_na(as.numeric(as.character(sell)),0),
    value = cost - sell,
         table = 14L) |> 
  left_join(GlobalY) |>
  left_join(month) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi_y) |>
  select(Address:Global, value_r)

U87P3S14 <- U87P3S14 |>
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    cost = DYCOL03,
    sell = DYCOL04) |>
  mutate(
         cost = replace_na(as.numeric(cost),0),
         sell = replace_na(as.numeric(sell),0),
         value = cost - sell,
         table = 14L) |> 
  left_join(GlobalY) |>
  left_join(month) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi_y) |>
  select(Address:Global, value_r)


list2env(lapply(mget(ls(pattern = "P3S.*")),
                function(x) {x |> mutate(purchased=factor(purchased,
                                                   levels = c(1,2,3,4,5,6,7,8),
                                                   labels = c("purchased",
                                                              "homemade",
                                                              "publicservice",
                                                              "cooperativeservice",
                                                              "privateservice",
                                                              "agriculture",
                                                              "nonagriculture",
                                                              "free")))} )
         , .GlobalEnv)


#############################
# Part 4, Table 1

R87P4S01$DYCOL09 <- as.numeric(levels(R87P4S01$DYCOL09))[R87P4S01$DYCOL09]
R87P4S01 <- R87P4S01 |>
  rename(
    member = DYCOL01,
    employed_w = DYCOL02,
    ISCO_w = DYCOL03,
    ISIC_w = DYCOL04,
    status_w = DYCOL05,
    hours_w = DYCOL06,
    days_w = DYCOL07,
    income_w_m = DYCOL08,
    income_w_y = DYCOL09,
    wage_w_m = DYCOL10,
    wage_w_y = DYCOL11,
    perk_w_m = DYCOL12,
    perk_w_y = DYCOL13,
    netincome_w_m = DYCOL14,
    netincome_w_y = DYCOL15) |>
  left_join(month) |>
  mutate(Global = 1000000) |>
  left_join(CPI) |>
  select(Address:netincome_w_y, cpi_m = cpi, cpi_y)

U87P4S01$DYCOL04 <- as.numeric(levels(U87P4S01$DYCOL04))[U87P4S01$DYCOL04]
U87P4S01$DYCOL15 <- as.numeric(levels(U87P4S01$DYCOL15))[U87P4S01$DYCOL15]

U87P4S01 <- U87P4S01 |>
  rename(
    member = DYCOL01,
    employed_w = DYCOL02,
    ISCO_w = DYCOL03,
    ISIC_w = DYCOL04,
    status_w = DYCOL05,
    hours_w = DYCOL06,
    days_w = DYCOL07,
    income_w_m = DYCOL08,
    income_w_y = DYCOL09,
    wage_w_m = DYCOL10,
    wage_w_y = DYCOL11,
    perk_w_m = DYCOL12,
    perk_w_y = DYCOL13,
    netincome_w_m = DYCOL14,
    netincome_w_y = DYCOL15) |>
  left_join(month) |>
  mutate(Global = 1000000) |>
  left_join(CPI) |>
  select(Address:netincome_w_y, cpi_m = cpi, cpi_y)

R87P4S02$DYCOL15 <- as.numeric(levels(R87P4S02$DYCOL15))[R87P4S02$DYCOL15]

# Part 4, Table 2
R87P4S02 <- R87P4S02 |>
  rename(
    member = DYCOL01,
    employed_s = DYCOL02,
    ISCO_s = DYCOL03,
    ISIC_s = DYCOL04,
    status_s = DYCOL05,
    agriculture = DYCOL06,
    hours_s = DYCOL07,
    days_s = DYCOL08,
    cost_employment = DYCOL09,
    cost_raw = DYCOL10,
    cost_machinery = DYCOL11,
    cost_others = DYCOL12,
    cost_tax = DYCOL13,
    sale = DYCOL14,
    income_s_y = DYCOL15) |>
  left_join(month) |>
  mutate(Global = 1000000) |>
  left_join(CPI) |>
  select(Address:income_s_y, cpi_m = cpi, cpi_y)

U87P4S02$DYCOL04 <- as.numeric(levels(U87P4S02$DYCOL04))[U87P4S02$DYCOL04]

U87P4S02 <- U87P4S02 |>
  rename(
    member = DYCOL01,
    employed_s = DYCOL02,
    ISCO_s = DYCOL03,
    ISIC_s = DYCOL04,
    status_s = DYCOL05,
    agriculture = DYCOL06,
    hours_s = DYCOL07,
    days_s = DYCOL08,
    cost_employment = DYCOL09,
    cost_raw = DYCOL10,
    cost_machinery = DYCOL11,
    cost_others = DYCOL12,
    cost_tax = DYCOL13,
    sale = DYCOL14,
    income_s_y = DYCOL15) |>
  left_join(month) |>
  mutate(Global = 1000000) |>
  left_join(CPI) |>
  select(Address:income_s_y, cpi_m = cpi, cpi_y)

# Part 4, Table 3
R87P4S03 <- R87P4S03 |>
  rename(
    member = DYCOL01,
    income_pension = DYCOL03,
    income_rent = DYCOL04,
    income_interest = DYCOL05,
    income_aid = DYCOL06,
    income_resale = DYCOL07,
    income_transfer = DYCOL08) |>
  left_join(month) |>
  mutate(Global = 1000000) |>
  left_join(CPI) |>
  select(Address:income_transfer, cpi_y)

U87P4S03 <- U87P4S03 |>
  rename(
    member = DYCOL01,
    income_pension = DYCOL03,
    income_rent = DYCOL04,
    income_interest = DYCOL05,
    income_aid = DYCOL06,
    income_resale = DYCOL07,
    income_transfer = DYCOL08) |>
  left_join(month) |>
  mutate(Global = 1000000) |>
  left_join(CPI) |>
  select(Address:income_transfer, cpi_y)

rm(list = setdiff(ls(), ls(pattern = "87"))) # removing unnecessary objects

save.image(file="./exported/HEIS87.Rdata")

month <- R87Data |>
  bind_rows(U87Data) |>
  select(Address,month) |>
  mutate(year=1387)

CPI <- readRDS("CPI.rds")

###############################################
# Item-level expenditure table
# Merging tables in a unique expenditure table

R87P3 <- bind_rows(mget(ls(pattern = "R87P3S.*")))
R87P3 <- R87P3 |>
  left_join(R87Data) |>
  mutate(urban = "R")

U87P3 <-  bind_rows(mget(ls(pattern = "U87P3S.*")))
U87P3 <- U87P3 |>
  left_join(U87Data) |>
  mutate(urban = "U")

itemcode <- read_excel("Codes.xlsx", sheet = "itemLabel") |> select(-labelFA)

itemcode$item <- factor(itemcode$Global, levels = itemcode$Global, labels = itemcode$labelEN)

EXP87 <- bind_rows(R87P3,U87P3) |>
  mutate(urban = as.factor(urban), 
         recallperiod=ifelse(Global %/% 1e6==2,1/12,1), 
         table = as.integer(str_sub(as.character(Global),2,3)), 
         Table = case_when(
           table == 1 ~ "food",
           table == 2 ~ "tobacco",
           table == 3 ~ "clothing",
           table == 4 ~ "housing",
           table == 5 ~ "appliances",
           table == 6 ~ "health",
           table == 7 ~ "transport",
           table == 8 ~ "communication",
           table == 9 ~ "recreation",
           table == 10 ~ "education",
           table == 11 ~ "restaurant",
           table == 12 ~ "miscellaneous",
           table == 13 ~ "transfers",
           table == 14 ~ "investment",
           TRUE ~ NA_character_)) |>
  group_by(Table, Global, code, urban) |>
  summarize(Value = sum(value*weight*recallperiod, na.rm = T),
            Value_r = sum(value_r*weight*recallperiod, na.rm = T),
            Kilogram = sum(kilogram*weight, na.rm = T),
            Gram = sum(gram*weight, na.rm = T),
            Price = median(price, na.rm = T)) |>
  filter(Value!=0) |>
  left_join(itemcode) |>
  select(-labelEN) |>
  as.data.frame()


attr(EXP87$Table, "label") <- "Table in Part 3"
attr(EXP87$Global, "label") <- "global item code"
attr(EXP87$code, "label") <- "item code in this year"
attr(EXP87$urban, "label") <- "rural or urban"
attr(EXP87$Price, "label") <- "median price"
attr(EXP87$Value, "label") <- "monthly expenditure"
attr(EXP87$Value_r, "label") <- "monthly expenditure in 1390 price"

saveRDS(EXP87, "./exported/EXP87.Rds")

###############################################
# Building individual level data

r87data<- R87Data |> select(Address, month, weight, khanevartype, Jaygozin, province)
u87data<- U87Data |> select(Address, month, weight, khanevartype, Jaygozin, province)


# faster code with data.table
DT <- data.table(R87P4S01)
DT <- DT[,.('employed_w'= first(employed_w),
            'ISCO_w'= first(ISCO_w),
            'ISIC_w'= first(ISIC_w),
            'status_w'= first(status_w),
            'hours_w'= sum(hours_w, na.rm = T),
            'days_w'= sum(days_w, na.rm = T),
            'income_w_m'= sum(income_w_m, na.rm = T),
            'income_w_y'= sum(income_w_y, na.rm = T),
            'wage_w_m'= sum(wage_w_m, na.rm = T),
            'wage_w_y'= sum(wage_w_y, na.rm = T),
            'perk_w_m'= sum(perk_w_m, na.rm = T),
            'perk_w_y'= sum(perk_w_y, na.rm = T),
            'netincome_w_m'= sum(netincome_w_m, na.rm = T),
            'netincome_w_y'= sum(netincome_w_y, na.rm = T)),
         by=.(Address, member)]
R87P4S01_unique <- as.data.frame(DT) |>
  mutate(across(c(hours_w,days_w), ~ifelse(.x == 0,NA,.x)))

DT <- data.table(R87P4S02)
DT <- DT[,.('employed_s'= first(employed_s),
            'ISCO_s'= first(ISCO_s),
            'ISIC_s'= first(ISIC_s),
            'status_s'= first(status_s),
            'agriculture'= first(agriculture),
            'hours_s'= sum(hours_s, na.rm = T),
            'days_s'= sum(days_s, na.rm = T),
            'income_s_y'= sum(income_s_y, na.rm = T),
            'cost_employment'= sum(cost_employment, na.rm = T),
            'cost_raw'= sum(cost_raw, na.rm = T),
            'cost_machinery'= sum(cost_machinery, na.rm = T),
            'cost_others'= sum(cost_others, na.rm = T),
            'cost_tax'= sum(cost_tax, na.rm = T),
            'sale'= sum(sale, na.rm = T)),
         by=.(Address, member)]

R87P4S02_unique <- as.data.frame(DT) |>
  mutate(across(c(hours_s,days_s), ~ifelse(.x == 0,NA,.x)))

DT <- data.table(R87P4S03)
DT <- DT[,.('income_pension'= sum(income_pension, na.rm = T),
            'income_rent'= sum(income_rent, na.rm = T),
            'income_interest'= sum(income_interest, na.rm = T),
            'income_aid'= sum(income_aid, na.rm = T),
            'income_resale'= sum(income_resale, na.rm = T),
            'income_transfer'= sum(income_transfer, na.rm = T)),
         by=.(Address, member)]
R87P4S03_unique <- as.data.frame(DT) 


DT <- data.table(U87P4S01)
DT <- DT[,.('employed_w'= first(employed_w),
            'ISCO_w'= first(ISCO_w),
            'ISIC_w'= first(ISIC_w),
            'status_w'= first(status_w),
            'hours_w'= sum(hours_w, na.rm = T),
            'days_w'= sum(days_w, na.rm = T),
            'income_w_m'= sum(income_w_m, na.rm = T),
            'income_w_y'= sum(income_w_y, na.rm = T),
            'wage_w_m'= sum(wage_w_m, na.rm = T),
            'wage_w_y'= sum(wage_w_y, na.rm = T),
            'perk_w_m'= sum(perk_w_m, na.rm = T),
            'perk_w_y'= sum(perk_w_y, na.rm = T),
            'netincome_w_m'= sum(netincome_w_m, na.rm = T),
            'netincome_w_y'= sum(netincome_w_y, na.rm = T)),
         by=.(Address, member)]
U87P4S01_unique <- as.data.frame(DT) |>
  mutate(across(c(hours_w,days_w), ~ifelse(.x == 0,NA,.x)))


DT <- data.table(U87P4S02)
DT <- DT[,.('employed_s'= first(employed_s),
            'ISCO_s'= first(ISCO_s),
            'ISIC_s'= first(ISIC_s),
            'status_s'= first(status_s),
            'agriculture'= first(agriculture),
            'hours_s'= sum(hours_s, na.rm = T),
            'days_s'= sum(days_s, na.rm = T),
            'income_s_y'= sum(income_s_y, na.rm = T),
            'cost_employment'= sum(cost_employment, na.rm = T),
            'cost_raw'= sum(cost_raw, na.rm = T),
            'cost_machinery'= sum(cost_machinery, na.rm = T),
            'cost_others'= sum(cost_others, na.rm = T),
            'cost_tax'= sum(cost_tax, na.rm = T),
            'sale'= sum(sale, na.rm = T)),
         by=.(Address, member)]
U87P4S02_unique <- as.data.frame(DT) |>
  mutate(across(c(hours_s,days_s), ~ifelse(.x == 0,NA,.x)))

DT <- data.table(U87P4S03)
DT <- DT[,.('income_pension'= sum(income_pension, na.rm = T),
            'income_rent'= sum(income_rent, na.rm = T),
            'income_interest'= sum(income_interest, na.rm = T),
            'income_aid'= sum(income_aid, na.rm = T),
            'income_resale'= sum(income_resale, na.rm = T),
            'income_transfer'= sum(income_transfer, na.rm = T)),
         by=.(Address, member)]
U87P4S03_unique <- as.data.frame(DT) 


Rind87 <- r87data  |>
  left_join(select(R87P1,-degree)) |>
  left_join(R87P4S01_unique) |>
  left_join(R87P4S02_unique) |>
  left_join(R87P4S03_unique) |> 
  mutate(across(where(is.character),as.integer)) |>
  mutate(urban = "R")

Uind87 <- u87data |>
  left_join(select(U87P1,-degree)) |>
  left_join(U87P4S01_unique) |>
  left_join(U87P4S02_unique) |>
  left_join(U87P4S03_unique) |> 
  mutate(across(where(is.character),as.integer)) |>
  mutate(urban = "U")

IND87 <- bind_rows(Rind87,Uind87) |>
  mutate(urban = as.factor(urban),
         Global = 1000000 ,
         employed_w = factor(employed_w, levels = c(1,2), labels = c("Yes","No")),
         status_w = factor(status_w, levels = c(1,2,3), labels = c("public","cooperative","private")),
         employed_s = factor(employed_s, levels = c(1,2), labels = c("Yes","No")),
         status_s = factor(status_s, levels = c(1,2,3), labels = c("employer","selfemployed","familyworker")),
         agriculture = factor(agriculture, levels = c(1,2), labels = c("agriculture","nonagriculture"))
  ) |>
  left_join(month) |>
  left_join(CPI) |>
  select(Address:urban, cpi_m = cpi, cpi_y) |>
  as.data.frame()

attr(IND87$occupationalst, "label") <- "employment status"
attr(IND87$employed_w, "label") <- "Whether employed in wage-earning job?"
attr(IND87$ISIC_w, "label") <- "Industry code of wage-earning job"
attr(IND87$ISCO_w, "label") <- "Occupation code of wage-earning job"
attr(IND87$status_w, "label") <- "Wage-earning job status: 1-public 2-cooperative 3-private"
attr(IND87$hours_w, "label") <- "Wage-earning job: hours per day"
attr(IND87$days_w, "label") <- "Wage-earning job: day per week"
attr(IND87$netincome_w_m, "label") <- "wage-earning job: net income previous month"
attr(IND87$netincome_w_y, "label") <- "wage earning job: net income previous year"
attr(IND87$employed_s, "label") <- "Whether employed in non-wage job?"
attr(IND87$ISIC_s, "label") <- "Industry code of non-wage job"
attr(IND87$ISCO_s, "label") <- "Occupation code of non-wage job"
attr(IND87$status_s, "label") <- "job status: 4-employer 2-selfemployed 3-familyworker"
attr(IND87$hours_s, "label") <- "non-wage job: hours per day"
attr(IND87$days_s, "label") <- "non-wage job: day per week"
attr(IND87$income_s_y, "label") <- "non-wage job: net income previous year"

saveRDS(IND87,"./exported/IND87.Rds")

###############################################
# Building Household Data

r87p1 <- R87P1 |>
  group_by(Address) |>
  summarize(size = sum(!is.na(member)),
            literates = sum(literacy == "literate", na.rm = TRUE),
            students = sum(studying == "Yes", na.rm = TRUE),
            employeds = sum(occupationalst == "employed", na.rm = TRUE)) 

u87p1 <- U87P1 |>
  group_by(Address) |>
  summarize(size = sum(!is.na(member)),
            literates = sum(literacy == "literate", na.rm = TRUE),
            students = sum(studying == "Yes", na.rm = TRUE),
            employeds = sum(occupationalst == "employed", na.rm = TRUE)) 

r_head <- R87P1 |>
  filter(relation == "head") |>
  select(-member,-relation,-studying)

u_head <- U87P1 |>
  filter(relation == "head") |>
  select(-member,-relation,-studying)

# summary of heads job codes
r_job <- Rind87 |>
  filter(relation == "head") |>
  select(Address,starts_with("IS"))

u_job <- Uind87 |>
  filter(relation == "head") |>
  select(Address,starts_with("IS"))

# Sum of household expenditure items
r87p3 <- R87P3 |>
  mutate(nominal_gross = value + sell,
         real_gross = nominal_gross * value_r/value) |>
  select(Address, table, nominal_net="value", real_net="value_r", ends_with("gross")) |>
  pivot_longer(cols = nominal_net:real_gross,
               names_to = c(".value","Net"),
               names_pattern = "(.*)_(.*)",
               values_drop_na = TRUE) |>
  mutate(Table = case_when(
    table == 1 ~ "food",
    table == 2 ~ "tobacco",
    table == 3 ~ "clothing",
    table == 4 ~ "housing",
    table == 5 ~ "appliances",
    table == 6 ~ "health",
    table == 7 ~ "transport",
    table == 8 ~ "communication",
    table == 9 ~ "recreation",
    table == 11 ~ "restaurant",
    table == 12 ~ "miscellaneous",
    table == 13 & Net =="net" ~ "durables",
    table == 14 & Net =="net" ~ "investment",
    table == 13 & Net =="gross" ~ "durables_gross",
    table == 14 & Net =="gross" ~ "investment_gross",
    TRUE ~ NA_character_)) |>
  group_by(Address,Table) |>
  summarize(cost = sum(nominal, na.rm = TRUE),
            cost_r = sum(real, na.rm = TRUE)) |>
  pivot_wider(Address, 
              names_from = "Table", 
              values_from = c("cost","cost_r"), 
              values_fill = list(cost = 0, cost_r = 0))

u87p3 <- U87P3 |>
  mutate(nominal_gross = value + sell,
         real_gross = nominal_gross * value_r/value) |>
  select(Address, table, nominal_net="value", real_net="value_r", ends_with("gross")) |>
  pivot_longer(cols = nominal_net:real_gross,
               names_to = c(".value","Net"),
               names_pattern = "(.*)_(.*)",
               values_drop_na = TRUE) |>
  mutate(Table = case_when(
    table == 1 ~ "food",
    table == 2 ~ "tobacco",
    table == 3 ~ "clothing",
    table == 4 ~ "housing",
    table == 5 ~ "appliances",
    table == 6 ~ "health",
    table == 7 ~ "transport",
    table == 8 ~ "communication",
    table == 9 ~ "recreation",
    table == 11 ~ "restaurant",
    table == 12 ~ "miscellaneous",
    table == 13 & Net =="net" ~ "durables",
    table == 14 & Net =="net" ~ "investment",
    table == 13 & Net =="gross" ~ "durables_gross",
    table == 14 & Net =="gross" ~ "investment_gross",
    TRUE ~ NA_character_)) |>
  group_by(Address,Table) |>
  summarize(cost = sum(nominal, na.rm = TRUE),
            cost_r = sum(real, na.rm = TRUE)) |>
  pivot_wider(Address, 
              names_from = "Table", 
              values_from = c("cost","cost_r"), 
              values_fill = list(cost = 0, cost_r = 0))

# Non-monetary household income
r_NM_housing <- R87P3S04 |>
  filter(Global %in% c(1042111,1042211,1042212) ) |>
  group_by(Address) |>
  summarize(income_nm_house = sum(value*12, na.rm = T))

u_NM_housing <- U87P3S04 |>
  filter(Global %in% c(1042111,1042211,1042212) ) |>
  group_by(Address) |>
  summarize(income_nm_house = sum(value*12, na.rm = T))

r_NMincome <- R87P3 |>
  filter(table<14) |> # investment is not a part of nonmonetary income
  mutate(value = ifelse(table==13, cost, value), # no second hand purchase here
         type = case_when(
    purchased %in% c("publicservice","cooperativeservice") ~ "public",
    purchased == "privateservice" ~ "private",
    purchased == "agriculture" ~ "agriculture",
    purchased == "nonagriculture" ~ "nonagriculture", 
    purchased %in% c("free","homemade") ~ as.character(purchased),
    TRUE ~ NA_character_),
    recallperiod=ifelse(table>12,1,12)) |>
  group_by(Address, type) |>
  summarize(value = sum(value*recallperiod, na.rm = T)) |>
  filter(!is.na(type)&value!=0) |> 
  pivot_wider(Address, 
              names_from="type", names_prefix = "income_nm_", 
              values_from = "value",  values_fill = list(value = 0)) |>
  full_join(r_NM_housing) 
r_NMincome[is.na(r_NMincome)] <- 0

u_NMincome <- U87P3 |>
  filter(table<14) |> # investment is not a part of nonmonetary income
  mutate(value = ifelse(table==13, cost, value), # no second hand purchase here
         type = case_when(
    purchased %in% c("publicservice","cooperativeservice") ~ "public",
    purchased == "privateservice" ~ "private",
    purchased == "agriculture" ~ "agriculture",
    purchased == "nonagriculture" ~ "nonagriculture", 
    purchased %in% c("free","homemade") ~ as.character(purchased),
    TRUE ~ NA_character_),
    recallperiod=ifelse(table>12,1,12)) |>
  group_by(Address, type) |>
  summarize(value = sum(value*recallperiod, na.rm = T)) |>
  filter(!is.na(type)&value!=0) |> 
  pivot_wider(Address, 
              names_from="type", names_prefix = "income_nm_", 
              values_from = "value", values_fill = list(value = 0)) |>
  full_join(u_NM_housing) 
u_NMincome[is.na(u_NMincome)] <- 0


# sum of household income with data.table
names(Rind87)
DT <- data.table(Rind87)
DT <- DT[,.('income_w_y'= sum(income_w_y, na.rm = T),
            'income_s_y'= sum(income_s_y, na.rm = T),
            'netincome_w_m'= sum(netincome_w_m, na.rm = T),
            'netincome_w_y'= sum(netincome_w_y, na.rm = T),
            'income_pension'= sum(income_pension, na.rm = T),
            'income_rent'= sum(income_rent, na.rm = T),
            'income_interest'= sum(income_interest, na.rm = T),
            'income_aid'= sum(income_aid, na.rm = T),
            'income_resale'= sum(income_resale, na.rm = T),
            'income_transfer'= sum(income_transfer, na.rm = T)),
         by=.(Address)]
r_incomeSum <- as.data.frame(DT) 

DT <- data.table(Uind87)
DT <- DT[,.('income_w_y'= sum(income_w_y, na.rm = T),
            'income_s_y'= sum(income_s_y, na.rm = T),
            'netincome_w_m'= sum(netincome_w_m, na.rm = T),
            'netincome_w_y'= sum(netincome_w_y, na.rm = T),
            'income_pension'= sum(income_pension, na.rm = T),
            'income_rent'= sum(income_rent, na.rm = T),
            'income_interest'= sum(income_interest, na.rm = T),
            'income_aid'= sum(income_aid, na.rm = T),
            'income_resale'= sum(income_resale, na.rm = T),
            'income_transfer'= sum(income_transfer, na.rm = T)),
         by=.(Address)]
u_incomeSum <- as.data.frame(DT)

# merging household-level data
RHH87 <- r87data |> 
  mutate(urban = "R") |>
  left_join(r87p1, by="Address") |>
  left_join(r_head, by = "Address") |>
  left_join(r_job, by = "Address") |>
  left_join(r87p3, by = "Address") |>
  left_join(r_incomeSum, by = "Address") |>
  left_join(r_NMincome, by = "Address") |>
  left_join(select(R87P2,-DYCOL02)) |>
  mutate(across(income_w_y:income_nm_house, ~replace_na(.x, 0)))

UHH87 <- u87data |> 
  mutate(urban = "U") |>
  left_join(u87p1, by="Address") |>
  left_join(u_head, by = "Address") |>
  left_join(u_job, by = "Address") |>
  left_join(u87p3, by = "Address") |>
  left_join(u_incomeSum, by = "Address") |>
  left_join(u_NMincome, by = "Address") |>
  left_join(select(U87P2,-DYCOL02)) |>
  mutate(across(income_w_y:income_nm_house, ~replace_na(.x, 0)))

HH87 <- bind_rows(RHH87, UHH87) |>
  mutate(urban = as.factor(urban)) |>
  mutate(expenditure = cost_food + cost_tobacco + cost_clothing + cost_housing + cost_appliances + cost_health + cost_transport + cost_communication + cost_recreation +  cost_restaurant + cost_miscellaneous
         + cost_durables/12,
         expenditure_gross = cost_food + cost_tobacco + cost_clothing + cost_housing + cost_appliances + cost_health + cost_transport + cost_communication + cost_recreation +  cost_restaurant + cost_miscellaneous
         + cost_durables_gross/12,
         income = income_s_y + netincome_w_y + income_pension + income_rent + income_interest + income_aid + income_resale + income_transfer + income_nm_agriculture + income_nm_free + income_nm_homemade + income_nm_public + income_nm_private + income_nm_nonagriculture + income_nm_house,
         Global = 1000000 ) |>
  left_join(month) |>
  left_join(CPI) |>
  select(Address:income, cpi_m = cpi, cpi_y) |>
  as.data.frame()

attr(HH87$size, "label") <- "Household size"
attr(HH87$literates, "label") <- "Number of literate members"
attr(HH87$students, "label") <- "Number of student members"
attr(HH87$employeds, "label") <- "Number of employed members"
attr(HH87$gender, "label") <- "Head's gender"
attr(HH87$age, "label") <- "Head's age"
attr(HH87$literacy, "label") <- "Head's literacy"
attr(HH87$education, "label") <- "Head's education"
attr(HH87$occupationalst, "label") <- "Head's job status"
attr(HH87$maritalst, "label") <- "Head's marital status"
attr(HH87$ISIC_w, "label") <- "Head's Industry code of wage-earning job"
attr(HH87$ISCO_w, "label") <- "Head's Occupation code of wage-earning job"
attr(HH87$ISIC_s, "label") <- "Head's Industry code of non-wage job"
attr(HH87$ISCO_s, "label") <- "Head's Occupation code of non-wage job"
attr(HH87$income_nm_agriculture, "label") <- "Non-monetary income from agriculture"
attr(HH87$income_nm_nonagriculture, "label") <- "Non-monetary income from nonagriculture"
attr(HH87$income_nm_public, "label") <- "Non-monetary income from public sector job"
attr(HH87$income_nm_private, "label") <- "Non-monetary income from private sector job"
attr(HH87$income_nm_homemade, "label") <- "Non-monetary income from home production"
attr(HH87$netincome_w_m, "label") <- "wage-earning job: net income previous month"
attr(HH87$netincome_w_y, "label") <- "wage-earning job: net income previous year"
attr(HH87$income_w_y, "label") <- "wage-earning job: gross income previous year"
attr(HH87$income_s_y, "label") <- "non-wage job: net income previous year"
attr(HH87$income, "label") <- "total household income in previous year"
attr(HH87$expenditure, "label") <- "total monthly expendiuture of the household"

saveRDS(HH87, file = "./exported/HH87.Rds")


#######################################
# Exporting Rds files to CSV, STATA, etc.

#install.packages("haven") # uncomment if not already installed

df <- readRDS("./exported/HH87.Rds") # Specify the Rds file to convert here

#haven::write_dta(df, "./exported/HH87.dta") # to export in STATA
#write_csv2(df, "./exported/HH87.csv") # to export in CSV
