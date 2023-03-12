library(tidyverse)
library(data.table)
library(readxl)

rm(list = ls())

load("./RAW/85.RData")

##############################
Province <- c(Markazi="00", Gilan="01", Mazandaran="02", AzarbaijanSharghi="03", AzarbaijanGharbi="04",
              Kermanshah="05", Kouzestan="06", Fars="07", Kerman="08", KhorasanRazavi="09",
              Esfahan="10", SistanBalouchestan="11", Kordestan="12", Hamedan="13", CharmahalBakhtiari="14",
              Lorestan="15", Ilam="16", KohkilouyeBoyerahamad="17", Boushehr="18", Zanjan="19", 
              Semnan="20", Yazd="21", Hormozgan="22", Tehran="23", Ardebil="24", Qom="25", Qazvin="26",
              Golestan="27", KhorasanShomali="28", KhorasanJonoubi="29")


R85Data <- Sum_R85 |> 
  select(ADDRESS, weight) |>
  rename(Address = ADDRESS) |>
  mutate(province = fct_recode(as.factor(substr(Address, 2, 3)), !!!Province),
         town = as.integer(substr(Address, 4, 5))) |>
  mutate_if(is.character, as.numeric)


U85Data <- Sum_U85 |> 
  select(ADDRESS, weight) |>
  rename(Address = ADDRESS) |>
  mutate(province = fct_recode(as.factor(substr(Address, 2, 3)), !!!Province),
         town = as.integer(substr(Address, 4, 5))) |>
  mutate_if(is.character, as.numeric)

##############################
# Part 1

relation <- c(head="1", spouse="2", child="3", childinlaw="4", grandchild="5", parent="6", sibling="7", relative="8", nonrelative="9")
gender <- c(Male="1", Female="2")
literacy <- c(literate="1", illiterate="2", NULL="", NULL="-", NULL=" ")
yesno <- c(Yes="1", No="2", NULL="", NULL="-", NULL=" ")
occupation <- c(employed="1", unemployed="2", IncomeWOJob="3", Student="4", Housewife="5", Other="6")
marital <- c(Married ="1", Widowed="2", Divorced="3", Single="4")

R85P1 <- R85P1 |> 
  rename(
    Address = ADDRESS,
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

U85P1 <- U85P1 |> 
  rename(
    Address = ADDRESS,
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
           "Elemantry","Secondary","HighSchool","Diploma","College","Bachelor","Master","PhD")
  )

#######################################################################
# Part 2
tenure <- c(OwnedEstateLand="1", OwnedEstate="2", Rent="3", Mortgage="4", Service="5", Free="6", Other="7")
material <- c(MetalBlock="1", BrickWood="2", Cement="3", Brick="4", Wood="5", WoodKesht="6", KeshtGel="7", Other="8")
fuel <- c(Oil="1", Gasoline="2", NaturalGas="3", Electricity="4", Wood="5", AnimalOil="6", Coke="7", Other="8" )
fuel1 <- c(Oil="9", Gasoline="10", NaturalGas="11", Electricity="12", Wood="13", AnimalOil="14", Coke="15", Other="16" )


# Part 2

R85P2 <- R85P2 |>
  rename(
    Address = ADDRESS,
    tenure = DYCOL01,
    room = DYCOL02,
    space = DYCOL03,
    construction = DYCOL04,
    material = DYCOL05,
    vehicle = DYCOL06,
    motorcycle = DYCOL07,
    bicycle = DYCOL08,
    radio = DYCOL09,
    radiotape = DYCOL10,
    TVbw = DYCOL11,
    TV = DYCOL12,
    VHS_VCD_DVD = DYCOL13,
    computer = DYCOL14,
    cellphone = DYCOL15,
    freezer = DYCOL16,
    refridgerator = DYCOL17,
    fridge = DYCOL18,
    stove = DYCOL19,
    vacuum = DYCOL20,
    washingmachine = DYCOL21,
    sewingmachine = DYCOL22,
    fan = DYCOL23,
    evapcoolingportable = DYCOL24,
    splitportable = DYCOL25,
    dishwasher = DYCOL26,
    none = DYCOL27,
    pipewater = DYCOL28,
    electricity = DYCOL29,
    pipegas = DYCOL30,
    telephone = DYCOL31,
    internet  = DYCOL32,
    bathroom = DYCOL33,
    kitchen = DYCOL34,
    evapcooling = DYCOL35,
    centralcooling = DYCOL36,
    centralheating = DYCOL37,
    package = DYCOL38,
    split = DYCOL39,
    #wastewater = DYCOL40,
    cookingfuel = DYCOL40,
    heatingfuel = DYCOL41,
    #waterheatingfuel = DYCOL43,
    celebration_m = DYCOL42,
    celebration_y = DYCOL43,
    mourning_m = DYCOL44,
    mourning_y = DYCOL45,
    house_maintenance_m = DYCOL46,
    house_maintenance_y = DYCOL47,
    pilgrimage_m = DYCOL48,
    pilgrimage_y = DYCOL49,
    travel_abroad_m = DYCOL50,
    travel_abroad_y = DYCOL51,
    surgery_m = DYCOL52,
    #surgery_y = DYCOL56,
    occasions_other_m = DYCOL53,
    occasions_other_y = DYCOL54,
    none_2 = DYCOL55) |>
  mutate(across(where(is.character), as.integer),
         across(c(tenure,material,cookingfuel,heatingfuel), as.factor),
         tenure = fct_recode(tenure, !!!tenure), 
         material = fct_recode(material, !!!material),
         cookingfuel = fct_recode(cookingfuel, !!!fuel), 
         heatingfuel = fct_recode(heatingfuel, !!!fuel1),
         across(c(vehicle:split,celebration_m:occasions_other_y), ~!is.na(.x)))

U85P2 <- U85P2 |>
  rename(
    Address = ADDRESS,
    tenure = DYCOL01,
    room = DYCOL02,
    space = DYCOL03,
    construction = DYCOL04,
    material = DYCOL05,
    vehicle = DYCOL06,
    motorcycle = DYCOL07,
    bicycle = DYCOL08,
    radio = DYCOL09,
    radiotape = DYCOL10,
    TVbw = DYCOL11,
    TV = DYCOL12,
    VHS_VCD_DVD = DYCOL13,
    computer = DYCOL14,
    cellphone = DYCOL15,
    freezer = DYCOL16,
    refridgerator = DYCOL17,
    fridge = DYCOL18,
    stove = DYCOL19,
    vacuum = DYCOL20,
    washingmachine = DYCOL21,
    sewingmachine = DYCOL22,
    fan = DYCOL23,
    evapcoolingportable = DYCOL24,
    splitportable = DYCOL25,
    dishwasher = DYCOL26,
    none = DYCOL27,
    pipewater = DYCOL28,
    electricity = DYCOL29,
    pipegas = DYCOL30,
    telephone = DYCOL31,
    internet  = DYCOL32,
    bathroom = DYCOL33,
    kitchen = DYCOL34,
    evapcooling = DYCOL35,
    centralcooling = DYCOL36,
    centralheating = DYCOL37,
    package = DYCOL38,
    split = DYCOL39,
    #wastewater = DYCOL40,
    cookingfuel = DYCOL40,
    heatingfuel = DYCOL41,
    #waterheatingfuel = DYCOL43,
    celebration_m = DYCOL42,
    celebration_y = DYCOL43,
    mourning_m = DYCOL44,
    mourning_y = DYCOL45,
    house_maintenance_m = DYCOL46,
    house_maintenance_y = DYCOL47,
    pilgrimage_m = DYCOL48,
    pilgrimage_y = DYCOL49,
    travel_abroad_m = DYCOL50,
    travel_abroad_y = DYCOL51,
    surgery_m = DYCOL52,
    #surgery_y = DYCOL56,
    occasions_other_m = DYCOL53,
    occasions_other_y = DYCOL54,
    none_2 = DYCOL55) |>
  mutate(across(where(is.character), as.integer),
         across(c(tenure,material,cookingfuel,heatingfuel), as.factor),
         tenure = fct_recode(tenure, !!!tenure), 
         material = fct_recode(material, !!!material),
         cookingfuel = fct_recode(cookingfuel, !!!fuel), 
         heatingfuel = fct_recode(heatingfuel, !!!fuel1),
         across(c(vehicle:split,celebration_m:occasions_other_y), ~!is.na(.x)))


###################################
# Part 3, Table 1

CPI <- readRDS("CPI.rds") |>
  filter(year == 1385) |>
  group_by(Global) |>
  summarise(across(cpi:cpi_y,~mean(.x, na.rm = T))) 


Global <- readRDS("Global.rds") |>  # Global code for items
  select(Global, code = "C85") |>
  filter(!is.na(code))

GlobalM <- Global |> filter(Global %/% 1e6 == 1)
GlobalY <- Global |> filter(Global %/% 1e6 == 2)

# Part 3, Table 1
R85P3S01$DYCOL04 <- as.numeric(as.character(R85P3S01$DYCOL04))
R85P3S01$DYCOL05 <- as.numeric(as.character(R85P3S01$DYCOL05))

R85P3S01 <- R85P3S01 |>
  rename(
    Address = ADDRESS,
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
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U85P3S01$DYCOL04 <- as.numeric(as.character(U85P3S01$DYCOL04))
U85P3S01$DYCOL05 <- as.numeric(as.character(U85P3S01$DYCOL05))

U85P3S01 <- U85P3S01 |>
  rename(
    Address = ADDRESS,
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
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 2

R85P3S02 <- R85P3S02 |>
  rename(
    Address = ADDRESS,
    code = DYCOL01,
    purchased = DYCOL02,
    gram = DYCOL03,
    kilogram = DYCOL04,
    price = DYCOL05,
    value = DYCOL06) |>
  mutate( 
    table = 2L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U85P3S02 <- U85P3S02 |>
  rename(
    Address = ADDRESS,
    code = DYCOL01,
    purchased = DYCOL02,
    gram = DYCOL03,
    kilogram = DYCOL04,
    price = DYCOL05,
    value = DYCOL06) |>
  mutate( 
    table = 2L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 3

R85P3S03 <- R85P3S03 |>
  rename(
    Address = ADDRESS,
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) |>
  mutate( 
    table = 3L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U85P3S03 <- U85P3S03 |>
  rename(
    Address = ADDRESS,
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) |>
  mutate(
    table = 3L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 4

R85P3S04 <- R85P3S04 |>
  rename(
    Address = ADDRESS,
    code = DYCOL01,
    mortgage = DYCOL02,
    purchased = DYCOL03,
    value = DYCOL04) |>
  mutate( 
    table = 4L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U85P3S04 <- U85P3S04 |>
  rename(
    Address = ADDRESS,
    code = DYCOL01,
    mortgage = DYCOL02,
    purchased = DYCOL03,
    value = DYCOL04) |>
  mutate( 
    table = 4L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 5

R85P3S05 <- R85P3S05 |>
  rename(
    Address = ADDRESS,
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) |>
  mutate( 
    table = 5L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U85P3S05 <- U85P3S05 |>
  rename(
    Address = ADDRESS,
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) |>
  mutate( 
    table = 5L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 6

R85P3S06 <- R85P3S06  |>
  rename(
    Address = ADDRESS,
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) |>
  mutate( 
    table = 6L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U85P3S06 <- U85P3S06  |>
  rename(
    Address = ADDRESS,
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) |>
  mutate( 
    table = 6L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 7

R85P3S07 <- R85P3S07 |>
  rename(
    Address = ADDRESS,
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) |>
  mutate(
         table = 7L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U85P3S07 <- U85P3S07 |>
  rename(
    Address = ADDRESS,
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) |>
  mutate(
         table = 7L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)


# Part 3, Table 8 

R85P3S08 <- R85P3S08 |>
  rename(
    Address = ADDRESS,
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) |>
  mutate( 
    table = 8L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U85P3S08 <- U85P3S08 |>
  rename(
    Address = ADDRESS,
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) |>
  mutate(
    table = 8L) |> 
 left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 9

R85P3S09 <- R85P3S09 |>
  rename(
    Address = ADDRESS,
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) |>
  mutate( 
    table = 9L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U85P3S09 <- U85P3S09 |>
  rename(
    Address = ADDRESS,
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) |>
  mutate( 
    table = 9L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 10

R85P3S10 <- R85P3S10 |>
  rename(
    Address = ADDRESS,
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) |>
  mutate(table = 10L) |>
  mutate_if(is.character, as.numeric)

U85P3S10 <- U85P3S10 |>
  rename(
    Address = ADDRESS,
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) |>
  mutate(table = 10L) |>
  mutate_if(is.character, as.numeric)

# Part 3, Table 11

R85P3S11 <- R85P3S11 |>
  rename(
    Address = ADDRESS,
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) |>
  mutate(
         table = 11L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U85P3S11 <- U85P3S11 |>
  rename(
    Address = ADDRESS,
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) |>
  mutate(
         table = 11L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)


# Part 3, Table 12

R85P3S12 <- R85P3S12 |>
  rename(
    Address = ADDRESS,
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) |>
  mutate( 
    table = 12L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U85P3S12 <- U85P3S12 |>
  rename(
    Address = ADDRESS,
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) |>
  mutate( 
    table = 12L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 13

R85P3S13 <- R85P3S13 |>
  rename(
    Address = ADDRESS,
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
  left_join(CPI) |>
  mutate(value_r=value*100/cpi_y) |>
  select(Address:Global, value_r)

U85P3S13$DYCOL04 <- as.numeric(as.character(U85P3S13$DYCOL04))

U85P3S13 <- U85P3S13 |>
  rename(
    Address = ADDRESS,
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
  left_join(CPI) |>
  mutate(value_r=value*100/cpi_y) |>
  select(Address:Global, value_r)


# Part 3, Table 14

R85P3S14 <- R85P3S14 |>
  rename(
    Address = ADDRESS,
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
  left_join(CPI) |>
  mutate(value_r=value*100/cpi_y) |>
  select(Address:Global, value_r)

U85P3S14 <- U85P3S14 |>
  rename(
    Address = ADDRESS,
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

R85P4S01$DYCOL08 <- as.numeric(as.character(R85P4S01$DYCOL08))

R85P4S01 <- R85P4S01 |>
  rename(
    Address = ADDRESS,
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
  mutate(Global = 1000000) |>
  left_join(CPI) |>
  select(Address:netincome_w_y, cpi_m = cpi, cpi_y)


U85P4S01 <- U85P4S01 |>
  rename(
    Address = ADDRESS,
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
  mutate(Global = 1000000) |>
  left_join(CPI) |>
  select(Address:netincome_w_y, cpi_m = cpi, cpi_y)


# Part 4, Table 2

R85P4S02 <- R85P4S02 |>
  rename(
    Address = ADDRESS,
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
  mutate(Global = 1000000) |>
  left_join(CPI) |>
  select(Address:income_s_y, cpi_m = cpi, cpi_y)

U85P4S02 <- U85P4S02 |>
  rename(
    Address = ADDRESS,
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
  mutate(Global = 1000000) |>
  left_join(CPI) |>
  select(Address:income_s_y, cpi_m = cpi, cpi_y)


# Part 4, Table 3
R85P4S03 <- R85P4S03 |>
  rename(
    Address = ADDRESS,
    member = DYCOL01,
    income_pension = DYCOL03,
    income_rent = DYCOL04,
    income_interest = DYCOL05,
    income_aid = DYCOL06,
    income_resale = DYCOL07,
    income_transfer = DYCOL08) |>
  mutate(Global = 1000000) |>
  left_join(CPI) |>
  select(Address:income_transfer, cpi_y)


U85P4S03 <- U85P4S03 |>
  rename(
    Address = ADDRESS,
    member = DYCOL01,
    income_pension = DYCOL03,
    income_rent = DYCOL04,
    income_interest = DYCOL05,
    income_aid = DYCOL06,
    income_resale = DYCOL07,
    income_transfer = DYCOL08) |>
  mutate(Global = 1000000) |>
  left_join(CPI) |>
  select(Address:income_transfer, cpi_y)


rm(list = setdiff(ls(), ls(pattern = "85"))) # removing unnecessary objects

save.image(file="./exported/HEIS85.Rdata")


CPI <- readRDS("CPI.rds") |>
  filter(year == 1385) |>
  group_by(Global) |>
  summarise(across(cpi:cpi_y,~mean(.x, na.rm = T))) 


###############################################
# Item-level expenditure table
# Merging tables in a unique expenditure table

R85P3 <- bind_rows(mget(ls(pattern = "R85P3S.*")))
R85P3 <- R85P3 |>
  left_join(R85Data) |>
  mutate(urban = "R")

U85P3 <- bind_rows(mget(ls(pattern = "U85P3S.*")))
U85P3 <- U85P3 |>
  left_join(U85Data) |>
  mutate(urban = "U")

itemcode <- read_excel("Codes.xlsx", sheet = "itemLabel") |> select(-labelFA)
itemcode$item <- factor(itemcode$Global, levels = itemcode$Global, labels = itemcode$labelEN)

EXP85 <- bind_rows(R85P3,U85P3) |>
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

attr(EXP85$Table, "label") <- "Table in Part 3"
attr(EXP85$Global, "label") <- "global item code"
attr(EXP85$code, "label") <- "item code in this year"
attr(EXP85$urban, "label") <- "rural or urban"
attr(EXP85$Price, "label") <- "median price"
attr(EXP85$Value, "label") <- "monthly expenditure"
attr(EXP85$Value_r, "label") <- "monthly expenditure in 1390 price"

saveRDS(EXP85, "./exported/EXP85.Rds")

###############################################
# Building individual level data

r85data <- R85Data |> select(Address, weight, province)
u85data <- U85Data |> select(Address, weight, province)

# faster code with data.table
DT <- data.table(R85P4S01)
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
R85P4S01_unique <- as.data.frame(DT) |>
  mutate(across(c(hours_w,days_w), ~ifelse(.x == 0,NA,.x)))

DT <- data.table(R85P4S02)
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
R85P4S02_unique <- as.data.frame(DT) |>
  mutate(across(c(hours_s,days_s), ~ifelse(.x == 0,NA,.x)))

DT <- data.table(R85P4S03)
DT <- DT[,.('income_pension'= sum(income_pension, na.rm = T),
            'income_rent'= sum(income_rent, na.rm = T),
            'income_interest'= sum(income_interest, na.rm = T),
            'income_aid'= sum(income_aid, na.rm = T),
            'income_resale'= sum(income_resale, na.rm = T),
            'income_transfer'= sum(income_transfer, na.rm = T)),
         by=.(Address, member)]
R85P4S03_unique <- as.data.frame(DT) 

DT <- data.table(U85P4S01)
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
U85P4S01_unique <- as.data.frame(DT) |>
  mutate(across(c(hours_w,days_w), ~ifelse(.x == 0,NA,.x)))

DT <- data.table(U85P4S02)
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
U85P4S02_unique <- as.data.frame(DT) |>
  mutate(across(c(hours_s,days_s), ~ifelse(.x == 0,NA,.x)))

DT <- data.table(U85P4S03)
DT <- DT[,.('income_pension'= sum(income_pension, na.rm = T),
            'income_rent'= sum(income_rent, na.rm = T),
            'income_interest'= sum(income_interest, na.rm = T),
            'income_aid'= sum(income_aid, na.rm = T),
            'income_resale'= sum(income_resale, na.rm = T),
            'income_transfer'= sum(income_transfer, na.rm = T)),
         by=.(Address, member)]
U85P4S03_unique <- as.data.frame(DT) 

Rind85 <- r85data  |>
  left_join(select(R85P1,-degree)) |>
  left_join(R85P4S01_unique) |>
  left_join(R85P4S02_unique) |>
  left_join(R85P4S03_unique) |> 
  mutate(across(where(is.character),as.integer)) |>
  mutate(urban = "R")

Uind85 <- u85data |>
  left_join(select(U85P1,-degree)) |>
  left_join(U85P4S01_unique) |>
  left_join(U85P4S02_unique) |>
  left_join(U85P4S03_unique) |> 
  mutate(across(where(is.character),as.integer)) |>
  mutate(urban = "U")

IND85 <- bind_rows(Rind85,Uind85) |>
  mutate(urban = as.factor(urban),
         Global = 1000000,
         employed_w = factor(employed_w, levels = c(1,2), labels = c("Yes","No")),
         status_w = factor(status_w, levels = c(1,2,3), labels = c("public","cooperative","private")),
         employed_s = factor(employed_s, levels = c(1,2), labels = c("Yes","No")),
         status_s = factor(status_s, levels = c(1,2,3), labels = c("employer","selfemployed","familyworker")),
         agriculture = factor(agriculture, levels = c(1,2), labels = c("agriculture","nonagriculture"))
  ) |>
  left_join(CPI) |>
  select(Address:urban, cpi_m = cpi, cpi_y) |>
  as.data.frame()

attr(IND85$occupationalst, "label") <- "employment status"
attr(IND85$employed_w, "label") <- "Whether employed in wage-earning job?"
attr(IND85$ISIC_w, "label") <- "Industry code of wage-earning job"
attr(IND85$ISCO_w, "label") <- "Occupation code of wage-earning job"
attr(IND85$status_w, "label") <- "Wage-earning job status: 1-public 2-cooperative 3-private"
attr(IND85$hours_w, "label") <- "Wage-earning job: hours per day"
attr(IND85$days_w, "label") <- "Wage-earning job: day per week"
attr(IND85$netincome_w_m, "label") <- "wage-earning job: net income previous month"
attr(IND85$netincome_w_y, "label") <- "wage earning job: net income previous year"
attr(IND85$employed_s, "label") <- "Whether employed in non-wage job?"
attr(IND85$ISIC_s, "label") <- "Industry code of non-wage job"
attr(IND85$ISCO_s, "label") <- "Occupation code of non-wage job"
attr(IND85$status_s, "label") <- "job status: 4-employer 2-selfemployed 3-familyworker"
attr(IND85$hours_s, "label") <- "non-wage job: hours per day"
attr(IND85$days_s, "label") <- "non-wage job: day per week"
attr(IND85$income_s_y, "label") <- "non-wage job: net income previous year"

saveRDS(IND85,"./exported/IND85.Rds")

###############################################
# Building Household Data

r85p1 <- R85P1 |>
  group_by(Address) |>
  summarize(size = sum(!is.na(member)),
            literates = sum(literacy == "literate", na.rm = TRUE),
            students = sum(studying == "Yes", na.rm = TRUE),
            employeds = sum(occupationalst == "employed", na.rm = TRUE)) 

u85p1 <- U85P1 |>
  group_by(Address) |>
  summarize(size = sum(!is.na(member)),
            literates = sum(literacy == "literate", na.rm = TRUE),
            students = sum(studying == "Yes", na.rm = TRUE),
            employeds = sum(occupationalst == "employed", na.rm = TRUE)) 

r_head <- R85P1 |>
  filter(relation == "head") |>
  select(-member,-relation,-studying)

u_head <- U85P1 |>
  filter(relation == "head") |>
  select(-member,-relation,-studying)

# summary of heads job codes
r_job <- Rind85 |>
  filter(relation == "head") |>
  select(Address,starts_with("IS"))

u_job <- Uind85 |>
  filter(relation == "head") |>
  select(Address,starts_with("IS"))

# Sum of household expenditure items
r85p3 <- R85P3 |>
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

u85p3 <- U85P3 |>
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
r_NM_housing <- R85P3S04 |>
  filter(Global %in% c(1042111,1042211,1042212) ) |>
  group_by(Address) |>
  summarize(income_nm_house = sum(value*12, na.rm = T))

u_NM_housing <- U85P3S04 |>
  filter(Global %in% c(1042111,1042211,1042212) ) |>
  group_by(Address) |>
  summarize(income_nm_house = sum(value*12, na.rm = T))

r_NMincome <- R85P3 |>
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

u_NMincome <- U85P3 |>
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

# # sum of household income
# r_incomeSum <- Rind98 |>
#   group_by(Address) |>
#   summarise(across(c(starts_with(c("income","netincome")),"subsidy"), ~sum(.x,na.rm = T)))
# u_incomeSum <- Uind98 |>
#   group_by(Address) |>
#   summarise(across(c(starts_with(c("income","netincome")),"subsidy"), ~sum(.x,na.rm = T)))

# sum of household income with data.table

DT <- data.table(Rind85)
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

DT <- data.table(Uind85)
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
RHH85 <- r85data |> 
  mutate(urban = "R") |>
  left_join(r85p1, by="Address") |>
  left_join(r_head, by = "Address") |>
  left_join(r_job, by = "Address") |>
  left_join(r85p3, by = "Address") |>
  left_join(r_incomeSum, by = "Address") |>
  left_join(r_NMincome, by = "Address") |>
  left_join(R85P2) |>
  mutate(across(income_w_y:income_nm_house, ~replace_na(.x, 0)))

UHH85 <- u85data |> 
  mutate(urban = "U") |>
  left_join(u85p1, by="Address") |>
  left_join(u_head, by = "Address") |>
  left_join(u_job, by = "Address") |>
  left_join(u85p3, by = "Address") |>
  left_join(u_incomeSum, by = "Address") |>
  left_join(u_NMincome, by = "Address") |>
  left_join(U85P2) |>
  mutate(across(income_w_y:income_nm_house, ~replace_na(.x, 0)))

HH85 <- bind_rows(RHH85, UHH85) |>
  mutate(urban = as.factor(urban)) |>
  mutate(expenditure = cost_food + cost_tobacco + cost_clothing + cost_housing + cost_appliances + cost_health + cost_transport + cost_communication + cost_recreation +  cost_restaurant + cost_miscellaneous
         + cost_durables/12,
         expenditure_gross = cost_food + cost_tobacco + cost_clothing + cost_housing + cost_appliances + cost_health + cost_transport + cost_communication + cost_recreation +  cost_restaurant + cost_miscellaneous
         + cost_durables_gross/12,
         income = income_s_y + netincome_w_y + income_pension + income_rent + income_interest + income_aid + income_resale + income_transfer + income_nm_agriculture + income_nm_free + income_nm_homemade + income_nm_public + income_nm_private + income_nm_nonagriculture + income_nm_house,
         Global = 1000000 ) |>
  left_join(CPI) |>
  select(Address:income, cpi_m = cpi, cpi_y) |>
  as.data.frame()

attr(HH85$size, "label") <- "Household size"
attr(HH85$literates, "label") <- "Number of literate members"
attr(HH85$students, "label") <- "Number of student members"
attr(HH85$employeds, "label") <- "Number of employed members"
attr(HH85$gender, "label") <- "Head's gender"
attr(HH85$age, "label") <- "Head's age"
attr(HH85$literacy, "label") <- "Head's literacy"
attr(HH85$education, "label") <- "Head's education"
attr(HH85$occupationalst, "label") <- "Head's job status"
attr(HH85$maritalst, "label") <- "Head's marital status"
attr(HH85$ISIC_w, "label") <- "Head's Industry code of wage-earning job"
attr(HH85$ISCO_w, "label") <- "Head's Occupation code of wage-earning job"
attr(HH85$ISIC_s, "label") <- "Head's Industry code of non-wage job"
attr(HH85$ISCO_s, "label") <- "Head's Occupation code of non-wage job"
attr(HH85$income_nm_agriculture, "label") <- "Non-monetary income from agriculture"
attr(HH85$income_nm_nonagriculture, "label") <- "Non-monetary income from nonagriculture"
attr(HH85$income_nm_public, "label") <- "Non-monetary income from public sector job"
attr(HH85$income_nm_private, "label") <- "Non-monetary income from private sector job"
attr(HH85$income_nm_homemade, "label") <- "Non-monetary income from home production"
attr(HH85$netincome_w_m, "label") <- "wage-earning job: net income previous month"
attr(HH85$netincome_w_y, "label") <- "wage-earning job: net income previous year"
attr(HH85$income_w_y, "label") <- "wage-earning job: gross income previous year"
attr(HH85$income_s_y, "label") <- "non-wage job: net income previous year"
attr(HH85$income, "label") <- "total household income in previous year"
attr(HH85$expenditure, "label") <- "total monthly expendiuture of the household"

saveRDS(HH85, file = "./exported/HH85.Rds")


#######################################
# Exporting Rds files to CSV, STATA, etc.

#install.packages("haven") # uncomment if not already installed

df <- readRDS("./exported/HH85.Rds") # Specify the Rds file to convert here

#haven::write_dta(df, "./exported/HH85.dta") # to export in STATA
#write_csv2(df, "./exported/HH85.csv") # to export in CSV

