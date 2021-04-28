library(tidyverse)
library(data.table)
library(readxl)

rm(list = ls())

load("./RAW/98.RData")

##############################
Province <- c(Markazi="00", Gilan="01", Mazandaran="02", AzarbaijanSharghi="03", AzarbaijanGharbi="04",
              Kermanshah="05", Kouzestan="06", Fars="07", Kerman="08", KhorasanRazavi="09",
              Esfahan="10", SistanBalouchestan="11", Kordestan="12", Hamedan="13", CharmahalBakhtiari="14",
              Lorestan="15", Ilam="16", KohkilouyeBoyerahamad="17", Boushehr="18", Zanjan="19", 
              Semnan="20", Yazd="21", Hormozgan="22", Tehran="23", Ardebil="24", Qom="25", Qazvin="26",
              Golestan="27", KhorasanShomali="28", KhorasanJonoubi="29", Alborz="30")

R98Data <- R98Data %>% 
  rename(month = MahMorajeh, khanevartype = NoeKhn) %>%
  mutate(province = fct_recode(as.factor(substr(Address, 2, 3)), !!!Province),
         town = as.integer(substr(Address, 4, 5)))
U98Data <- U98Data %>% 
  rename(month = MahMorajeh, khanevartype = NoeKhn) %>%
  mutate(province = fct_recode(as.factor(substr(Address, 2, 3)), !!!Province),
         town = as.integer(substr(Address, 4, 5)))

##############################
# Part 1

relation <- c(Head="1", Spouse="2", Child="3", SonDaughter_inLaw="4", GrandSonDaughter="5", Parent="6", Sibling="7", OtherRelative="8", NonRelative="9")
gender <- c(Male="1", Female="2")
literacy <- c(literate="1", illiterate="2")
yesno <- c(Yes="1", No="2")
education <- c(Elemantry="1", Secondary="2", HighSchool="3", Diploma="4", College="5", Bachelor="6", Master="7", PhD="8", Other="9")
occupation <- c(employed="1", unemployed="2", IncomeWOJob="3", Student="4", Housewife="5", Other="6")
marital <- c(Married ="1", Widowed="2", Divorced="3", Single="4")

R98P1 <- R98P1 %>% 
  rename(
    member = DYCOL01,
    relation = DYCOL03,
    gender = DYCOL04,
    age = DYCOL05,
    literacy = DYCOL06,
    studying = DYCOL07,
    education = DYCOL08,
    occupationalst = DYCOL09,
    maritalst = DYCOL10) %>% 
  mutate(across(where(is.character), as.integer),
         across(c(relation,gender,literacy,studying,education,occupationalst,maritalst), as.factor),
         relation = fct_recode(relation, !!!relation), 
         gender = fct_recode(gender, !!!gender),
         literacy = fct_recode(literacy, !!!literacy), 
         studying = fct_recode(studying, !!!yesno),
         education = fct_recode(education, !!!education), 
         occupationalst = fct_recode(occupationalst, !!!occupation),
         maritalst = fct_recode(maritalst, !!!marital))

U98P1 <- U98P1 %>% 
  rename(
    member = DYCOL01,
    relation = DYCOL03,
    gender = DYCOL04,
    age = DYCOL05,
    literacy = DYCOL06,
    studying = DYCOL07,
    education = DYCOL08,
    occupationalst = DYCOL09,
    maritalst = DYCOL10 ) %>% 
  mutate(across(where(is.character), as.integer),
         across(c(relation,gender,literacy,studying,education,occupationalst,maritalst), as.factor),
         relation = fct_recode(relation, !!!relation), 
         gender = fct_recode(gender, !!!gender),
         literacy = fct_recode(literacy, !!!literacy), 
         studying = fct_recode(studying, !!!yesno),
         education = fct_recode(education, !!!education), 
         occupationalst = fct_recode(occupationalst, !!!occupation),
         maritalst = fct_recode(maritalst, !!!marital))

##############################
# Part 2

tenure <- c(OwnedEstateLand="1", OwnedEstate="2", Rent="3", Mortgage="4", Service="5", Free="6", Other="7")
material <- c(MetalBlock="1", BrickWood="2", Cement="3", Brick="4", Wood="5", WoodKesht="6", KeshtGel="7", Other="8")
fuel <- c(Oil="1", Gasoline="2", LiquidGas="3", NaturalGas="4", Electricity="5", Wood="6", AnimalOil="7", Coke="8", Other="9", None="10" )
fuel1 <- c(Oil="11", Gasoline="12", LiquidGas="13", NaturalGas="14", Electricity="15", Wood="16", AnimalOil="17", Coke="18", Other="19", None="20" )
fuel2 <- c(Oil="21", Gasoline="22", LiquidGas="23", NaturalGas="24", Electricity="25", Wood="26", AnimalOil="27", Coke="28", Other="29", None="30" )

R98P2 <- R98P2 %>%
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
    microwave = DYCOL28,
    none = DYCOL29,
    pipewater = DYCOL30,
    electricity = DYCOL31,
    pipegas = DYCOL32,
    telephone = DYCOL33,
    internet  = DYCOL34,
    bathroom = DYCOL35,
    kitchen = DYCOL36,
    evapcooling = DYCOL37,
    centralcooling = DYCOL38,
    centralheating = DYCOL39,
    package = DYCOL40,
    split = DYCOL41,
    wastewater = DYCOL42,
    cookingfuel = DYCOL43,
    heatingfuel = DYCOL44,
    waterheatingfuel = DYCOL45) %>%
  mutate(across(where(is.character), as.integer),
         across(c(tenure,material,cookingfuel,heatingfuel,waterheatingfuel), as.factor),
         tenure = fct_recode(tenure, !!!tenure), 
         material = fct_recode(material, !!!material),
         cookingfuel = fct_recode(cookingfuel, !!!fuel), 
         heatingfuel = fct_recode(heatingfuel, !!!fuel1),
         waterheatingfuel = fct_recode(waterheatingfuel, !!!fuel2),
         across(vehicle:wastewater, ~!is.na(.x)))
         

U98P2 <- U98P2 %>%
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
    microwave = DYCOL28,
    none = DYCOL29,
    pipewater = DYCOL30,
    electricity = DYCOL31,
    pipegas = DYCOL32,
    telephone = DYCOL33,
    internet  = DYCOL34,
    bathroom = DYCOL35,
    kitchen = DYCOL36,
    evapcooling = DYCOL37,
    centralcooling = DYCOL38,
    centralheating = DYCOL39,
    package = DYCOL40,
    split = DYCOL41,
    wastewater = DYCOL42,
    cookingfuel = DYCOL43,
    heatingfuel = DYCOL44,
    waterheatingfuel = DYCOL45) %>%
  mutate(across(where(is.character), as.integer),
         across(c(tenure,material,cookingfuel,heatingfuel,waterheatingfuel), as.factor),
         tenure = fct_recode(tenure, !!!tenure), 
         material = fct_recode(material, !!!material),
         cookingfuel = fct_recode(cookingfuel, !!!fuel), 
         heatingfuel = fct_recode(heatingfuel, !!!fuel1),
         waterheatingfuel = fct_recode(waterheatingfuel, !!!fuel2),
         across(vehicle:wastewater, ~!is.na(.x)))

##############################
# Part 3, Table 1

month <- R98Data %>%
  bind_rows(U98Data) %>%
  select(Address,month) %>%
  mutate(year=1398)

CPI <- readRDS("CPI/CPI.rds")

R98P3S01 <- R98P3S01 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    gram = DYCOL03,
    kilogram = DYCOL04,
    price = DYCOL05,
    value = DYCOL06 ) %>%
  mutate(DYCOL00 = case_when(
    code == 11241 ~ 11240L,
    TRUE ~ code),
    across(c(price,value,kilogram),  ~ as.numeric(as.character(.x)) ),
    table = 1L) %>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)


U98P3S01 <- U98P3S01 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    gram = DYCOL03,
    kilogram = DYCOL04,
    price = DYCOL05,
    value = DYCOL06 ) %>%
  mutate(DYCOL00 = case_when(
    code == 11241 ~ 11240L,
    TRUE ~ code),
    across(c(price,value,kilogram),  ~ as.numeric(as.character(.x)) ),
    table = 1L) %>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)

# Part 3, Table 2
R98P3S02 <- R98P3S02 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    gram = DYCOL03,
    kilogram = DYCOL04,
    price = DYCOL05,
    value = DYCOL06 ) %>%
  mutate(DYCOL00 = code,
         table = 2L) %>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)

U98P3S02 <- U98P3S02 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    gram = DYCOL03,
    kilogram = DYCOL04,
    price = DYCOL05,
    value = DYCOL06 ) %>%
  mutate(DYCOL00 = code,
         table = 2L) %>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)

# Part 3, Table 3
R98P3S03 <- R98P3S03 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03 ) %>%
  mutate(DYCOL00 = case_when(
    code == 31244 ~ 31255L,
    code == 31269 ~ 31263L,
    TRUE ~ code),
    table = 3L) %>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)


U98P3S03 <- U98P3S03 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) %>%
  mutate(DYCOL00 = case_when(
    code == 31244 ~ 31255L,
    code == 31269 ~ 31263L,
    TRUE ~ code),
    table = 3L) %>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)


# Part 3, Table 4
R98P3S04 <- R98P3S04 %>%
  rename(
    code = DYCOL01,
    mortgage = DYCOL02,
    purchased = DYCOL03,
    value = DYCOL04 ) %>%
  mutate(DYCOL00 = case_when(
    code == 44418 ~ 44419L,
    TRUE ~ code),
    table = 4L) %>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)


U98P3S04 <- U98P3S04 %>%
  rename(
    code = DYCOL01,
    mortgage = DYCOL02,
    purchased = DYCOL03,
    value = DYCOL04 ) %>%
  mutate(DYCOL00 = case_when(
    code == 44418 ~ 44419L,
    TRUE ~ code),
    table = 4L)  %>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)

# Part 3, Table 5
R98P3S05 <- R98P3S05 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03 ) %>%
  mutate(DYCOL00 = code,
         table = 5L) %>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)


U98P3S05 <- U98P3S05 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03 ) %>%
  mutate(DYCOL00 = code,
         table = 5L) %>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)



# Part 3, Table 6
R98P3S06 <- R98P3S06  %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) %>%
  mutate(DYCOL00 = case_when(
    code == 62128 ~ 62125L,
    code == 62129 ~ 62126L,
    TRUE ~ code),
    table = 6L) %>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)


U98P3S06 <- U98P3S06 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) %>%
  mutate(DYCOL00 = case_when(
    code == 62128 ~ 62125L,
    code == 62129 ~ 62126L,
    TRUE ~ code),
    table = 6L) %>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)


# Part 3, Table 7
R98P3S07 <- R98P3S07 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) %>%
  mutate(DYCOL00 = case_when(
    code == 73611 ~ 73615L,
    TRUE ~ code),
    table = 7L) %>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)


U98P3S07 <- U98P3S07 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03 ) %>%
  mutate(DYCOL00 = case_when(
    code == 73611 ~ 73615L,
    TRUE ~ code),
    table = 7L) %>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)


# Part 3, Table 8
R98P3S08 <- R98P3S08 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) %>%
  mutate(DYCOL00 = code,
         table = 8L) %>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)


U98P3S08 <- U98P3S08 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) %>%
  mutate(DYCOL00 = code,
         table = 8L) %>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)


# Part 3, Table 9
R98P3S09 <- R98P3S09 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) %>%
  mutate(DYCOL00 = code,
         table = 9L) %>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)


U98P3S09 <- U98P3S09 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) %>%
  mutate(DYCOL00 = code,
         table = 9L) %>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)


# Part 3, Table 10
R98P3S10 <- R98P3S10 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) %>%
  mutate(DYCOL00 = code,
         table = 10L) %>%
  mutate_if(is.character, as.numeric)

U98P3S10 <- U98P3S10 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) %>%
  mutate(DYCOL00 = code,
         table = 10L)  %>%
  mutate_if(is.character, as.numeric)

# Part 3, Table 11

R98P3S11 <- R98P3S11 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) %>%
  mutate(DYCOL00 = code,
         table = 11L) %>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)


U98P3S11 <- U98P3S11 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) %>%
  mutate(DYCOL00 = code,
         table = 11L) %>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)


# Part 3, Table 12
R98P3S12 <- R98P3S12 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) %>%
  mutate(DYCOL00 = code,
         table = 12L) %>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)


U98P3S12 <- U98P3S12 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03) %>%
  mutate(DYCOL00 = code,
         table = 12L) %>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)



# Part 3, Table 13
R98P3S13 <- R98P3S13 %>%
  rename(
    code = DYCOL01,
    insured_loan = DYCOL02,
    loanfrom = DYCOL03,
    purchased = DYCOL04,
    cost = DYCOL05,
    sell = DYCOL06) %>%
  mutate(DYCOL00 = case_when(
    code == 51161 ~ 51160L,
    TRUE ~ code),
    cost = replace_na(cost,0),
    sell = replace_na(sell,0),
    value = cost - sell,
    table = 13L) %>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi_y) %>%
  select(Address:table, value_r)


U98P3S13 <- U98P3S13 %>%
  rename(
    code = DYCOL01,
    insured_loan = DYCOL02,
    loanfrom = DYCOL03,
    purchased = DYCOL04,
    cost = DYCOL05,
    sell = DYCOL06) %>%
  mutate(DYCOL00 = case_when(
    code == 51161 ~ 51160L,
    TRUE ~ code),
    cost = replace_na(cost,0),
    sell = replace_na(sell,0),
    value = cost - sell,
    table = 13L) %>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi_y) %>%
  select(Address:table, value_r)

# Part 3, Table 14
R98P3S14 <- R98P3S14 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    cost = DYCOL03,
    sell = DYCOL04) %>%
  mutate(DYCOL00 = code,
         cost = replace_na(cost,0),
         sell = replace_na(sell,0),
         value = cost - sell,
         table = 14L) %>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi_y) %>%
  select(Address:table, value_r)

U98P3S14 <- U98P3S14 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    cost = DYCOL03,
    sell = DYCOL04) %>%
  mutate(DYCOL00 = code,
         cost = replace_na(cost,0),
         sell = replace_na(sell,0),
         value = cost - sell,
         table = 14L) %>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi_y) %>%
  select(Address:table, value_r)


list2env(lapply(mget(ls(pattern = "P3S.*")),
                function(x) {x %>% mutate(purchased=factor(purchased,
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


##############################
# Part 4, Table 1
R98P4S01 <- R98P4S01 %>%
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
    netincome_w_y = DYCOL15) %>%
  left_join(month) %>%
  mutate(DYCOL00= NA_integer_) %>%
  left_join(CPI) %>%
  select(Address:netincome_w_y, cpi_m = cpi, cpi_y)
 
U98P4S01 <- U98P4S01 %>%
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
    netincome_w_y = DYCOL15) %>%
  left_join(month) %>%
  mutate(DYCOL00= NA_integer_) %>%
  left_join(CPI) %>%
  select(Address:netincome_w_y, cpi_m = cpi, cpi_y)

# Part 4, Table 2
R98P4S02 <- R98P4S02 %>%
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
    income_s_y = DYCOL15) %>%
  left_join(month) %>%
  mutate(DYCOL00= NA_integer_) %>%
  left_join(CPI) %>%
  select(Address:income_s_y, cpi_m = cpi, cpi_y)

U98P4S02 <- U98P4S02 %>%
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
    income_s_y = DYCOL15) %>%
  left_join(month) %>%
  mutate(DYCOL00= NA_integer_) %>%
  left_join(CPI) %>%
  select(Address:income_s_y, cpi_y)

# Part 4, Table 3
R98P4S03 <- R98P4S03 %>%
  rename(
    member = DYCOL01,
    income_pension = DYCOL03,
    income_rent = DYCOL04,
    income_interest = DYCOL05,
    income_aid = DYCOL06,
    income_resale = DYCOL07,
    income_transfer = DYCOL08) %>%
  left_join(month) %>%
  mutate(DYCOL00= NA_integer_) %>%
  left_join(CPI) %>%
  select(Address:income_transfer, cpi_y)

U98P4S03 <- U98P4S03 %>%
  rename(
    member = DYCOL01,
    income_pension = DYCOL03,
    income_rent = DYCOL04,
    income_interest = DYCOL05,
    income_aid = DYCOL06,
    income_resale = DYCOL07,
    income_transfer = DYCOL08) %>%
  left_join(month) %>%
  mutate(DYCOL00= NA_integer_) %>%
  left_join(CPI) %>%
  select(Address:income_transfer, cpi_y)

# Part 4, Table 4

R98P4S04 <- R98P4S04 %>%
  rename(
    member = Dycol01,
    subsidy_number = Dycol03,
    subsidy_month = Dycol04,
    subsidy = Dycol05) %>%
  left_join(month) %>%
  mutate(DYCOL00= NA_integer_) %>%
  left_join(CPI) %>%
  select(Address:subsidy, cpi_y)

U98P4S04 <- U98P4S04 %>%
  rename(
    member = Dycol01,
    subsidy_number = Dycol03,
    subsidy_month = Dycol04,
    subsidy = Dycol05) %>%
  left_join(month) %>%
  mutate(DYCOL00= NA_integer_) %>%
  left_join(CPI) %>%
  select(Address:subsidy, cpi_y)

rm(list = setdiff(ls(), ls(pattern = "98"))) # removing unnecessary objects

save.image(file="./exported/HEIS98.Rdata")

month <- R98Data %>%
  bind_rows(U98Data) %>%
  select(Address,month) %>%
  mutate(year=1398)

CPI <- readRDS("CPI/CPI.rds")

###############################################
# Item-level expenditure table

R98P3 <- bind_rows(mget(ls(pattern = "R98P3S.*")))
R98P3 <- R98P3 %>% 
  left_join(R98Data) %>%
  mutate(urban = "R")

U98P3 <- bind_rows(mget(ls(pattern = "U98P3S.*")))
U98P3 <- U98P3 %>% 
  left_join(U98Data) %>% 
  mutate(urban = "U")

itemcode <- read_excel("itemlabels.xlsx") %>%
  filter(!is.na(Global)) %>%
  mutate(gcode=ifelse(is.na(G2),Global,Global*100+G2)) %>%
  select(gcode, Label, LabelFA)
itemcode$item <- factor(itemcode$gcode, levels = itemcode$gcode, labels = itemcode$Label)

EXP98 <- bind_rows(R98P3,U98P3) %>%
  rename(gcode = DYCOL00) %>%
  mutate(urban = as.factor(urban), 
         recallperiod=ifelse(table>12,1/12,1),
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
           table == 11 ~ "restaurant",
           table == 12 ~ "miscellaneous",
           table == 13 ~ "durables",
           table == 14 ~ "investment",
           TRUE ~ NA_character_)) %>%
  group_by(Table, gcode, code, urban) %>%
  summarize(Value = sum(value*weight*recallperiod, na.rm = T),
            Value_r = sum(value_r*weight*recallperiod, na.rm = T),
            Kilogram = sum(kilogram*weight, na.rm = T),
            Gram = sum(gram*weight, na.rm = T),
            Price = median(price, na.rm = T)) %>%
  filter(Value!=0) %>%
  left_join(itemcode) %>%
  select(-Label, -LabelFA) %>%
  as.data.frame()

attr(EXP98$Table, "label") <- "Table in Part 3"
attr(EXP98$gcode, "label") <- "global item code"
attr(EXP98$code, "label") <- "item code in this year"
attr(EXP98$urban, "label") <- "rural or urban"
attr(EXP98$Price, "label") <- "median price"
attr(EXP98$Value, "label") <- "monthly expenditure"
attr(EXP98$Value_r, "label") <- "monthly expenditure in 1390 price"

saveRDS(EXP98, "./exported/EXP98.Rds")

###############################################
# Building individual level data

r98data<- R98Data %>% select(Address, month, weight, khanevartype, Jaygozin, province)
u98data<- U98Data %>% select(Address, month, weight, khanevartype, Jaygozin, province)


# # Multi-job people are repeated and need to be unified. The below code is using dplyr
# R98P4S01_unique <- R98P4S01 %>%
#   group_by(Address, member) %>%
#   summarize(across(c(employed_w,ISCO_w,ISIC_w,status_w), first),
#             across(c(hours_w,days_w,ends_with(c("_y","_m"))), ~ sum(.x,na.rm = T))) %>%
#   ungroup() %>%
#   mutate(across(c(hours_w,days_w), ~ifelse(.x == 0,NA,.x)))
#
# R98P4S02_unique <- R98P4S02 %>%
#   group_by(Address, member) %>%
#   summarize(across(c(employed_s,ISCO_s,ISIC_s,status_s, agriculture), first),
#             across(c(hours_s,days_s,starts_with("cost_"),sale ,income_s_y),
#                    ~sum(.x,na.rm = T))) %>%
#   ungroup() %>%
#   mutate(across(c(hours_s,days_s), ~ifelse(.x == 0,NA,.x)))
# 
# R98P4S03_unique <- R98P4S03 %>%
#   group_by(Address, member) %>%
#   summarize(across(starts_with("income_"),~sum(.x,na.rm = T)))
# 
# R98P4S04_unique <- R98P4S04 %>%
#   group_by(Address, member) %>%
#   summarize(across(starts_with("subsidy"),~sum(.x,na.rm = T)))
# U98P4S01_unique <- U98P4S01 %>%
#   group_by(Address, member) %>%
#   summarize(across(c(employed_w,ISCO_w,ISIC_w,status_w), first),
#             across(c(hours_w,days_w,ends_with(c("_y","_m"))), ~ sum(.x,na.rm = T))) %>%
#   ungroup() %>%
#   mutate(across(c(hours_w,days_w), ~ifelse(.x == 0,NA,.x)))
# 
# U98P4S02_unique <- U98P4S02 %>%
#   group_by(Address, member) %>%
#   summarize(across(c(employed_s,ISCO_s,ISIC_s,status_s, agriculture), first),
#             across(c(hours_s,days_s,starts_with("cost_"),sale, income_s_y),
#                    ~sum(.x,na.rm = T))) %>%
#   ungroup() %>%
#   mutate(across(c(hours_s,days_s), ~ifelse(.x == 0,NA,.x)))
# 
# U98P4S03_unique <- U98P4S03 %>%
#   group_by(Address, member) %>%
#   summarize(across(starts_with("income_"),~sum(.x,na.rm = T)))
# 
# U98P4S04_unique <- U98P4S04 %>%
#   group_by(Address, member) %>%
#   summarize(across(starts_with("subsidy"), ~sum(.x,na.rm = T)))


# faster code with data.table
DT <- data.table(R98P4S01)
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
            'netincome_w_y'= sum(netincome_w_y, na.rm = T)
            ),
        by=.(Address, member)]
R98P4S01_unique <- as.data.frame(DT) %>%
  mutate(across(c(hours_w,days_w), ~ifelse(.x == 0,NA,.x)))


DT <- data.table(R98P4S02)
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
            'sale'= sum(sale, na.rm = T)
            ),
         by=.(Address, member)]
R98P4S02_unique <- as.data.frame(DT) %>%
  mutate(across(c(hours_s,days_s), ~ifelse(.x == 0,NA,.x)))

DT <- data.table(R98P4S03)
DT <- DT[,.('income_pension'= sum(income_pension, na.rm = T),
            'income_rent'= sum(income_rent, na.rm = T),
            'income_interest'= sum(income_interest, na.rm = T),
            'income_aid'= sum(income_aid, na.rm = T),
            'income_resale'= sum(income_resale, na.rm = T),
            'income_transfer'= sum(income_transfer, na.rm = T)
            ),
         by=.(Address, member)]
R98P4S03_unique <- as.data.frame(DT) 


DT <- data.table(R98P4S04)
DT <- DT[,.('subsidy_number'= sum(subsidy_number, na.rm = T),
            'subsidy_month'= sum(subsidy_month, na.rm = T),
            'subsidy'= sum(subsidy, na.rm = T)
            ),
         by=.(Address, member)]
R98P4S04_unique <- as.data.frame(DT) 


DT <- data.table(U98P4S01)
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
            'netincome_w_y'= sum(netincome_w_y, na.rm = T)
            ),
         by=.(Address, member)]
U98P4S01_unique <- as.data.frame(DT) %>%
  mutate(across(c(hours_w,days_w), ~ifelse(.x == 0,NA,.x)))


DT <- data.table(U98P4S02)
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
            'sale'= sum(sale, na.rm = T)
            ),
         by=.(Address, member)]
U98P4S02_unique <- as.data.frame(DT) %>%
  mutate(across(c(hours_s,days_s), ~ifelse(.x == 0,NA,.x)))

DT <- data.table(U98P4S03)
DT <- DT[,.('income_pension'= sum(income_pension, na.rm = T),
            'income_rent'= sum(income_rent, na.rm = T),
            'income_interest'= sum(income_interest, na.rm = T),
            'income_aid'= sum(income_aid, na.rm = T),
            'income_resale'= sum(income_resale, na.rm = T),
            'income_transfer'= sum(income_transfer, na.rm = T)
            ),
         by=.(Address, member)]
U98P4S03_unique <- as.data.frame(DT) 

DT <- data.table(U98P4S04)
DT <- DT[,.('subsidy_number'= sum(subsidy_number, na.rm = T),
            'subsidy_month'= sum(subsidy_month, na.rm = T),
            'subsidy'= sum(subsidy, na.rm = T)
            ),
         by=.(Address, member)]
U98P4S04_unique <- as.data.frame(DT) 


Rind98 <- r98data  %>%
  left_join(R98P1) %>%
  left_join(R98P4S01_unique) %>%
  left_join(R98P4S02_unique) %>%
  left_join(R98P4S03_unique) %>% 
  left_join(R98P4S04_unique) %>%
  mutate(across(where(is.character),as.integer)) %>%
  mutate(urban = "R")

Uind98 <- u98data %>%
  left_join(U98P1) %>%
  left_join(U98P4S01_unique) %>%
  left_join(U98P4S02_unique) %>%
  left_join(U98P4S03_unique) %>% 
  left_join(U98P4S04_unique) %>%
  mutate(across(where(is.character),as.integer)) %>%
  mutate(urban = "U")

IND98 <- bind_rows(Rind98,Uind98) %>%
  mutate(urban = as.factor(urban),
         DYCOL00 = NA_integer_ ,
         employed_w = factor(employed_w, levels = c(1,2), labels = c("Yes","No")),
         status_w = factor(status_w, levels = c(1,2,3), labels = c("public","cooperative","private")),
         employed_s = factor(employed_s, levels = c(1,2), labels = c("Yes","No")),
         status_s = factor(status_s, levels = c(4,5,6), labels = c("employer","selfemployed","familyworker")),
         agriculture = factor(agriculture, levels = c(1,2), labels = c("agriculture","nonagriculture"))
         ) %>%
  left_join(month) %>%
  left_join(CPI) %>%
  select(Address:urban, cpi_m = cpi, cpi_y) %>%
  as.data.frame()

attr(IND98$occupationalst, "label") <- "Job status"
attr(IND98$employed_w, "label") <- "Whether employed in wage-earning job?"
attr(IND98$ISIC_w, "label") <- "Industry code of wage-earning job"
attr(IND98$ISCO_w, "label") <- "Occupation code of wage-earning job"
attr(IND98$status_w, "label") <- "Wage-earning job status: 1-public 2-cooperative 3-private"
attr(IND98$hours_w, "label") <- "Wage-earning job: hours per day"
attr(IND98$days_w, "label") <- "Wage-earning job: day per week"
attr(IND98$netincome_w_m, "label") <- "wage-earning job: net income previous month"
attr(IND98$netincome_w_y, "label") <- "wage earning job: net income previous year"
attr(IND98$employed_s, "label") <- "Whether employed in non-wage job?"
attr(IND98$ISIC_s, "label") <- "Industry code of non-wage job"
attr(IND98$ISCO_s, "label") <- "Occupation code of non-wage job"
attr(IND98$status_s, "label") <- "job status: 4-employer 5-selfemployed 6-familyworker"
attr(IND98$hours_s, "label") <- "non-wage job: hours per day"
attr(IND98$days_s, "label") <- "non-wage job: day per week"
attr(IND98$income_s_y, "label") <- "non-wage job: net income previous year"

saveRDS(IND98,"./exported/IND98.Rds")

###############################################
# Building Household Data

r98p1 <- R98P1 %>%
  group_by(Address) %>%
  summarize(size = sum(!is.na(member)),
         literates = sum(literacy == "literate", na.rm = TRUE),
         students = sum(studying == "Yes", na.rm = TRUE),
         employeds = sum(occupationalst == "employed", na.rm = TRUE)) 

u98p1 <- U98P1 %>%
  group_by(Address) %>%
  summarize(size = sum(!is.na(member)),
            literates = sum(literacy == "literate", na.rm = TRUE),
            students = sum(studying == "Yes", na.rm = TRUE),
            employeds = sum(occupationalst == "employed", na.rm = TRUE)) 

r_head <- R98P1 %>%
  filter(relation == "Head") %>%
  select(-member,-relation,-studying)

u_head <- U98P1 %>%
        filter(relation == "Head") %>%
        select(-member,-relation,-studying)

# summary of heads job codes
r_job <- Rind98 %>%
  filter(relation == "Head") %>%
  select(Address,starts_with("IS"))

u_job <- Uind98 %>%
  filter(relation == "Head") %>%
  select(Address,starts_with("IS"))

# Sum of household expenditure items
r98p3 <- R98P3 %>%
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
    table == 13 ~ "durables",
    table == 14 ~ "investment",
    TRUE ~ NA_character_)) %>%
  group_by(Address,Table) %>%
  summarize(cost = sum(value, na.rm = TRUE),
            cost_r = sum(value_r, na.rm = TRUE)) %>%
  pivot_wider(Address, 
              names_from = "Table", 
              values_from = c("cost","cost_r"), 
              values_fill = list(cost = 0, cost_r = 0))

u98p3 <- U98P3 %>%
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
    table == 13 ~ "durables",
    table == 14 ~ "investment",
    TRUE ~ NA_character_)) %>%
  group_by(Address,Table) %>%
  summarize(cost = sum(value, na.rm = TRUE),
            cost_r = sum(value_r, na.rm = TRUE)) %>%
  pivot_wider(Address, 
              names_from = "Table", 
              values_from = c("cost","cost_r"), 
              values_fill = list(cost = 0, cost_r = 0))

# Non-monetary household income
r_NM_housing <- R98P3S04 %>%
  filter(DYCOL00 %/% 1000 == 42) %>%
  group_by(Address) %>%
  summarize(income_nm_house = sum(value*12, na.rm = T))

u_NM_housing <- U98P3S04 %>%
  filter(DYCOL00 %/% 1000 == 42) %>%
  group_by(Address) %>%
  summarize(income_nm_house = sum(value*12, na.rm = T))

r_NMincome <- R98P3 %>%
  mutate(type = case_when(
    purchased %in% c("publicservice","cooperativeservice") ~ "public",
    purchased == "privateservice" ~ "private",
    purchased == "agriculture" ~ "agriculture",
    purchased == "nonagriculture" ~ "nonagriculture", 
    purchased %in% c("free","homemade") ~ as.character(purchased),
    TRUE ~ NA_character_),
    recallperiod=ifelse(table>12,1,12)) %>%
  group_by(Address, type) %>%
  summarize(value = sum(value*recallperiod, na.rm = T)) %>%
  filter(!is.na(type)&value!=0) %>% 
  pivot_wider(Address, 
              names_from="type", names_prefix = "income_nm_", 
              values_from = "value",  values_fill = list(value = 0)) %>%
  full_join(r_NM_housing) 
r_NMincome[is.na(r_NMincome)] <- 0

u_NMincome <- U98P3 %>%
  mutate(type = case_when(
    purchased %in% c("publicservice","cooperativeservice") ~ "public",
    purchased == "privateservice" ~ "private",
    purchased == "agriculture" ~ "agriculture",
    purchased == "nonagriculture" ~ "nonagriculture", 
    purchased %in% c("free","homemade") ~ as.character(purchased),
    TRUE ~ NA_character_),
    recallperiod=ifelse(table>12,1,12)) %>%
  group_by(Address, type) %>%
  summarize(value = sum(value*recallperiod, na.rm = T)) %>%
  filter(!is.na(type)&value!=0) %>% 
  pivot_wider(Address, 
              names_from="type", names_prefix = "income_nm_", 
              values_from = "value", values_fill = list(value = 0)) %>%
  full_join(u_NM_housing) 
u_NMincome[is.na(u_NMincome)] <- 0


  
# # sum of household income
# r_incomeSum <- Rind98 %>%
#   group_by(Address) %>%
#   summarise(across(c(starts_with(c("income","netincome")),"subsidy"), ~sum(.x,na.rm = T)))
# u_incomeSum <- Uind98 %>%
#   group_by(Address) %>%
#   summarise(across(c(starts_with(c("income","netincome")),"subsidy"), ~sum(.x,na.rm = T)))

# sum of household income with data.table
DT <- data.table(Rind98)
DT <- DT[,.('income_w_y'= sum(income_w_y, na.rm = T),
            'income_s_y'= sum(income_s_y, na.rm = T),
            'netincome_w_m'= sum(netincome_w_m, na.rm = T),
            'netincome_w_y'= sum(income_w_y, na.rm = T),
            'income_pension'= sum(income_pension, na.rm = T),
            'income_rent'= sum(income_rent, na.rm = T),
            'income_interest'= sum(income_interest, na.rm = T),
            'income_aid'= sum(income_aid, na.rm = T),
            'income_resale'= sum(income_resale, na.rm = T),
            'income_transfer'= sum(income_transfer, na.rm = T),
            'subsidy'= sum(subsidy, na.rm = T)
            ),
         by=.(Address)]
r_incomeSum <- as.data.frame(DT) 

DT <- data.table(Uind98)
DT <- DT[,.('income_w_y'= sum(income_w_y, na.rm = T),
            'income_s_y'= sum(income_s_y, na.rm = T),
            'netincome_w_m'= sum(netincome_w_m, na.rm = T),
            'netincome_w_y'= sum(income_w_y, na.rm = T),
            'income_pension'= sum(income_pension, na.rm = T),
            'income_rent'= sum(income_rent, na.rm = T),
            'income_interest'= sum(income_interest, na.rm = T),
            'income_aid'= sum(income_aid, na.rm = T),
            'income_resale'= sum(income_resale, na.rm = T),
            'income_transfer'= sum(income_transfer, na.rm = T),
            'subsidy'= sum(subsidy, na.rm = T)
            ),
         by=.(Address)]
u_incomeSum <- as.data.frame(DT)

# merging household-level data
RHH98 <- r98data %>% 
  mutate(urban = "R") %>%
  left_join(r98p1, by="Address") %>%
  left_join(r_head, by = "Address") %>%
  left_join(r_job, by = "Address") %>%
  left_join(r98p3, by = "Address") %>%
  left_join(r_incomeSum, by = "Address") %>%
  left_join(r_NMincome, by = "Address") %>%
  left_join(R98P2) %>%
  mutate(across(income_w_y:income_nm_house, ~replace_na(.x, 0)))

UHH98 <- u98data %>% 
  mutate(urban = "U") %>%
  left_join(u98p1, by="Address") %>%
  left_join(u_head, by = "Address") %>%
  left_join(u_job, by = "Address") %>%
  left_join(u98p3, by = "Address") %>%
  left_join(u_incomeSum, by = "Address") %>%
  left_join(u_NMincome, by = "Address") %>%
  left_join(U98P2) %>%
  mutate(across(income_w_y:income_nm_house, ~replace_na(.x, 0)))

HH98 <- bind_rows(RHH98, UHH98) %>%
  mutate(urban = as.factor(urban)) %>%
  mutate(expenditure = cost_food + cost_tobacco + cost_clothing + cost_housing + cost_appliances + cost_health + cost_transport + cost_communication + cost_recreation +  cost_restaurant + cost_miscellaneous
              + cost_durables/12 + cost_investment/12,
         income = income_s_y + netincome_w_y + income_pension + income_rent + income_interest + income_aid + income_transfer + subsidy + income_nm_agriculture + income_nm_free + income_nm_homemade + income_nm_public + income_nm_private + income_nm_nonagriculture + income_nm_house,
         DYCOL00 = NA_integer_ 
         ) %>%
  left_join(month) %>%
  left_join(CPI) %>%
  select(Address:income, cpi_m = cpi, cpi_y) %>%
  as.data.frame()

attr(HH98$size, "label") <- "Household size"
attr(HH98$literates, "label") <- "Number of literate members"
attr(HH98$students, "label") <- "Number of student members"
attr(HH98$employeds, "label") <- "Number of employed members"
attr(HH98$gender, "label") <- "Head's gender"
attr(HH98$age, "label") <- "Head's age"
attr(HH98$literacy, "label") <- "Head's literacy"
attr(HH98$education, "label") <- "Head's education"
attr(HH98$occupationalst, "label") <- "Head's job status"
attr(HH98$maritalst, "label") <- "Head's marital status"
attr(HH98$ISIC_w, "label") <- "Head's Industry code of wage-earning job"
attr(HH98$ISCO_w, "label") <- "Head's Occupation code of wage-earning job"
attr(HH98$ISIC_s, "label") <- "Head's Industry code of non-wage job"
attr(HH98$ISCO_s, "label") <- "Head's Occupation code of non-wage job"
attr(HH98$income_nm_agriculture, "label") <- "Non-monetary income from agriculture"
attr(HH98$income_nm_nonagriculture, "label") <- "Non-monetary income from nonagriculture"
attr(HH98$income_nm_public, "label") <- "Non-monetary income from public sector job"
attr(HH98$income_nm_private, "label") <- "Non-monetary income from private sector job"
attr(HH98$income_nm_homemade, "label") <- "Non-monetary income from home production"
attr(HH98$income_nm_free, "label") <- "Non-monetary income from free goods/services"
attr(HH98$netincome_w_m, "label") <- "wage-earning job: net income previous month"
attr(HH98$netincome_w_y, "label") <- "wage-earning job: net income previous year"
attr(HH98$income_w_y, "label") <- "wage-earning job: gross income previous year"
attr(HH98$income_s_y, "label") <- "non-wage job: net income previous year"
attr(HH98$income, "label") <- "total household income in previous year"
attr(HH98$expenditure, "label") <- "total monthly expendiuture of the household"

saveRDS(HH98, file = "./exported/HH98.Rds")


#######################################
# Exporting Rds files to CSV, STATA, etc.

#install.packages("haven") # uncomment if not already installed

df <- readRDS("./exported/HH98.Rds") # Specify the Rds file to convert here

#haven::write_dta(df, "./exported/HH98.dta") # to export in STATA
#write_csv2(df, "./exported/HH98.csv") # to export in CSV
