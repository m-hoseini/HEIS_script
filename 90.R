library(tidyverse)
library(data.table)
library(readxl)

rm(list = ls())

load("./RAW/90.RData")

##############################
Province <- c(Markazi="00", Gilan="01", Mazandaran="02", AzarbaijanSharghi="03", AzarbaijanGharbi="04",
              Kermanshah="05", Kouzestan="06", Fars="07", Kerman="08", KhorasanRazavi="09",
              Esfahan="10", SistanBalouchestan="11", Kordestan="12", Hamedan="13", CharmahalBakhtiari="14",
              Lorestan="15", Ilam="16", KohkilouyeBoyerahamad="17", Boushehr="18", Zanjan="19", 
              Semnan="20", Yazd="21", Hormozgan="22", Tehran="23", Ardebil="24", Qom="25", Qazvin="26",
              Golestan="27", KhorasanShomali="28", KhorasanJonoubi="29", Alborz="30")


R90_weight<-Sum_R90%>%select(ADDRESS, weight)%>%rename(Address = ADDRESS)%>%mutate_if(is.character, as.numeric)
R90Data <- R90Data %>% 
  rename(month = MahMorajeh, khanevartype = NoeKhn) %>%
  mutate(province = fct_recode(as.factor(substr(Address, 2, 3)), !!!Province),
         town = as.integer(substr(Address, 4, 5)))%>%
  left_join(R90_weight)


U90_weight<-Sum_U90%>%select(ADDRESS, weight)%>%rename(Address = ADDRESS)%>%mutate_if(is.character, as.numeric)
U90Data <- U90Data %>% 
  rename(month = MahMorajeh, khanevartype = NoeKhn) %>%
  mutate(province = fct_recode(as.factor(substr(Address, 2, 3)), !!!Province),
         town = as.integer(substr(Address, 4, 5)))%>%
  left_join(U90_weight)

##############################
# Part 1


relation <- c(Head="1", Spouse="2", Child="3", SonDaughter_inLaw="4", GrandSonDaughter="5", Parent="6", Sibling="7", OtherRelative="8", NonRelative="9")
gender <- c(Male="1", Female="2")
literacy <- c(literate="1", illiterate="2")
yesno <- c(Yes="1", No="2")
education <- c(Elemantry="1", Secondary="2", HighSchool="3", Diploma="4", College="5", Bachelor="6", Master="7", PhD="8", Other="9")
occupation <- c(employed="1", unemployed="2", IncomeWOJob="3", Student="4", Housewife="5", Other="6")
marital <- c(Married ="1", Widowed="2", Divorced="3", Single="4")

R90P1$DYCOL08 <- as.numeric(substr(sprintf("%03d", as.numeric(levels(R90P1$DYCOL08))[R90P1$DYCOL08]),1,1))
R90P1 <- R90P1 %>% 
  rename(
    member = DYCOL01,
    relation = DYCOL03,
    gender = DYCOL04,
    age = DYCOL05,
    literacy = DYCOL06,
    studying = DYCOL07,
    degree = DYCOL08,
    occupationalst = DYCOL09,
    maritalst = DYCOL10) %>% 
  mutate(across(where(is.character), as.integer),
         across(c(relation,gender,literacy,studying,degree,occupationalst,maritalst), as.factor),
         relation = fct_recode(relation, !!!relation), 
         gender = fct_recode(gender, !!!gender),
         literacy = fct_recode(literacy, !!!literacy), 
         studying = fct_recode(studying, !!!yesno),
         degree = fct_recode(degree, !!!education), 
         occupationalst = fct_recode(occupationalst, !!!occupation),
         maritalst = fct_recode(maritalst, !!!marital))

U90P1$DYCOL08 <- as.numeric(substr(sprintf("%03d", as.numeric(levels(U90P1$DYCOL08))[U90P1$DYCOL08]),1,1))
U90P1 <- U90P1 %>% 
  rename(
    member = DYCOL01,
    relation = DYCOL03,
    gender = DYCOL04,
    age = DYCOL05,
    literacy = DYCOL06,
    studying = DYCOL07,
    degree = DYCOL08,
    occupationalst = DYCOL09,
    maritalst = DYCOL10) %>% 
  mutate(across(where(is.character), as.integer),
         across(c(relation,gender,literacy,studying,degree,occupationalst,maritalst), as.factor),
         relation = fct_recode(relation, !!!relation), 
         gender = fct_recode(gender, !!!gender),
         literacy = fct_recode(literacy, !!!literacy), 
         studying = fct_recode(studying, !!!yesno),
         degree = fct_recode(degree, !!!education), 
         occupationalst = fct_recode(occupationalst, !!!occupation),
         maritalst = fct_recode(maritalst, !!!marital))

#######################################################################
# Part 2
tenure <- c(OwnedEstateLand="1", OwnedEstate="2", Rent="3", Mortgage="4", Service="5", Free="6", Other="7")
material <- c(MetalBlock="1", BrickWood="2", Cement="3", Brick="4", Wood="5", WoodKesht="6", KeshtGel="7", Other="8")
fuel <- c(Oil="1", Gasoline="2", LiquidGas="3", NaturalGas="4", Electricity="5", Wood="6", AnimalOil="7", Coke="8", Other="9", None="10" )
fuel1 <- c(Oil="11", Gasoline="12", LiquidGas="13", NaturalGas="14", Electricity="15", Wood="16", AnimalOil="17", Coke="18", Other="19", None="20" )
fuel2 <- c(Oil="21", Gasoline="22", LiquidGas="23", NaturalGas="24", Electricity="25", Wood="26", AnimalOil="27", Coke="28", Other="29", None="30" )



# Part 2

R90P2 <- R90P2 %>%
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

U90P2 <- U90P2 %>%
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

###################################
# Part 3, Table 1

month <- R90Data %>%
  bind_rows(U90Data) %>%
  select(Address,month) %>%
  mutate(year=1390)

CPI <- readRDS("CPI/CPI.rds")

# Part 3, Table 1

R90P3S01$DYCOL04 <- as.numeric(levels(R90P3S01$DYCOL04))[R90P3S01$DYCOL04]
R90P3S01$DYCOL05 <- as.numeric(levels(R90P3S01$DYCOL05))[R90P3S01$DYCOL05]
R90P3S01$DYCOL06 <- as.numeric(levels(R90P3S01$DYCOL06))[R90P3S01$DYCOL06]

R90P3S01 <- R90P3S01 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    gram = DYCOL03,
    kilogram = DYCOL04,
    price = DYCOL05,
    value = DYCOL06
  )%>%
  mutate(DYCOL00 = case_when(
    code == 11319 ~ 11325L,
    code == 11321 ~ 11326L,
    code == 11322 ~ 11327L,
    code == 11323 ~ 11328L,
    TRUE ~ code),
    across(c(price,value,kilogram),  ~ as.numeric(as.character(.x)) ),
    table = 1L)%>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)

U90P3S01$DYCOL04 <- as.numeric(levels(U90P3S01$DYCOL04))[U90P3S01$DYCOL04]
U90P3S01$DYCOL05 <- as.numeric(levels(U90P3S01$DYCOL05))[U90P3S01$DYCOL05]
U90P3S01$DYCOL06 <- as.numeric(levels(U90P3S01$DYCOL06))[U90P3S01$DYCOL06]

U90P3S01 <- U90P3S01 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    gram = DYCOL03,
    kilogram = DYCOL04,
    price = DYCOL05,
    value = DYCOL06
  )%>%
  mutate(DYCOL00 = case_when(
    code == 11319 ~ 11325L,
    code == 11321 ~ 11326L,
    code == 11322 ~ 11327L,
    code == 11323 ~ 11328L,
    TRUE ~ code),
    across(c(price,value,kilogram),  ~ as.numeric(as.character(.x)) ),
    table = 1L)%>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)


# Part 3, Table 2

R90P3S02 <- R90P3S02 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    gram = DYCOL03,
    kilogram = DYCOL04,
    price = DYCOL05,
    value = DYCOL06
  )%>%
  mutate( DYCOL00 = case_when(
    code == 22111 ~ 22112L,
    code == 22112 ~ 22116L,
    TRUE ~ code),
    table = 2L)%>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)
    
U90P3S02 <- U90P3S02 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    gram = DYCOL03,
    kilogram = DYCOL04,
    price = DYCOL05,
    value = DYCOL06
  )%>%
  mutate( DYCOL00 = case_when(
    code == 22111 ~ 22112L,
    code == 22112 ~ 22116L,
    TRUE ~ code),
    table = 2L)%>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)

# Part 3, Table 3

R90P3S03$DYCOL03 <- as.numeric(levels(R90P3S03$DYCOL03))[R90P3S03$DYCOL03]
R90P3S03 <- R90P3S03 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03,
  )%>%
  mutate(DYCOL00 = case_when(
    code == 31239 ~ 31244L,
    code == 31241 ~ 31245L,
    code == 31243 ~ 31253L,
    code == 31244 ~ 31255L,
    code == 31258 ~ 31259L,
    code == 31259 ~ 31264L,
    code == 31269 ~ 31263L,
    TRUE ~ code),
    table = 3L)%>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)

U90P3S03$DYCOL03 <- as.numeric(levels(U90P3S03$DYCOL03))[U90P3S03$DYCOL03]
U90P3S03 <- U90P3S03 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03,
  )%>%
  mutate(DYCOL00 = case_when(
    code == 31239 ~ 31244L,
    code == 31241 ~ 31245L,
    code == 31243 ~ 31253L,
    code == 31244 ~ 31255L,
    code == 31258 ~ 31259L,
    code == 31259 ~ 31264L,
    code == 31269 ~ 31263L,
    TRUE ~ code),
    table = 3L)%>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)


# Part 3, Table 4

R90P3S04 <- R90P3S04 %>%
  rename(
    code = DYCOL01,
    mortgage = DYCOL02,
    purchased = DYCOL03,
    value = DYCOL04
  )%>%
  mutate(DYCOL00 = case_when(
    code == 44418 ~ 44419L,
    TRUE ~ code),
    table = 4L)%>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)

U90P3S04$DYCOL04 <- as.numeric(levels(U90P3S04$DYCOL04))[U90P3S04$DYCOL04]
U90P3S04 <- U90P3S04 %>%
  rename(
    code = DYCOL01,
    mortgage = DYCOL02,
    purchased = DYCOL03,
    value = DYCOL04
  )%>%
  mutate(DYCOL00 = case_when(
    code == 44418 ~ 44419L,
    TRUE ~ code),
    table = 4L)%>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)

# Part 3, Table 5

R90P3S05 <- R90P3S05 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03,
  ) %>%
  mutate( DYCOL00 = case_when(
    code == 52123 ~ 52126L,
    TRUE ~ code),
    table = 5L)%>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)

U90P3S05 <- U90P3S05 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03,
  ) %>%
  mutate( DYCOL00 = case_when(
    code == 52123 ~ 52126L,
    TRUE ~ code),
    table = 5L)%>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)

# Part 3, Table 6

R90P3S06 <- R90P3S06  %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03,
  ) %>%
  mutate(DYCOL00 = case_when(
    code == 61214 ~ 61213L,
    code == 61335 ~ 61338L,
    code == 62329 ~ 62349L,
    TRUE ~ code),
    table = 6L)%>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)

U90P3S06 <- U90P3S06  %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03,
  ) %>%
  mutate(DYCOL00 = case_when(
    code == 61214 ~ 61213L,
    code == 61335 ~ 61338L,
    code == 62329 ~ 62349L,
    TRUE ~ code),
    table = 6L)%>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)

# Part 3, Table 7

R90P3S07$DYCOL03 <- as.numeric(levels(R90P3S07$DYCOL03))[R90P3S07$DYCOL03]

R90P3S07 <- R90P3S07 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03,
  ) %>%
  mutate(DYCOL00 = case_when(
    code == 73611 ~ 73615L,
    TRUE ~ code),
    table = 7L)%>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)

U90P3S07 <- U90P3S07 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03,
  ) %>%
  mutate(DYCOL00 = case_when(
    code == 73611 ~ 73615L,
    TRUE ~ code),
    table = 7L)%>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)


# Part 3, Table 8 

R90P3S08 <- R90P3S08%>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03,
  ) %>%
  mutate(DYCOL00 = code,
         table = 8L)%>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)

U90P3S08 <- U90P3S08%>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03,
  ) %>%
  mutate(DYCOL00 = code,
         table = 8L)%>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)

# Part 3, Table 9

R90P3S09 <- R90P3S09 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03,
  )%>%
  mutate(DYCOL00 = code,
         table = 9L)%>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)

U90P3S09$DYCOL03 <- as.numeric(levels(U90P3S09$DYCOL03))[U90P3S09$DYCOL03]

U90P3S09 <- U90P3S09 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03,
  )%>%
  mutate(DYCOL00 = code,
         table = 9L)%>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)


# Part 3, Table 10

R90P3S10 <- R90P3S10 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03,
  ) %>%
  mutate(DYCOL00 = code) %>%
  mutate_if(is.character, as.numeric)

U90P3S10 <- U90P3S10 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03,
  ) %>%
  mutate(DYCOL00 = code) %>%
  mutate_if(is.character, as.numeric)

# Part 3, Table 11


R90P3S11 <- R90P3S11 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03,
  ) %>%
  mutate(DYCOL00 = code,
         table = 11L)%>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)

U90P3S11 <- U90P3S11 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03,
  )%>%
  mutate(DYCOL00 = code,
         table = 11L)%>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)


# Part 3, Table 12

R90P3S12 <- R90P3S12 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03,
  )%>%
  mutate(DYCOL00 = code,
         table = 12L)%>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)

U90P3S12 <- U90P3S12 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    value = DYCOL03,
  )%>%
  mutate(DYCOL00 = code,
         table = 12L)%>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi) %>%
  select(Address:table, value_r)
      
# Part 3, Table 13

R90P3S13$DYCOL05 <- as.numeric(levels(R90P3S13$DYCOL05))[R90P3S13$DYCOL05]
R90P3S13$DYCOL03 <- as.numeric(levels(R90P3S13$DYCOL03))[R90P3S13$DYCOL03]

R90P3S13 <- R90P3S13 %>%
  rename(
    code = DYCOL01,
    insured_loan = DYCOL02,
    loanfrom = DYCOL03,
    purchased = DYCOL04,
    cost = DYCOL05,
    sell = DYCOL06,
  ) %>%
  mutate(DYCOL00 = case_when(
    code == 51161 ~ 51160L,
    TRUE ~ code),
    cost = replace_na(as.numeric(as.character(cost)),0),
    sell = replace_na(as.numeric(as.character(sell)),0),
    value = cost - sell,
    table = 13L) %>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi_y) %>%
  select(Address:table, value_r)

U90P3S13$DYCOL03 <- as.numeric(levels(U90P3S13$DYCOL03))[U90P3S13$DYCOL03]
U90P3S13$DYCOL05 <- as.numeric(levels(U90P3S13$DYCOL05))[U90P3S13$DYCOL05]

U90P3S13 <- U90P3S13 %>%
  rename(
    code = DYCOL01,
    insured_loan = DYCOL02,
    loanfrom = DYCOL03,
    purchased = DYCOL04,
    cost = DYCOL05,
    sell = DYCOL06,
  ) %>%
  mutate(DYCOL00 = case_when(
    code == 51161 ~ 51160L,
    TRUE ~ code),
    cost = replace_na(as.numeric(as.character(cost)),0),
    sell = replace_na(as.numeric(as.character(sell)),0),
    value = cost - sell,
    table = 13L) %>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi_y) %>%
  select(Address:table, value_r)


# Part 3, Table 14

R90P3S14 <- R90P3S14 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    cost = DYCOL03,
    sell = DYCOL04
  )%>%
  mutate(DYCOL00 = code,
         cost = replace_na(cost,0),
         sell = replace_na(sell,0),
         value = cost - sell,
         table = 14L) %>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi_y) %>%
  select(Address:table, value_r)

U90P3S14 <- U90P3S14 %>%
  rename(
    code = DYCOL01,
    purchased = DYCOL02,
    cost = DYCOL03,
    sell = DYCOL04
  )%>%
  mutate(DYCOL00 = code,
         cost = replace_na(cost,0),
         sell = replace_na(sell,0),
         value = cost - sell,
         table = 14L) %>%
  left_join(month) %>%
  left_join(CPI) %>%
  mutate(value_r=value*100/cpi_y) %>%
  select(Address:table, value_r)

#############################
# Part 4, Table 1

R90P4S01 <- R90P4S01 %>%
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
    netincome_w_y = DYCOL15)%>%
  left_join(month) %>%
  mutate(DYCOL00= NA_integer_) %>%
  left_join(CPI) %>%
  select(Address:netincome_w_y, cpi_m = cpi, cpi_y)

U90P4S01$DYCOL08 <- as.numeric(levels(U90P4S01$DYCOL08))[U90P4S01$DYCOL08]
U90P4S01 <- U90P4S01 %>%
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
    netincome_w_y = DYCOL15)%>%
  left_join(month) %>%
  mutate(DYCOL00= NA_integer_) %>%
  left_join(CPI) %>%
  select(Address:netincome_w_y, cpi_m = cpi, cpi_y)



# Part 4, Table 2
R90P4S02$DYCOL15 <- as.numeric(levels(R90P4S02$DYCOL15))[R90P4S02$DYCOL15]
R90P4S02$DYCOL03 <- as.numeric(levels(R90P4S02$DYCOL03))[R90P4S02$DYCOL03]
R90P4S02$DYCOL04 <- as.numeric(levels(R90P4S02$DYCOL04))[R90P4S02$DYCOL04]
R90P4S02$DYCOL10 <- as.numeric(levels(R90P4S02$DYCOL10))[R90P4S02$DYCOL10]
R90P4S02 <- R90P4S02 %>%
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
    income_s_y = DYCOL15)%>%
  left_join(month) %>%
  mutate(DYCOL00= NA_integer_) %>%
  left_join(CPI) %>%
  select(Address:income_s_y, cpi_m = cpi, cpi_y)

U90P4S02$DYCOL15 <- as.numeric(levels(U90P4S02$DYCOL15))[U90P4S02$DYCOL15]
U90P4S02$DYCOL12 <- as.numeric(levels(U90P4S02$DYCOL12))[U90P4S02$DYCOL12]

U90P4S02 <- U90P4S02 %>%
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
    income_s_y = DYCOL15)%>%
  left_join(month) %>%
  mutate(DYCOL00= NA_integer_) %>%
  left_join(CPI) %>%
  select(Address:income_s_y, cpi_m = cpi, cpi_y)


# Part 4, Table 3
R90P4S03 <- R90P4S03 %>%
  rename(
    member = DYCOL01,
    income_pension = DYCOL03,
    income_rent = DYCOL04,
    income_interest = DYCOL05,
    income_aid = DYCOL06,
    income_resale = DYCOL07,
    income_transfer = DYCOL08)%>%
  left_join(month) %>%
  mutate(DYCOL00= NA_integer_) %>%
  left_join(CPI) %>%
  select(Address:income_transfer, cpi_y)


U90P4S03 <- U90P4S03 %>%
  rename(
    member = DYCOL01,
    income_pension = DYCOL03,
    income_rent = DYCOL04,
    income_interest = DYCOL05,
    income_aid = DYCOL06,
    income_resale = DYCOL07,
    income_transfer = DYCOL08)%>%
  left_join(month) %>%
  mutate(DYCOL00= NA_integer_) %>%
  left_join(CPI) %>%
  select(Address:income_transfer, cpi_y)


# Part 4, Table 4

R90P4S04 <- R90P4S04 %>%
  rename(
    member = Dycol01,
    subsidy_number = Dycol03,
    subsidy_month = Dycol04,
    subsidy = Dycol05)%>%
  left_join(month) %>%
  mutate(DYCOL00= NA_integer_) %>%
  left_join(CPI) %>%
  select(Address:subsidy, cpi_y)


U90P4S04 <- U90P4S04 %>%
  rename(
    member = Dycol01,
    subsidy_number = Dycol03,
    subsidy_month = Dycol04,
    subsidy = Dycol05)%>%
  left_join(month) %>%
  mutate(DYCOL00= NA_integer_) %>%
  left_join(CPI) %>%
  select(Address:subsidy, cpi_y)


rm(list = setdiff(ls(), ls(pattern = "90"))) # removing unnecessary objects

save.image(file="./exported/HEIS90.Rdata")

month <- R90Data %>%
  bind_rows(U90Data) %>%
  select(Address,month) %>%
  mutate(year=1390)

CPI <- readRDS("CPI/CPI.rds")


###############################################
# Item-level expenditure table
# Merging tables in a unique expenditure table

R90P3 <- bind_rows(mget(ls(pattern = "R90P3S.*")))
R90P3 <- R90P3 %>%
  left_join(R90Data) %>%
  mutate(urban = "R")

U90P3 <- bind_rows(mget(ls(pattern = "U90P3S.*")))
U90P3 <- U90P3 %>%
  left_join(U90Data) %>%
  mutate(urban = "U")

itemcode <- read_excel("itemlabels.xlsx") %>%
  filter(!is.na(Global)) %>%
  mutate(gcode=ifelse(is.na(G2),Global,Global*100+G2)) %>%
  select(gcode, Label, LabelFA)
itemcode$item <- factor(itemcode$gcode, levels = itemcode$gcode, labels = itemcode$Label)

EXP90 <- bind_rows(R90P3,U90P3)%>%
  rename(gcode = DYCOL00) %>%
  mutate(urban = as.factor(urban), 
         recallperiod=ifelse(table>12,1/12,1)) %>%
  group_by(table, gcode, code, urban) %>%
  summarize(Value = sum(value*weight*recallperiod, na.rm = T),
            Value_r = sum(value_r*weight*recallperiod, na.rm = T),
            Kilogram = sum(kilogram*weight, na.rm = T),
            Gram = sum(gram*weight, na.rm = T),
            Price = median(price, na.rm = T)) %>%
  filter(Value!=0) %>%
  left_join(itemcode) %>%
  select(-Label, -LabelFA) %>%
  as.data.frame()

attr(EXP90$table, "label") <- "table number in Part 3"
attr(EXP90$gcode, "label") <- "global item code"
attr(EXP90$code, "label") <- "item code in this year"
attr(EXP90$urban, "label") <- "rural or urban"
attr(EXP90$Price, "label") <- "median price"
attr(EXP90$Value, "label") <- "monthly expenditure"
attr(EXP90$Value_r, "label") <- "monthly expenditure in 1390 price"

saveRDS(EXP90, "./exported/EXP90.Rds")

###############################################
# Building individual level data

r90data<- R90Data %>% select(Address, month, weight, khanevartype, Jaygozin, province)
u90data<- U90Data %>% select(Address, month, weight, khanevartype, Jaygozin, province)

# faster code with data.table
DT <- data.table(R90P4S01)
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
R90P4S01_unique <- as.data.frame(DT) %>%
  mutate(across(c(hours_w,days_w), ~ifelse(.x == 0,NA,.x)))

DT <- data.table(R90P4S02)
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

R90P4S02_unique <- as.data.frame(DT) %>%
  mutate(across(c(hours_s,days_s), ~ifelse(.x == 0,NA,.x)))

DT <- data.table(R90P4S03)
DT <- DT[,.('income_pension'= sum(income_pension, na.rm = T),
            'income_rent'= sum(income_rent, na.rm = T),
            'income_interest'= sum(income_interest, na.rm = T),
            'income_aid'= sum(income_aid, na.rm = T),
            'income_resale'= sum(income_resale, na.rm = T),
            'income_transfer'= sum(income_transfer, na.rm = T)
),
by=.(Address, member)]
R90P4S03_unique <- as.data.frame(DT) 


DT <- data.table(R90P4S04)
DT <- DT[,.('subsidy_number'= sum(subsidy_number, na.rm = T),
            'subsidy_month'= sum(subsidy_month, na.rm = T),
            'subsidy'= sum(subsidy, na.rm = T)
),
by=.(Address, member)]
R90P4S04_unique <- as.data.frame(DT) 


DT <- data.table(U90P4S01)
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
U90P4S01_unique <- as.data.frame(DT) %>%
  mutate(across(c(hours_w,days_w), ~ifelse(.x == 0,NA,.x)))


DT <- data.table(U90P4S02)
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
U90P4S02_unique <- as.data.frame(DT) %>%
  mutate(across(c(hours_s,days_s), ~ifelse(.x == 0,NA,.x)))

DT <- data.table(U90P4S03)
DT <- DT[,.('income_pension'= sum(income_pension, na.rm = T),
            'income_rent'= sum(income_rent, na.rm = T),
            'income_interest'= sum(income_interest, na.rm = T),
            'income_aid'= sum(income_aid, na.rm = T),
            'income_resale'= sum(income_resale, na.rm = T),
            'income_transfer'= sum(income_transfer, na.rm = T)
),
by=.(Address, member)]
U90P4S03_unique <- as.data.frame(DT) 

DT <- data.table(U90P4S04)
DT <- DT[,.('subsidy_number'= sum(subsidy_number, na.rm = T),
            'subsidy_month'= sum(subsidy_month, na.rm = T),
            'subsidy'= sum(subsidy, na.rm = T)
),
by=.(Address, member)]
U90P4S04_unique <- as.data.frame(DT) 


Rind90 <- r90data  %>%
  left_join(R90P1) %>%
  left_join(R90P4S01_unique) %>%
  left_join(R90P4S02_unique) %>%
  left_join(R90P4S03_unique) %>% 
  left_join(R90P4S04_unique) %>%
  mutate(across(where(is.character),as.integer)) %>%
  mutate(urban = "R")

Uind90 <- u90data %>%
  left_join(U90P1) %>%
  left_join(U90P4S01_unique) %>%
  left_join(U90P4S02_unique) %>%
  left_join(U90P4S03_unique) %>% 
  left_join(U90P4S04_unique) %>%
  mutate(across(where(is.character),as.integer)) %>%
  mutate(urban = "U")


IND90 <- bind_rows(Rind90,Uind90)%>%
  mutate(urban = as.factor(urban),
         DYCOL00 = NA_integer_ ) %>%
  left_join(month) %>%
  left_join(CPI) %>%
  select(Address:urban, cpi_m = cpi, cpi_y) %>%
  as.data.frame()

attr(IND90$occupationalst, "label") <- "Job status"
attr(IND90$employed_w, "label") <- "Whether employed in wage-earning job?"
attr(IND90$ISIC_w, "label") <- "Industry code of wage-earning job"
attr(IND90$ISCO_w, "label") <- "Occupation code of wage-earning job"
attr(IND90$status_w, "label") <- "Wage-earning job status: 1-public 2-cooperative 3-private"
attr(IND90$hours_w, "label") <- "Wage-earning job: hours per day"
attr(IND90$days_w, "label") <- "Wage-earning job: day per week"
attr(IND90$netincome_w_m, "label") <- "wage-earning job: net income previous month"
attr(IND90$netincome_w_y, "label") <- "wage earning job: net income previous year"
attr(IND90$employed_s, "label") <- "Whether employed in non-wage job?"
attr(IND90$ISIC_s, "label") <- "Industry code of non-wage job"
attr(IND90$ISCO_s, "label") <- "Occupation code of non-wage job"
attr(IND90$status_s, "label") <- "job status: 4-employer 2-selfemployed 3-familyworker"
attr(IND90$hours_s, "label") <- "non-wage job: hours per day"
attr(IND90$days_s, "label") <- "non-wage job: day per week"
attr(IND90$income_s_y, "label") <- "non-wage job: net income previous year"

saveRDS(IND90,"./exported/IND90.Rds")

###############################################
# Building Household Data

r90p1 <- R90P1 %>%
  group_by(Address) %>%
  summarize(size = sum(!is.na(member)),
            literates = sum(literacy == "literate", na.rm = TRUE),
            students = sum(studying == "Yes", na.rm = TRUE),
            employeds = sum(occupationalst == "employed", na.rm = TRUE)) 

u90p1 <- U90P1 %>%
  group_by(Address) %>%
  summarize(size = sum(!is.na(member)),
            literates = sum(literacy == "literate", na.rm = TRUE),
            students = sum(studying == "Yes", na.rm = TRUE),
            employeds = sum(occupationalst == "employed", na.rm = TRUE)) 

r_head <- R90P1 %>%
  filter(relation == "Head") %>%
  select(-member,-relation,-studying)

u_head <- U90P1 %>%
  filter(relation == "Head") %>%
  select(-member,-relation,-studying)

# summary of heads job codes
r_job <- Rind90 %>%
  filter(relation == "Head") %>%
  select(Address,starts_with("IS"))

u_job <- Uind90 %>%
  filter(relation == "Head") %>%
  select(Address,starts_with("IS"))

# Sum of household expenditure items
r90p3 <- R90P3 %>%
  mutate(Table = case_when(
    table == 1 ~ "food",
    table == 2 ~ "tobacco",
    table == 3 ~ "clothing",
    table == 4 ~ "housing",
    table == 5 ~ "furniture",
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

u90p3 <- U90P3 %>%
  mutate(Table = case_when(
    table == 1 ~ "food",
    table == 2 ~ "tobacco",
    table == 3 ~ "clothing",
    table == 4 ~ "housing",
    table == 5 ~ "furniture",
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
r_NM_housing <- R90P3S04 %>%
  filter(DYCOL00 %/% 1000 == 42) %>%
  group_by(Address) %>%
  summarize(income_nm_house = sum(value, na.rm = T))

u_NM_housing <- U90P3S04 %>%
  filter(DYCOL00 %/% 1000 == 42) %>%
  group_by(Address) %>%
  summarize(income_nm_house = sum(value, na.rm = T))

r_NMincome <- R90P3 %>%
  mutate(type = case_when(
    purchased %in% 3:4 ~ "public",
    purchased == 5 ~ "private",
    purchased == 6 ~ "agriculture",
    purchased == 7 ~ "nonagriculture", 
    purchased %in% c(2,8) ~ "miscellaneous",
    TRUE ~ NA_character_)) %>%
  group_by(Address, type) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  filter(!is.na(type)&value!=0) %>% 
  pivot_wider(Address, 
              names_from="type", names_prefix = "income_nm_", 
              values_from = "value",  values_fill = list(value = 0)) %>%
  full_join(r_NM_housing) 
r_NMincome[is.na(r_NMincome)] <- 0

u_NMincome <- U90P3 %>%
  mutate(type = case_when(
    purchased %in% 3:4 ~ "public",
    purchased == 5 ~ "private",
    purchased == 6 ~ "agriculture",
    purchased == 7 ~ "nonagriculture", 
    purchased %in% c(2,8) ~ "miscellaneous",
    TRUE ~ NA_character_)) %>%
  group_by(Address, type) %>%
  summarize(value = sum(value, na.rm = T)) %>%
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

DT <- data.table(Rind90)
DT <- DT[,.('income_w_y'= sum(income_w_m, na.rm = T),
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

DT <- data.table(Uind90)
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
RHH90 <- r90data %>% 
  mutate(urban = "R") %>%
  left_join(r90p1, by="Address") %>%
  left_join(r_head, by = "Address") %>%
  left_join(r_job, by = "Address") %>%
  left_join(r90p3, by = "Address") %>%
  left_join(r_incomeSum, by = "Address") %>%
  left_join(r_NMincome, by = "Address") %>%
  left_join(R90P2) %>%
  mutate(across(income_w_y:income_nm_house, ~replace_na(.x, 0)))

UHH90 <- u90data %>% 
  mutate(urban = "U") %>%
  left_join(u90p1, by="Address") %>%
  left_join(u_head, by = "Address") %>%
  left_join(u_job, by = "Address") %>%
  left_join(u90p3, by = "Address") %>%
  left_join(u_incomeSum, by = "Address") %>%
  left_join(u_NMincome, by = "Address") %>%
  left_join(U90P2) %>%
  mutate(across(income_w_y:income_nm_house, ~replace_na(.x, 0)))

HH90 <- bind_rows(RHH90, UHH90) %>%
  mutate(urban = as.factor(urban)) %>%
  mutate(expenditure = cost_food + cost_tobacco + cost_clothing + cost_housing + cost_furniture + cost_health + cost_transport + cost_communication + cost_recreation +  cost_restaurant + cost_miscellaneous
         + cost_durables/12 + cost_investment/12,
         income = income_s_y + netincome_w_y + income_pension + income_rent + income_interest + income_aid + income_transfer + subsidy + income_nm_agriculture + income_nm_miscellaneous + income_nm_public + income_nm_private + income_nm_nonagriculture + income_nm_house,
         DYCOL00 = NA_integer_ 
  ) %>%
  left_join(month) %>%
  left_join(CPI) %>%
  select(Address:income, cpi_m = cpi, cpi_y) %>%
  as.data.frame()

attr(HH90$size, "label") <- "Household size"
attr(HH90$literates, "label") <- "Number of literate members"
attr(HH90$students, "label") <- "Number of student members"
attr(HH90$employeds, "label") <- "Number of employed members"
attr(HH90$gender, "label") <- "Head's gender"
attr(HH90$age, "label") <- "Head's age"
attr(HH90$literacy, "label") <- "Head's literacy"
attr(HH90$degree, "label") <- "Head's degree"
attr(HH90$occupationalst, "label") <- "Head's job status"
attr(HH90$maritalst, "label") <- "Head's marital status"
attr(HH90$ISIC_w, "label") <- "Head's Industry code of wage-earning job"
attr(HH90$ISCO_w, "label") <- "Head's Occupation code of wage-earning job"
attr(HH90$ISIC_s, "label") <- "Head's Industry code of non-wage job"
attr(HH90$ISCO_s, "label") <- "Head's Occupation code of non-wage job"
attr(HH90$income_nm_agriculture, "label") <- "Non-monetary income from agriculture"
attr(HH90$income_nm_nonagriculture, "label") <- "Non-monetary income from nonagriculture"
attr(HH90$income_nm_public, "label") <- "Non-monetary income from public sector job"
attr(HH90$income_nm_private, "label") <- "Non-monetary income from private sector job"
attr(HH90$income_nm_miscellaneous, "label") <- "Non-monetary income from other sources (home production, free)"
attr(HH90$netincome_w_m, "label") <- "wage-earning job: net income previous month"
attr(HH90$netincome_w_y, "label") <- "wage-earning job: net income previous year"
attr(HH90$income_w_y, "label") <- "wage-earning job: gross income previous year"
attr(HH90$income_s_y, "label") <- "non-wage job: net income previous year"
attr(HH90$income, "label") <- "total household income in previous year"
attr(HH90$expenditure, "label") <- "total monthly expendiuture of the household"

saveRDS(HH90, file = "./exported/HH90.Rds")


#######################################
# Exporting Rds files to CSV, STATA, etc.

#install.packages("haven") # uncomment if not already installed

df <- readRDS("./exported/HH90.Rds") # Specify the Rds file to convert here

#haven::write_dta(df, "./exported/HH90.dta") # to export in STATA
#write_csv2(df, "./exported/HH90.csv") # to export in CSV

