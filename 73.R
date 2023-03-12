library(tidyverse)
library(data.table)
library(readxl)

rm(list = ls())

load("./RAW/73.RData")

##############################
Province <- c(Markazi="00", Gilan="01", Mazandaran="02", AzarbaijanSharghi="03", AzarbaijanGharbi="04",
              Kermanshah="05", Kouzestan="06", Fars="07", Kerman="08", KhorasanRazavi="09",
              Esfahan="10", SistanBalouchestan="11", Kordestan="12", Hamedan="13", CharmahalBakhtiari="14",
              Lorestan="15", Ilam="16", KohkilouyeBoyerahamad="17", Boushehr="18", Zanjan="19", 
              Semnan="20", Yazd="21", Hormozgan="22", Tehran="23", Ardebil="24")

RW <- readRDS("pop63_75.rds") |>
  filter(R_U=="rural") |>
  select(province, pop=Pop1373)

UW <- readRDS("pop63_75.rds") |>
  filter(R_U=="urban") |>
  select(province, pop=Pop1373)  

R73Data <- R73P2 |> 
  mutate(Address = str_pad(ADDRESS,7,pad="0"),
         province = fct_recode(as.factor(substr(Address,2,3)), !!!Province)) |>
  left_join(RW) |>
  group_by(province) |>
  mutate(population=n(),
         weight=round(pop/population)) |>
  select(Address, weight, province) |>
  mutate_if(is.character, as.numeric)



U73Data <- U73P2 |> 
  mutate(Address = str_pad(ADDRESS,7,pad="0"),
         province = fct_recode(as.factor(substr(Address,2,3)), !!!Province)) |>
  left_join(UW) |>
  group_by(province) |>
  mutate(population=n(),
         weight=round(pop/population)) |>
  select(Address, weight, province) |>
  mutate_if(is.character, as.numeric)

##############################
# Part 1

relation <- c(head="1", spouse="2", child="3", childinlaw="4", grandchild="5", parent="6", sibling="7", relative="8", nonrelative="9")
gender <- c(Male="1", Female="2")
literacy <- c(literate="1", illiterate="2", NULL="", NULL="-", NULL=" ")
yesno <- c(Yes="1", No="2", NULL="", NULL="-", NULL=" ")
occupation <- c(employed="1", unemployed="2", IncomeWOJob="3", Student="4", Housewife="5", Other="6")
marital <- c(Married ="1", Widowed="2", Divorced="3", Single="4")

R73P1 <- R73P1 |> 
  rename(
    Address = ADDRESS,
    member = COL01,
    relation = COL03,
    gender = COL04,
    age = COL05,
    literacy = COL06,
    studying = COL07,
    degree = COL08,
    occupationalst = COL09,
    maritalst = COL10) |> 
  mutate(across(where(is.character), as.integer),
         across(c(relation,gender,literacy,studying,degree,occupationalst,maritalst), as.factor),
         relation = fct_recode(relation, !!!relation), 
         gender = fct_recode(gender, !!!gender),
         literacy = fct_recode(literacy, !!!literacy), 
         studying = fct_recode(studying, !!!yesno),
         occupationalst = fct_recode(occupationalst, !!!occupation),
         maritalst = fct_recode(maritalst, !!!marital),
         education = fct_relevel(as.factor(case_when(
           as.integer(as.character(degree))<20| ( studying == "No" & degree %in% c("21","22","23","24") ) ~ "Elemantry",
           ( degree %in% c("25","26","27","38") ) | (str_sub(degree,1,1)=="2" & studying == "Yes") ~ "Secondary", 
           ( studying == "Yes" & degree %in% c("30","31","32","34","38","40","41","42","46") ) ~ "HighSchool", 
           degree %in% c("33","35","39","43","45","47","48","49") ~ "Diploma", 
           degree %in% c("51","52") ~ "College",
           degree %in% c("61","62","73") ~ "Bachelor",
           degree %in% c("71","72") ~ "Master",
           degree %in% c("75","76","77","78") ~ "PhD",
           str_sub(degree,1,1)=="8"| str_sub(degree,1,1)=="9" ~ "Other",
           TRUE ~ NA_character_)), 
           "Elemantry","Secondary","HighSchool","Diploma","College","Bachelor","Master","PhD"))

U73P1 <- U73P1 |> 
  rename(
    Address = ADDRESS,
    member = COL01,
    relation = COL03,
    gender = COL04,
    age = COL05,
    literacy = COL06,
    studying = COL07,
    degree = COL08,
    occupationalst = COL09,
    maritalst = COL10) |> 
  mutate(across(where(is.character), as.integer),
         across(c(relation,gender,literacy,studying,degree,occupationalst,maritalst), as.factor),
         relation = fct_recode(relation, !!!relation), 
         gender = fct_recode(gender, !!!gender),
         literacy = fct_recode(literacy, !!!literacy), 
         studying = fct_recode(studying, !!!yesno),
         occupationalst = fct_recode(occupationalst, !!!occupation),
         maritalst = fct_recode(maritalst, !!!marital),
         education = fct_relevel(as.factor(case_when(
           as.integer(as.character(degree))<20| ( studying == "No" & degree %in% c("21","22","23","24") ) ~ "Elemantry",
           ( degree %in% c("25","26","27","38") ) | (str_sub(degree,1,1)=="2" & studying == "Yes") ~ "Secondary", 
           ( studying == "Yes" & degree %in% c("30","31","32","34","38","40","41","42","46") ) ~ "HighSchool", 
           degree %in% c("33","35","39","43","45","47","48","49") ~ "Diploma", 
           degree %in% c("51","52") ~ "College",
           degree %in% c("61","62","73") ~ "Bachelor",
           degree %in% c("71","72") ~ "Master",
           degree %in% c("75","76","77","78") ~ "PhD",
           str_sub(degree,1,1)=="8"| str_sub(degree,1,1)=="9" ~ "Other",
           TRUE ~ NA_character_)), 
           "Elemantry","Secondary","HighSchool","Diploma","College","Bachelor","Master","PhD"))


#######################################################################
# Part 2
tenure <- c(OwnedEstateLand="1", OwnedEstate="2", Rent="3", Service="4", Free="5", Other="6")
material <- c(MetalBlock="1", BrickWood="2", Cement="3", Brick="4", Wood="5", WoodKesht="6", KeshtGel="7", Other="8")
fuel <- c(Oil="1", Gasoline="2", NaturalGas="3", Electricity="4", Wood="5", AnimalOil="6", Other="7" )


R73P2 <- R73P2 |>
  rename(
    Address = ADDRESS,
    tenure = Q1,
    room = Q2,
    space = Q3,
    vehicle = Q5_01,
    motorcycle = Q5_02,
    bicycle = Q5_03,
    radio = Q5_05,
    radiotape = Q5_06,
    TVbw = Q5_07,
    TV = Q5_08,
    freezer = Q5_09,
    refridgerator = Q5_10,
    stove = Q5_11,
    vacuum = Q5_12,
    washingmachine = Q5_13,
    sewingmachine = Q5_04,
    pipewater = Q6_1,
    electricity = Q6_2,
    pipegas = Q6_3,
    telephone = Q6_6,
    bathroom = Q6_4,
    kitchen = Q6_8,
    evapcooling = Q6_5,
    centralheating = Q6_7,
    cookingfuel = Q7A,
    heatingfuel = Q7B) |>
  mutate(across(where(is.character), as.integer),
         across(c(tenure,cookingfuel,heatingfuel), as.factor),
         tenure = fct_recode(tenure, !!!tenure), 
         construction = case_when(
           (Q4B==5&Q4B==4)|(Q4A==5&Q4B==4) ~ 1L,
           (Q4A==4)|(Q4B==4) ~ 2L,
           TRUE ~ 3L),
         material = as.factor(case_when(
           (Q4B==5&Q4B==1)|(Q4A==1&Q4B==5)|(Q4B==5&Q4B==6)|(Q4A==6&Q4B==5) ~ 1L,
           (Q4B==7&Q4B==1)|(Q4A==1&Q4B==7)|(Q4B==7&Q4B==6)|(Q4A==6&Q4B==7) ~ 2L,
           (Q4A==2)|(Q4A==3) ~ 3L,
           (Q4B==1)|(Q4A==1) ~ 4L,
           (Q4A==7&is.na(Q4B)) ~ 5L,
           (Q4B==7&Q4B==8)|(Q4A==8&Q4B==7) ~ 6L,
           (Q4B==9&Q4B==8)|(Q4A==8&Q4B==9) ~ 7L,
           (Q4B==10)|(Q4A==10) ~ 8L,
           TRUE ~ 8L)),
         material = fct_recode(material, !!!material),
         cookingfuel = fct_recode(cookingfuel, !!!fuel), 
         heatingfuel = fct_recode(heatingfuel, !!!fuel),
         across(c(vehicle:centralheating), ~(.x == 1)) )

U73P2 <- U73P2 |>
  rename(
    Address = ADDRESS,
    tenure = Q1,
    room = Q2,
    space = Q3,
    vehicle = Q5_01,
    motorcycle = Q5_02,
    bicycle = Q5_03,
    radio = Q5_05,
    radiotape = Q5_06,
    TVbw = Q5_07,
    TV = Q5_08,
    freezer = Q5_09,
    refridgerator = Q5_10,
    stove = Q5_11,
    vacuum = Q5_12,
    washingmachine = Q5_13,
    sewingmachine = Q5_04,
    pipewater = Q6_1,
    electricity = Q6_2,
    pipegas = Q6_3,
    telephone = Q6_6,
    bathroom = Q6_4,
    kitchen = Q6_8,
    evapcooling = Q6_5,
    centralheating = Q6_7,
    cookingfuel = Q7A,
    heatingfuel = Q7B) |>
  mutate(across(where(is.character), as.integer),
         across(c(tenure,cookingfuel,heatingfuel), as.factor),
         tenure = fct_recode(tenure, !!!tenure), 
         construction = case_when(
           (Q4B==5&Q4B==4)|(Q4A==5&Q4B==4) ~ 1L,
           (Q4A==4)|(Q4B==4) ~ 2L,
           TRUE ~ 3L),
         material = as.factor(case_when(
           (Q4B==5&Q4B==1)|(Q4A==1&Q4B==5)|(Q4B==5&Q4B==6)|(Q4A==6&Q4B==5) ~ 1L,
           (Q4B==7&Q4B==1)|(Q4A==1&Q4B==7)|(Q4B==7&Q4B==6)|(Q4A==6&Q4B==7) ~ 2L,
           (Q4A==2)|(Q4A==3) ~ 3L,
           (Q4B==1)|(Q4A==1) ~ 4L,
           (Q4A==7&is.na(Q4B)) ~ 5L,
           (Q4B==7&Q4B==8)|(Q4A==8&Q4B==7) ~ 6L,
           (Q4B==9&Q4B==8)|(Q4A==8&Q4B==9) ~ 7L,
           (Q4B==10)|(Q4A==10) ~ 8L,
           TRUE ~ 8L)),
         material = fct_recode(material, !!!material),
         cookingfuel = fct_recode(cookingfuel, !!!fuel), 
         heatingfuel = fct_recode(heatingfuel, !!!fuel),
         across(c(vehicle:centralheating), ~(.x == 1)) )

###################################
# Part 3

CPI <- readRDS("CPI.rds") |>
  filter(year == 1373) |>
  group_by(Global) |>
  summarise(across(cpi:cpi_y,~mean(.x, na.rm=T))) 


Global <- readRDS("Global.rds") |>  # Global code for items
  select(Global, code = "C73") |>
  filter(!is.na(code))

GlobalM <- Global |> filter(Global %/% 1e6 == 1)
GlobalY <- Global |> filter(Global %/% 1e6 == 2)

# Part 3, Table 1

R73P3S01 <- R73P3S01 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    pricetype = COL4,
    kilogram = COL5_6,
    price = COL7,
    value = COL8) |>
  mutate(
    across(c(price,value,kilogram),  ~ as.numeric(as.character(.x)) ),
    table = 1L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)


U73P3S01 <- U73P3S01 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    pricetype = COL4,
    kilogram = COL5_6,
    price = COL7,
    value = COL8) |>
  mutate(
    across(c(price,value,kilogram),  ~ as.numeric(as.character(.x)) ),
    table = 1L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)



# Part 3, Table 2

R73P3S02 <- R73P3S02 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    pricetype = COL4,
    value = COL5) |>
  mutate(
    table = 2L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U73P3S02 <- U73P3S02 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    pricetype = COL4,
    value = COL5) |>
  mutate(
    table = 2L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 3

R73P3S03 <- R73P3S03 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    pricetype = COL4,
    value = COL5) |>
  mutate(
    table = 3L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U73P3S03 <- U73P3S03 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    pricetype = COL4,
    value = COL5) |>
  mutate(
    table = 3L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 4

R73P3S04 <- R73P3S04 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    pricetype = COL4,
    value = COL5) |>
  mutate(
    table = 4L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U73P3S04 <- U73P3S04 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    pricetype = COL4,
    value = COL5) |>
  mutate(
    table = 4L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 5

R73P3S05 <- R73P3S05 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    pricetype = COL4,
    value = COL5) |>
  mutate(
    table = 5L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U73P3S05 <- U73P3S05 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    pricetype = COL4,
    value = COL5) |>
  mutate(
    table = 5L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 6

R73P3S06 <- R73P3S06  |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    pricetype = COL4,
    value = COL5) |>
  mutate(
    table = 6L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U73P3S06 <- U73P3S06  |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    pricetype = COL4,
    value = COL5) |>
  mutate(
    table = 6L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 7

R73P3S07 <- R73P3S07 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    pricetype = COL4,
    value = COL5) |>
  mutate(
    table = 7L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U73P3S07 <- U73P3S07 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    pricetype = COL4,
    value = COL5) |>
  mutate(
    table = 7L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)


# Part 3, Table 8 

R73P3S08 <- R73P3S08 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    pricetype = COL4,
    value = COL5) |>
  mutate(
    table = 8L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U73P3S08 <- U73P3S08 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    pricetype = COL4,
    value = COL5) |>
  mutate(
    table = 8L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 9


R73P3S09 <- R73P3S09 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    pricetype = COL4,
    cost = COL5,
    sell = COL6) |>
  mutate(
    cost = replace_na(as.numeric(as.character(cost)),0),
    sell = replace_na(as.numeric(as.character(sell)),0),
    value = cost - sell,
    table = 9L) |> 
  left_join(GlobalY) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U73P3S09 <- U73P3S09 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    pricetype = COL4,
    cost = COL5,
    sell = COL6) |>
  mutate(
    cost = replace_na(as.numeric(as.character(cost)),0),
    sell = replace_na(as.numeric(as.character(sell)),0),
    value = cost - sell,
    table = 9L) |> 
  left_join(GlobalY) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 10

R73P3S10 <- R73P3S10 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    pricetype = COL4,
    cost = COL5,
    sell = COL6) |>
  mutate(
    cost = replace_na(as.numeric(as.character(cost)),0),
    sell = replace_na(as.numeric(as.character(sell)),0),
    value = cost - sell,
    table = 10L) |> 
  left_join(GlobalY) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi_y) |>
  select(Address:Global, value_r)

U73P3S10 <- U73P3S10 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    pricetype = COL4,
    cost = COL5,
    sell = COL6) |>
  mutate(
    cost = replace_na(as.numeric(as.character(cost)),0),
    sell = replace_na(as.numeric(as.character(sell)),0),
    value = cost - sell,
    table = 10L) |> 
  left_join(GlobalY) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi_y) |>
  select(Address:Global, value_r)


list2env(lapply(mget(ls(pattern = "P3S.*")),
                function(x) {x |> mutate(purchased=factor(purchased,
                                                           levels = c(2,3,4,5,6,7,8),
                                                           labels = c("purchased",
                                                                      "homemade",
                                                                      "publicservice",
                                                                      "privateservice",
                                                                      "agriculture",
                                                                      "nonagriculture",
                                                                      "free")))} )
         , .GlobalEnv)



#############################
# Part 4, Table 1

R73P4S1$COL02 <- as.numeric(as.character(R73P4S1$COL02))
R73P4S1$COL03 <- as.numeric(as.character(R73P4S1$COL03))
R73P4S01 <- R73P4S1 |>
  rename(
    Address = ADDRESS,
    member = COL01,
    ISCO_w = COL02,
    ISIC_w = COL03,
    status_w = COL04,
    income_w_m = COL05,
    income_w_y = COL06,
    wage_w_m = COL07,
    wage_w_y = COL08,
    perk_w_m = COL09,
    perk_w_y = COL10,
    netincome_w_m = COL11,
    netincome_w_y = COL12) |>
  mutate(Global = 1000000) |>
  left_join(CPI) |>
  select(Address:netincome_w_y, cpi_m = cpi, cpi_y)

U73P4S1$COL02 <- as.numeric(as.character(U73P4S1$COL02))
U73P4S1$COL03 <- as.numeric(as.character(U73P4S1$COL03))
U73P4S01 <- U73P4S1 |>
  rename(
    Address = ADDRESS,
    member = COL01,
    ISCO_w = COL02,
    ISIC_w = COL03,
    status_w = COL04,
    income_w_m = COL05,
    income_w_y = COL06,
    wage_w_m = COL07,
    wage_w_y = COL08,
    perk_w_m = COL09,
    perk_w_y = COL10,
    netincome_w_m = COL11,
    netincome_w_y = COL12) |>
  mutate(Global = 1000000) |>
  left_join(CPI) |>
  select(Address:netincome_w_y, cpi_m = cpi, cpi_y)


# Part 4, Table 2
R73P4S2$COL02 <- as.numeric(as.character(R73P4S2$COL02))
R73P4S2$COL03 <- as.numeric(as.character(R73P4S2$COL03))
R73P4S02 <- R73P4S2 |>
  rename(
    Address = ADDRESS,
    member = COL01,
    ISCO_s = COL02,
    ISIC_s = COL03,
    status_s = COL04,
    agriculture = COL05,
    cost_employment = COL06,
    cost_raw = COL07,
    cost_machinery = COL08,
    cost_others = COL09,
    cost_tax = COL10,
    sale = COL11,
    income_s_y = COL12) |>
  mutate(Global = 1000000) |>
  left_join(CPI) |>
  select(Address:income_s_y, cpi_m = cpi, cpi_y)

U73P4S2$COL02 <- as.numeric(as.character(U73P4S2$COL02))
U73P4S2$COL03 <- as.numeric(as.character(U73P4S2$COL03))
U73P4S02 <- U73P4S2 |>
  rename(
    Address = ADDRESS,
    member = COL01,
    ISCO_s = COL02,
    ISIC_s = COL03,
    status_s = COL04,
    agriculture = COL05,
    cost_employment = COL06,
    cost_raw = COL07,
    cost_machinery = COL08,
    cost_others = COL09,
    cost_tax = COL10,
    sale = COL11,
    income_s_y = COL12) |>
  mutate(Global = 1000000) |>
  left_join(CPI) |>
  select(Address:income_s_y, cpi_m = cpi, cpi_y)


# Part 4, Table 3
R73P4S03 <- R73P4S3 |>
  rename(
    Address = ADDRESS,
    member = COL01,
    income_pension = COL03,
    income_rent = COL04,
    income_interest = COL05,
    income_aid = COL06,
    income_resale = COL07) |>
  mutate(Global = 1000000) |>
  left_join(CPI) |>
  select(Address:income_resale, cpi_y)


U73P4S03 <- U73P4S3 |>
  rename(
    Address = ADDRESS,
    member = COL01,
    income_pension = COL03,
    income_rent = COL04,
    income_interest = COL05,
    income_aid = COL06,
    income_resale = COL07) |>
  mutate(Global = 1000000) |>
  left_join(CPI) |>
  select(Address:income_resale, cpi_y)

rm(R73P4S1,U73P4S1,R73P4S2,U73P4S2,R73P4S3,U73P4S3)

rm(list = setdiff(ls(), ls(pattern = "73"))) # removing unnecessary objects

save.image(file="./exported/HEIS73.Rdata")


CPI <- readRDS("CPI.rds") |>
  filter(year == 1373) |>
  group_by(Global) |>
  summarise(across(cpi:cpi_y,~mean(.x, na.rm=T))) 


###############################################
# Item-level expenditure table
# Merging tables in a unique expenditure table

R73P3 <- bind_rows(mget(ls(pattern = "R73P3S.*")))
R73P3 <- R73P3 |>
  left_join(R73Data) |>
  mutate(urban = "R")

U73P3 <- bind_rows(mget(ls(pattern = "U73P3S.*")))
U73P3 <- U73P3 |>
  left_join(U73Data) |>
  mutate(urban = "U")

itemcode <- read_excel("Codes.xlsx", sheet = "itemLabel") |> select(-labelFA)
itemcode$item <- factor(itemcode$Global, levels = itemcode$Global, labels = itemcode$labelEN)

EXP73 <- bind_rows(R73P3,U73P3) |>
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
            Price = median(price, na.rm = T)) |>
  filter(Value!=0) |>
  left_join(itemcode) |>
  select(-labelEN) |>
  as.data.frame()

attr(EXP73$Table, "label") <- "Table in Part 3"
attr(EXP73$Global, "label") <- "global item code"
attr(EXP73$code, "label") <- "item code in this year"
attr(EXP73$urban, "label") <- "rural or urban"
attr(EXP73$Price, "label") <- "median price"
attr(EXP73$Value, "label") <- "monthly expenditure"
attr(EXP73$Value_r, "label") <- "monthly expenditure in 1390 price"

saveRDS(EXP73, "./exported/EXP73.Rds")

###############################################
# Building individual level data

r73data <- R73Data |> select(Address, weight, province)
u73data <- U73Data |> select(Address, weight, province)

# faster code with data.table
DT <- data.table(R73P4S01)
DT <- DT[,.(
            'ISCO_w'= first(ISCO_w),
            'ISIC_w'= first(ISIC_w),
            'status_w'= first(status_w),
            'income_w_m'= sum(income_w_m, na.rm = T),
            'income_w_y'= sum(income_w_y, na.rm = T),
            'wage_w_m'= sum(wage_w_m, na.rm = T),
            'wage_w_y'= sum(wage_w_y, na.rm = T),
            'perk_w_m'= sum(perk_w_m, na.rm = T),
            'perk_w_y'= sum(perk_w_y, na.rm = T),
            'netincome_w_m'= sum(netincome_w_m, na.rm = T),
            'netincome_w_y'= sum(netincome_w_y, na.rm = T)),
         by=.(Address, member)]
R73P4S01_unique <- as.data.frame(DT) 

DT <- data.table(R73P4S02)
DT <- DT[,.(
            'ISCO_s'= first(ISCO_s),
            'ISIC_s'= first(ISIC_s),
            'status_s'= first(status_s),
            'agriculture'= first(agriculture),
            'income_s_y'= sum(income_s_y, na.rm = T),
            'cost_employment'= sum(cost_employment, na.rm = T),
            'cost_raw'= sum(cost_raw, na.rm = T),
            'cost_machinery'= sum(cost_machinery, na.rm = T),
            'cost_others'= sum(cost_others, na.rm = T),
            'cost_tax'= sum(cost_tax, na.rm = T),
            'sale'= sum(sale, na.rm = T)),
         by=.(Address, member)]
R73P4S02_unique <- as.data.frame(DT)

DT <- data.table(R73P4S03)
DT <- DT[,.('income_pension'= sum(income_pension, na.rm = T),
            'income_rent'= sum(income_rent, na.rm = T),
            'income_interest'= sum(income_interest, na.rm = T),
            'income_aid'= sum(income_aid, na.rm = T),
            'income_resale'= sum(income_resale, na.rm = T)),
         by=.(Address, member)]
R73P4S03_unique <- as.data.frame(DT) 

DT <- data.table(U73P4S01)
DT <- DT[,.(
            'ISCO_w'= first(ISCO_w),
            'ISIC_w'= first(ISIC_w),
            'status_w'= first(status_w),
            'income_w_m'= sum(income_w_m, na.rm = T),
            'income_w_y'= sum(income_w_y, na.rm = T),
            'wage_w_m'= sum(wage_w_m, na.rm = T),
            'wage_w_y'= sum(wage_w_y, na.rm = T),
            'perk_w_m'= sum(perk_w_m, na.rm = T),
            'perk_w_y'= sum(perk_w_y, na.rm = T),
            'netincome_w_m'= sum(netincome_w_m, na.rm = T),
            'netincome_w_y'= sum(netincome_w_y, na.rm = T)),
         by=.(Address, member)]
U73P4S01_unique <- as.data.frame(DT)

DT <- data.table(U73P4S02)
DT <- DT[,.(
            'ISCO_s'= first(ISCO_s),
            'ISIC_s'= first(ISIC_s),
            'status_s'= first(status_s),
            'agriculture'= first(agriculture),
            'income_s_y'= sum(income_s_y, na.rm = T),
            'cost_employment'= sum(cost_employment, na.rm = T),
            'cost_raw'= sum(cost_raw, na.rm = T),
            'cost_machinery'= sum(cost_machinery, na.rm = T),
            'cost_others'= sum(cost_others, na.rm = T),
            'cost_tax'= sum(cost_tax, na.rm = T),
            'sale'= sum(sale, na.rm = T)),
         by=.(Address, member)]
U73P4S02_unique <- as.data.frame(DT) 

DT <- data.table(U73P4S03)
DT <- DT[,.('income_pension'= sum(income_pension, na.rm = T),
            'income_rent'= sum(income_rent, na.rm = T),
            'income_interest'= sum(income_interest, na.rm = T),
            'income_aid'= sum(income_aid, na.rm = T),
            'income_resale'= sum(income_resale, na.rm = T)),
         by=.(Address, member)]
U73P4S03_unique <- as.data.frame(DT) 

Rind73 <- r73data  |>
  left_join(select(R73P1,-degree)) |>
  left_join(R73P4S01_unique) |>
  left_join(R73P4S02_unique) |>
  left_join(R73P4S03_unique) |> 
  mutate(across(where(is.character),as.integer)) |>
  mutate(urban = "R")

Uind73 <- u73data |>
  left_join(select(U73P1,-degree)) |>
  left_join(U73P4S01_unique) |>
  left_join(U73P4S02_unique) |>
  left_join(U73P4S03_unique) |> 
  mutate(across(where(is.character),as.integer)) |>
  mutate(urban = "U")

IND73 <- bind_rows(Rind73,Uind73) |>
  mutate(urban = as.factor(urban),
         Global = 1000000,
         status_w = factor(status_w, levels = c(1,2), labels = c("public","private")),
         status_s = factor(status_s, levels = c(1,2,3), labels = c("employer","selfemployed","familyworker")),
         agriculture = factor(agriculture, levels = c(1,2), labels = c("agriculture","nonagriculture"))
  ) |>
  left_join(CPI) |>
  select(Address:urban, cpi_m = cpi, cpi_y) |>
  as.data.frame()

attr(IND73$occupationalst, "label") <- "employment status"
attr(IND73$ISIC_w, "label") <- "Industry code of wage-earning job"
attr(IND73$ISCO_w, "label") <- "Occupation code of wage-earning job"
attr(IND73$status_w, "label") <- "Wage-earning job status: 1-public 2-cooperative 3-private"
attr(IND73$netincome_w_m, "label") <- "wage-earning job: net income previous month"
attr(IND73$netincome_w_y, "label") <- "wage earning job: net income previous year"
attr(IND73$ISIC_s, "label") <- "Industry code of non-wage job"
attr(IND73$ISCO_s, "label") <- "Occupation code of non-wage job"
attr(IND73$status_s, "label") <- "job status: 4-employer 2-selfemployed 3-familyworker"
attr(IND73$income_s_y, "label") <- "non-wage job: net income previous year"

saveRDS(IND73,"./exported/IND73.Rds")

###############################################
# Building Household Data

r73p1 <- R73P1 |>
  group_by(Address) |>
  summarize(size = sum(!is.na(member)),
            literates = sum(literacy == "literate", na.rm = TRUE),
            students = sum(studying == "Yes", na.rm = TRUE),
            employeds = sum(occupationalst == "employed", na.rm = TRUE)) 

u73p1 <- U73P1 |>
  group_by(Address) |>
  summarize(size = sum(!is.na(member)),
            literates = sum(literacy == "literate", na.rm = TRUE),
            students = sum(studying == "Yes", na.rm = TRUE),
            employeds = sum(occupationalst == "employed", na.rm = TRUE)) 

r_head <- R73P1 |>
  filter(relation == "head") |>
  select(-member,-relation,-studying)

u_head <- U73P1 |>
  filter(relation == "head") |>
  select(-member,-relation,-studying)

# summary of heads job codes
r_job <- Rind73 |>
  filter(relation == "head") |>
  select(Address,starts_with("IS"))

u_job <- Uind73 |>
  filter(relation == "head") |>
  select(Address,starts_with("IS"))

# Sum of household expenditure items
r73p3 <- R73P3 |>
  mutate(nominal_gross = value + sell,
         real_gross = nominal_gross * value_r/value) |>
  select(Address, table, nominal_net="value", real_net="value_r", ends_with("gross")) |>
  pivot_longer(cols = nominal_net:real_gross,
               names_to = c(".value","Net"),
               names_pattern = "(.*)_(.*)",
               values_drop_na = TRUE) |>
  mutate(Table = case_when(
    table == 1 ~ "food_tobacco",
    table == 2 ~ "clothing",
    table == 3 ~ "housing",
    table == 4 ~ "appliances",
    table == 5 ~ "health",
    table == 6 ~ "transport_communication",
    table == 7 ~ "recreation",
    table == 8 ~ "miscellaneous",
    table == 9 & Net =="net" ~ "durables",
    table == 10 & Net =="net" ~ "investment",
    table == 9 & Net =="gross" ~ "durables_gross",
    table == 10 & Net =="gross" ~ "investment_gross",
    TRUE ~ NA_character_)) |>
  group_by(Address,Table) |>
  summarize(cost = sum(nominal, na.rm = TRUE),
            cost_r = sum(real, na.rm = TRUE)) |>
  pivot_wider(Address, 
              names_from = "Table", 
              values_from = c("cost","cost_r"), 
              values_fill = list(cost = 0, cost_r = 0))

u73p3 <- U73P3 |>
  mutate(nominal_gross = value + sell,
         real_gross = nominal_gross * value_r/value) |>
  select(Address, table, nominal_net="value", real_net="value_r", ends_with("gross")) |>
  pivot_longer(cols = nominal_net:real_gross,
               names_to = c(".value","Net"),
               names_pattern = "(.*)_(.*)",
               values_drop_na = TRUE) |>
  mutate(Table = case_when(
    table == 1 ~ "food_tobacco",
    table == 2 ~ "clothing",
    table == 3 ~ "housing",
    table == 4 ~ "appliances",
    table == 5 ~ "health",
    table == 6 ~ "transport_communication",
    table == 7 ~ "recreation",
    table == 8 ~ "miscellaneous",
    table == 9 & Net =="net" ~ "durables",
    table == 10 & Net =="net" ~ "investment",
    table == 9 & Net =="gross" ~ "durables_gross",
    table == 10 & Net =="gross" ~ "investment_gross",
    TRUE ~ NA_character_)) |>
  group_by(Address,Table) |>
  summarize(cost = sum(nominal, na.rm = TRUE),
            cost_r = sum(real, na.rm = TRUE)) |>
  pivot_wider(Address, 
              names_from = "Table", 
              values_from = c("cost","cost_r"), 
              values_fill = list(cost = 0, cost_r = 0))

# Non-monetary household income
r_NM_housing <- R73P3S03 |>
  filter(Global %in% c(1042111,1042211,1042212) ) |>
  group_by(Address) |>
  summarize(income_nm_house = sum(value*12, na.rm = T))

u_NM_housing <- U73P3S03 |>
  filter(Global %in% c(1042111,1042211,1042212) ) |>
  group_by(Address) |>
  summarize(income_nm_house = sum(value*12, na.rm = T))

r_NMincome <- R73P3 |>
  mutate(type = case_when(
    purchased %in% c("publicservice","cooperativeservice") ~ "public",
    purchased == "privateservice" ~ "private",
    purchased == "agriculture" ~ "agriculture",
    purchased == "nonagriculture" ~ "nonagriculture", 
    purchased %in% c("free","homemade") ~ as.character(purchased),
    TRUE ~ NA_character_),
    recallperiod=ifelse(table>8,1,12)) |>
  group_by(Address, type) |>
  summarize(value = sum(value*recallperiod, na.rm = T)) |>
  filter(!is.na(type)&value!=0) |> 
  pivot_wider(Address, 
              names_from="type", names_prefix = "income_nm_", 
              values_from = "value",  values_fill = list(value = 0)) |>
  full_join(r_NM_housing) 
r_NMincome[is.na(r_NMincome)] <- 0

u_NMincome <- U73P3 |>
  mutate(type = case_when(
    purchased %in% c("publicservice","cooperativeservice") ~ "public",
    purchased == "privateservice" ~ "private",
    purchased == "agriculture" ~ "agriculture",
    purchased == "nonagriculture" ~ "nonagriculture", 
    purchased %in% c("free","homemade") ~ as.character(purchased),
    TRUE ~ NA_character_),
    recallperiod=ifelse(table>8,1,12)) |>
  group_by(Address, type) |>
  summarize(value = sum(value*recallperiod, na.rm = T)) |>
  filter(!is.na(type)&value!=0) |> 
  pivot_wider(Address, 
              names_from="type", names_prefix = "income_nm_", 
              values_from = "value", values_fill = list(value = 0)) |>
  full_join(u_NM_housing) 
u_NMincome[is.na(u_NMincome)] <- 0


# sum of household income with data.table

DT <- data.table(Rind73)
DT <- DT[,.('income_w_y'= sum(income_w_y, na.rm = T),
            'income_s_y'= sum(income_s_y, na.rm = T),
            'netincome_w_m'= sum(netincome_w_m, na.rm = T),
            'netincome_w_y'= sum(netincome_w_y, na.rm = T),
            'income_pension'= sum(income_pension, na.rm = T),
            'income_rent'= sum(income_rent, na.rm = T),
            'income_interest'= sum(income_interest, na.rm = T),
            'income_aid'= sum(income_aid, na.rm = T),
            'income_resale'= sum(income_resale, na.rm = T)),
         by=.(Address)]
r_incomeSum <- as.data.frame(DT) 

DT <- data.table(Uind73)
DT <- DT[,.('income_w_y'= sum(income_w_y, na.rm = T),
            'income_s_y'= sum(income_s_y, na.rm = T),
            'netincome_w_m'= sum(netincome_w_m, na.rm = T),
            'netincome_w_y'= sum(netincome_w_y, na.rm = T),
            'income_pension'= sum(income_pension, na.rm = T),
            'income_rent'= sum(income_rent, na.rm = T),
            'income_interest'= sum(income_interest, na.rm = T),
            'income_aid'= sum(income_aid, na.rm = T),
            'income_resale'= sum(income_resale, na.rm = T)),
         by=.(Address)]
u_incomeSum <- as.data.frame(DT)

# merging household-level data
RHH73 <- r73data |> 
  mutate(urban = "R") |>
  left_join(r73p1, by="Address") |>
  left_join(r_head, by = "Address") |>
  left_join(r_job, by = "Address") |>
  left_join(r73p3, by = "Address") |>
  left_join(r_incomeSum, by = "Address") |>
  left_join(r_NMincome, by = "Address") |>
  left_join(R73P2) |>
  mutate(across(income_w_y:income_nm_house, ~replace_na(.x, 0)))

UHH73 <- u73data |> 
  mutate(urban = "U") |>
  left_join(u73p1, by="Address") |>
  left_join(u_head, by = "Address") |>
  left_join(u_job, by = "Address") |>
  left_join(u73p3, by = "Address") |>
  left_join(u_incomeSum, by = "Address") |>
  left_join(u_NMincome, by = "Address") |>
  left_join(U73P2) |>
  mutate(across(income_w_y:income_nm_house, ~replace_na(.x, 0)))

HH73 <- bind_rows(RHH73, UHH73) |>
  mutate(urban = as.factor(urban)) |>
  mutate(expenditure = cost_food_tobacco + cost_clothing + cost_housing + cost_appliances + cost_health + cost_transport_communication + cost_recreation +  cost_miscellaneous
         + cost_durables/12 ,
         income = income_s_y + netincome_w_y + income_pension + income_rent + income_interest + income_aid  + income_nm_agriculture + income_nm_free + income_nm_homemade + income_nm_public + income_nm_private + income_nm_nonagriculture + income_nm_house,
         Global = 1000000 ) |>
  left_join(CPI) |>
  select(Address:income, cpi_m = cpi, cpi_y) |>
  as.data.frame()

attr(HH73$size, "label") <- "Household size"
attr(HH73$literates, "label") <- "Number of literate members"
attr(HH73$students, "label") <- "Number of student members"
attr(HH73$employeds, "label") <- "Number of employed members"
attr(HH73$gender, "label") <- "Head's gender"
attr(HH73$age, "label") <- "Head's age"
attr(HH73$literacy, "label") <- "Head's literacy"
attr(HH73$education, "label") <- "Head's education"
attr(HH73$occupationalst, "label") <- "Head's job status"
attr(HH73$maritalst, "label") <- "Head's marital status"
attr(HH73$ISIC_w, "label") <- "Head's Industry code of wage-earning job"
attr(HH73$ISCO_w, "label") <- "Head's Occupation code of wage-earning job"
attr(HH73$ISIC_s, "label") <- "Head's Industry code of non-wage job"
attr(HH73$ISCO_s, "label") <- "Head's Occupation code of non-wage job"
attr(HH73$income_nm_agriculture, "label") <- "Non-monetary income from agriculture"
attr(HH73$income_nm_nonagriculture, "label") <- "Non-monetary income from nonagriculture"
attr(HH73$income_nm_public, "label") <- "Non-monetary income from public sector job"
attr(HH73$income_nm_private, "label") <- "Non-monetary income from private sector job"
attr(HH73$income_nm_homemade, "label") <- "Non-monetary income from home production"
attr(HH73$netincome_w_m, "label") <- "wage-earning job: net income previous month"
attr(HH73$netincome_w_y, "label") <- "wage-earning job: net income previous year"
attr(HH73$income_w_y, "label") <- "wage-earning job: gross income previous year"
attr(HH73$income_s_y, "label") <- "non-wage job: net income previous year"
attr(HH73$income, "label") <- "total household income in previous year"
attr(HH73$expenditure, "label") <- "total monthly expendiuture of the household"

saveRDS(HH73, file = "./exported/HH73.Rds")


#######################################
# Exporting Rds files to CSV, STATA, etc.

#install.packages("haven") # uncomment if not already installed

df <- readRDS("./exported/HH73.Rds") # Specify the Rds file to convert here

#haven::write_dta(df, "./exported/HH73.dta") # to export in STATA
#write_csv2(df, "./exported/HH73.csv") # to export in CSV
