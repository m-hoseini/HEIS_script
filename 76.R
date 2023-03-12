library(tidyverse)
library(data.table)
library(readxl)

rm(list = ls())

load("./RAW/76.RData")

##############################
Province <- c(Markazi="00", Gilan="01", Mazandaran="02", AzarbaijanSharghi="03", AzarbaijanGharbi="04",
              Kermanshah="05", Kouzestan="06", Fars="07", Kerman="08", KhorasanRazavi="09",
              Esfahan="10", SistanBalouchestan="11", Kordestan="12", Hamedan="13", CharmahalBakhtiari="14",
              Lorestan="15", Ilam="16", KohkilouyeBoyerahamad="17", Boushehr="18", Zanjan="19", 
              Semnan="20", Yazd="21", Hormozgan="22", Tehran="23", Ardebil="24", Qom="25", Qazvin="26",
              Golestan="27")


R76Data <- Sum_R76 |> 
  mutate(province = fct_recode(as.factor(OSTAN), !!!Province),
         khanevar = paste0( substr(KHANEVAR,1,1), substr(KHANEVAR,3,5) ),
         Address = paste0(R_U,OSTAN,khanevar)) |>
  select(Address, weight = weight_R, province) |>
  mutate_if(is.character, as.numeric)


U76Data <- Sum_U76 |> 
  mutate(province = fct_recode(as.factor(OSTAN), !!!Province),
         khanevar = paste0( substr(KHANEVAR,1,1), substr(KHANEVAR,3,5) ),
         Address = paste0(R_U,OSTAN,khanevar)) |>
  select(Address, weight = weight_U, province) |>
  mutate_if(is.character, as.numeric)

##############################
# Part 1


relation <- c(head="1", spouse="2", child="3", childinlaw="4", grandchild="5", parent="6", sibling="7", relative="8", nonrelative="9")
gender <- c(Male="1", Female="2")
literacy <- c(literate="1", illiterate="2", NULL="", NULL="-", NULL=" ")
yesno <- c(Yes="1", No="2", NULL="", NULL="-", NULL=" ")
occupation <- c(employed="1", unemployed="2", IncomeWOJob="3", Student="4", Housewife="5", Other="6")
marital <- c(Married ="1", Widowed="2", Divorced="3", Single="4")

R76P1 <- R76P1 |> 
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

U76P1 <- U76P1 |> 
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
fuel1 <- c(Oil="8", Gasoline="9", NaturalGas="10", Electricity="11", Wood="12", AnimalOil="13", Other="14" )


# Part 2

R76P2 <- R76P2 |>
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
    VHS_VCD_DVD = Q5_14,
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
         heatingfuel = fct_recode(heatingfuel, !!!fuel1),
         across(c(vehicle:centralheating), ~(.x == 1)) )

U76P2 <- U76P2 |>
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
    VHS_VCD_DVD = Q5_14,
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
         heatingfuel = fct_recode(heatingfuel, !!!fuel1),
         across(c(vehicle:centralheating), ~(.x == 1)) )

###################################
# Part 3, Table 1

CPI <- readRDS("CPI.rds") |>
  filter(year == 1376) |>
  group_by(Global) |>
  summarise(across(cpi:cpi_y,~mean(.x, na.rm=T))) 


Global <- readRDS("Global.rds") |>  # Global code for items
  select(Global, code = "C76") |>
  filter(!is.na(code))

GlobalM <- Global |> filter(Global %/% 1e6 == 1)
GlobalY <- Global |> filter(Global %/% 1e6 == 2)

# Part 3, Table 1

R76P3S01 <- R76P3S01 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    kilogram = COL4_5,
    price = COL6,
    value = COL7) |>
  mutate(
    across(c(price,value,kilogram),  ~ as.numeric(as.character(.x)) ),
    table = 1L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)


U76P3S01 <- U76P3S01 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    kilogram = COL4_5,
    price = COL6,
    value = COL7) |>
  mutate(
    across(c(price,value,kilogram),  ~ as.numeric(as.character(.x)) ),
    table = 1L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)



# Part 3, Table 2

R76P3S02 <- R76P3S02 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    value = COL4) |>
  mutate(
    table = 2L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U76P3S02 <- U76P3S02 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    value = COL4) |>
  mutate(
    table = 2L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 3

R76P3S03 <- R76P3S03 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    value = COL4) |>
  mutate(
    table = 3L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U76P3S03 <- U76P3S03 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    value = COL4) |>
  mutate(
    table = 3L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 4

R76P3S04 <- R76P3S04 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    value = COL4) |>
  mutate(
    table = 4L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U76P3S04 <- U76P3S04 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    value = COL4) |>
  mutate(
    table = 4L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 5

R76P3S05 <- R76P3S05 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    value = COL4) |>
  mutate(
    table = 5L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U76P3S05 <- U76P3S05 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    value = COL4) |>
  mutate(
    table = 5L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 6

R76P3S06 <- R76P3S06  |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    value = COL4) |>
  mutate(
    table = 6L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U76P3S06 <- U76P3S06  |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    value = COL4) |>
  mutate(
    table = 6L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 7

R76P3S07 <- R76P3S07 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    value = COL4) |>
  mutate(
    table = 7L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U76P3S07 <- U76P3S07 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    value = COL4) |>
  mutate(
    table = 7L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)


# Part 3, Table 8 

R76P3S08 <- R76P3S08 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    value = COL4) |>
  mutate(
    table = 8L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U76P3S08 <- U76P3S08 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    value = COL4) |>
  mutate(
    table = 8L) |> 
  left_join(GlobalM) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 9


R76P3S09 <- R76P3S09 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    cost = COL4,
    sell = COL5) |>
  mutate(
    cost = replace_na(as.numeric(as.character(cost)),0),
    sell = replace_na(as.numeric(as.character(sell)),0),
    value = cost - sell,
    table = 9L) |> 
  left_join(GlobalY) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U76P3S09 <- U76P3S09 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    cost = COL4,
    sell = COL5) |>
  mutate(
    cost = replace_na(as.numeric(as.character(cost)),0),
    sell = replace_na(as.numeric(as.character(sell)),0),
    value = cost - sell,
    table = 9L) |> 
  left_join(GlobalY) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)



list2env(lapply(mget(ls(pattern = "P3S.*")),
                function(x) {x |> mutate(purchased=factor(purchased,
                                                           levels = c(1,2,3,4,5,6,7),
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

R76P4S01 <- R76P4S1 |>
  rename(
    Address = ADDRESS,
    member = COL01,
    employed_w = COL02,
    ISCO_w = COL03,
    ISIC_w = COL04,
    status_w = COL05,
    income_w_m = COL06,
    income_w_y = COL07,
    wage_w_m = COL08,
    wage_w_y = COL09,
    perk_w_m = COL10,
    perk_w_y = COL11,
    netincome_w_m = COL12,
    netincome_w_y = COL13) |>
  mutate(Global = 1000000) |>
  left_join(CPI) |>
  select(Address:netincome_w_y, cpi_m = cpi, cpi_y)

U76P4S01 <- U76P4S1 |>
  rename(
    Address = ADDRESS,
    member = COL01,
    employed_w = COL02,
    ISCO_w = COL03,
    ISIC_w = COL04,
    status_w = COL05,
    income_w_m = COL06,
    income_w_y = COL07,
    wage_w_m = COL08,
    wage_w_y = COL09,
    perk_w_m = COL10,
    perk_w_y = COL11,
    netincome_w_m = COL12,
    netincome_w_y = COL13) |>
  mutate(Global = 1000000) |>
  left_join(CPI) |>
  select(Address:netincome_w_y, cpi_m = cpi, cpi_y)


# Part 4, Table 2

R76P4S02 <- R76P4S2 |>
  rename(
    Address = ADDRESS,
    member = COL01,
    employed_s = COL02,
    ISCO_s = COL03,
    ISIC_s = COL04,
    status_s = COL05,
    agriculture = COL06,
    cost_employment = COL07,
    cost_raw = COL08,
    cost_machinery = COL09,
    cost_others = COL10,
    cost_tax = COL11,
    sale = COL12,
    income_s_y = COL13) |>
  mutate(Global = 1000000) |>
  left_join(CPI) |>
  select(Address:income_s_y, cpi_m = cpi, cpi_y)

U76P4S02 <- U76P4S2 |>
  rename(
    Address = ADDRESS,
    member = COL01,
    employed_s = COL02,
    ISCO_s = COL03,
    ISIC_s = COL04,
    status_s = COL05,
    agriculture = COL06,
    cost_employment = COL07,
    cost_raw = COL08,
    cost_machinery = COL09,
    cost_others = COL10,
    cost_tax = COL11,
    sale = COL12,
    income_s_y = COL13) |>
  mutate(Global = 1000000) |>
  left_join(CPI) |>
  select(Address:income_s_y, cpi_m = cpi, cpi_y)


# Part 4, Table 3
R76P4S03 <- R76P4S3 |>
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


U76P4S03 <- U76P4S3 |>
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

rm(R76P4S1,U76P4S1,R76P4S2,U76P4S2,R76P4S3,U76P4S3)

rm(list = setdiff(ls(), ls(pattern = "76"))) # removing unnecessary objects

save.image(file="./exported/HEIS76.Rdata")


CPI <- readRDS("CPI.rds") |>
  filter(year == 1376) |>
  group_by(Global) |>
  summarise(across(cpi:cpi_y,~mean(.x, na.rm=T))) 


###############################################
# Item-level expenditure table
# Merging tables in a unique expenditure table

R76P3 <- bind_rows(mget(ls(pattern = "R76P3S.*")))
R76P3 <- R76P3 |>
  left_join(R76Data) |>
  mutate(urban = "R")

U76P3 <- bind_rows(mget(ls(pattern = "U76P3S.*")))
U76P3 <- U76P3 |>
  left_join(U76Data) |>
  mutate(urban = "U")

itemcode <- read_excel("Codes.xlsx", sheet = "itemLabel") |> select(-labelFA)
itemcode$item <- factor(itemcode$Global, levels = itemcode$Global, labels = itemcode$labelEN)

EXP76 <- bind_rows(R76P3,U76P3) |>
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

attr(EXP76$Table, "label") <- "Table in Part 3"
attr(EXP76$Global, "label") <- "global item code"
attr(EXP76$code, "label") <- "item code in this year"
attr(EXP76$urban, "label") <- "rural or urban"
attr(EXP76$Price, "label") <- "median price"
attr(EXP76$Value, "label") <- "monthly expenditure"
attr(EXP76$Value_r, "label") <- "monthly expenditure in 1390 price"

saveRDS(EXP76, "./exported/EXP76.Rds")

###############################################
# Building individual level data

r76data <- R76Data |> select(Address, weight, province)
u76data <- U76Data |> select(Address, weight, province)

# faster code with data.table
DT <- data.table(R76P4S01)
DT <- DT[,.('employed_w'= first(employed_w),
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
R76P4S01_unique <- as.data.frame(DT) 

DT <- data.table(R76P4S02)
DT <- DT[,.('employed_s'= first(employed_s),
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
R76P4S02_unique <- as.data.frame(DT)

DT <- data.table(R76P4S03)
DT <- DT[,.('income_pension'= sum(income_pension, na.rm = T),
            'income_rent'= sum(income_rent, na.rm = T),
            'income_interest'= sum(income_interest, na.rm = T),
            'income_aid'= sum(income_aid, na.rm = T),
            'income_resale'= sum(income_resale, na.rm = T)),
         by=.(Address, member)]
R76P4S03_unique <- as.data.frame(DT) 

DT <- data.table(U76P4S01)
DT <- DT[,.('employed_w'= first(employed_w),
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
U76P4S01_unique <- as.data.frame(DT)

DT <- data.table(U76P4S02)
DT <- DT[,.('employed_s'= first(employed_s),
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
U76P4S02_unique <- as.data.frame(DT) 

DT <- data.table(U76P4S03)
DT <- DT[,.('income_pension'= sum(income_pension, na.rm = T),
            'income_rent'= sum(income_rent, na.rm = T),
            'income_interest'= sum(income_interest, na.rm = T),
            'income_aid'= sum(income_aid, na.rm = T),
            'income_resale'= sum(income_resale, na.rm = T)),
         by=.(Address, member)]
U76P4S03_unique <- as.data.frame(DT) 

Rind76 <- r76data  |>
  left_join(select(R76P1,-degree)) |>
  left_join(R76P4S01_unique) |>
  left_join(R76P4S02_unique) |>
  left_join(R76P4S03_unique) |> 
  mutate(across(where(is.character),as.integer)) |>
  mutate(urban = "R")

Uind76 <- u76data |>
  left_join(select(U76P1,-degree)) |>
  left_join(U76P4S01_unique) |>
  left_join(U76P4S02_unique) |>
  left_join(U76P4S03_unique) |> 
  mutate(across(where(is.character),as.integer)) |>
  mutate(urban = "U")

IND76 <- bind_rows(Rind76,Uind76) |>
  mutate(urban = as.factor(urban),
         Global = 1000000,
         employed_w = factor(employed_w, levels = c(1,2), labels = c("Yes","No")),
         status_w = factor(status_w, levels = c(1,2), labels = c("public","private")),
         employed_s = factor(employed_s, levels = c(1,2), labels = c("Yes","No")),
         status_s = factor(status_s, levels = c(1,2,3), labels = c("employer","selfemployed","familyworker")),
         agriculture = factor(agriculture, levels = c(1,2), labels = c("agriculture","nonagriculture"))
  ) |>
  left_join(CPI) |>
  select(Address:urban, cpi_m = cpi, cpi_y) |>
  as.data.frame()

attr(IND76$occupationalst, "label") <- "employment status"
attr(IND76$employed_w, "label") <- "Whether employed in wage-earning job?"
attr(IND76$ISIC_w, "label") <- "Industry code of wage-earning job"
attr(IND76$ISCO_w, "label") <- "Occupation code of wage-earning job"
attr(IND76$status_w, "label") <- "Wage-earning job status: 1-public 2-cooperative 3-private"
attr(IND76$netincome_w_m, "label") <- "wage-earning job: net income previous month"
attr(IND76$netincome_w_y, "label") <- "wage earning job: net income previous year"
attr(IND76$employed_s, "label") <- "Whether employed in non-wage job?"
attr(IND76$ISIC_s, "label") <- "Industry code of non-wage job"
attr(IND76$ISCO_s, "label") <- "Occupation code of non-wage job"
attr(IND76$status_s, "label") <- "job status: 4-employer 2-selfemployed 3-familyworker"
attr(IND76$income_s_y, "label") <- "non-wage job: net income previous year"

saveRDS(IND76,"./exported/IND76.Rds")

###############################################
# Building Household Data

r76p1 <- R76P1 |>
  group_by(Address) |>
  summarize(size = sum(!is.na(member)),
            literates = sum(literacy == "literate", na.rm = TRUE),
            students = sum(studying == "Yes", na.rm = TRUE),
            employeds = sum(occupationalst == "employed", na.rm = TRUE)) 

u76p1 <- U76P1 |>
  group_by(Address) |>
  summarize(size = sum(!is.na(member)),
            literates = sum(literacy == "literate", na.rm = TRUE),
            students = sum(studying == "Yes", na.rm = TRUE),
            employeds = sum(occupationalst == "employed", na.rm = TRUE)) 

r_head <- R76P1 |>
  filter(relation == "head") |>
  select(-member,-relation,-studying)

u_head <- U76P1 |>
  filter(relation == "head") |>
  select(-member,-relation,-studying)

# summary of heads job codes
r_job <- Rind76 |>
  filter(relation == "head") |>
  select(Address,starts_with("IS"))

u_job <- Uind76 |>
  filter(relation == "head") |>
  select(Address,starts_with("IS"))

# Sum of household expenditure items
r76p3 <- R76P3 |>
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

u76p3 <- U76P3 |>
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
r_NM_housing <- R76P3S03 |>
  filter(Global %in% c(1042111,1042211,1042212) ) |>
  group_by(Address) |>
  summarize(income_nm_house = sum(value*12, na.rm = T))

u_NM_housing <- U76P3S03 |>
  filter(Global %in% c(1042111,1042211,1042212) ) |>
  group_by(Address) |>
  summarize(income_nm_house = sum(value*12, na.rm = T))

r_NMincome <- R76P3 |>
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

u_NMincome <- U76P3 |>
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

DT <- data.table(Rind76)
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

DT <- data.table(Uind76)
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
RHH76 <- r76data |> 
  mutate(urban = "R") |>
  left_join(r76p1, by="Address") |>
  left_join(r_head, by = "Address") |>
  left_join(r_job, by = "Address") |>
  left_join(r76p3, by = "Address") |>
  left_join(r_incomeSum, by = "Address") |>
  left_join(r_NMincome, by = "Address") |>
  left_join(R76P2) |>
  mutate(across(income_w_y:income_nm_house, ~replace_na(.x, 0)))

UHH76 <- u76data |> 
  mutate(urban = "U") |>
  left_join(u76p1, by="Address") |>
  left_join(u_head, by = "Address") |>
  left_join(u_job, by = "Address") |>
  left_join(u76p3, by = "Address") |>
  left_join(u_incomeSum, by = "Address") |>
  left_join(u_NMincome, by = "Address") |>
  left_join(U76P2) |>
  mutate(across(income_w_y:income_nm_house, ~replace_na(.x, 0)))

HH76 <- bind_rows(RHH76, UHH76) |>
  mutate(urban = as.factor(urban)) |>
  mutate(expenditure = cost_food_tobacco + cost_clothing + cost_housing + cost_appliances + cost_health + cost_transport_communication + cost_recreation +  cost_miscellaneous
         + cost_durables/12 ,
         income = income_s_y + netincome_w_y + income_pension + income_rent + income_interest + income_aid  + income_nm_agriculture + income_nm_free + income_nm_homemade + income_nm_public + income_nm_private + income_nm_nonagriculture + income_nm_house,
         Global = 1000000 ) |>
  left_join(CPI) |>
  select(Address:income, cpi_m = cpi, cpi_y) |>
  as.data.frame()

attr(HH76$size, "label") <- "Household size"
attr(HH76$literates, "label") <- "Number of literate members"
attr(HH76$students, "label") <- "Number of student members"
attr(HH76$employeds, "label") <- "Number of employed members"
attr(HH76$gender, "label") <- "Head's gender"
attr(HH76$age, "label") <- "Head's age"
attr(HH76$literacy, "label") <- "Head's literacy"
attr(HH76$education, "label") <- "Head's education"
attr(HH76$occupationalst, "label") <- "Head's job status"
attr(HH76$maritalst, "label") <- "Head's marital status"
attr(HH76$ISIC_w, "label") <- "Head's Industry code of wage-earning job"
attr(HH76$ISCO_w, "label") <- "Head's Occupation code of wage-earning job"
attr(HH76$ISIC_s, "label") <- "Head's Industry code of non-wage job"
attr(HH76$ISCO_s, "label") <- "Head's Occupation code of non-wage job"
attr(HH76$income_nm_agriculture, "label") <- "Non-monetary income from agriculture"
attr(HH76$income_nm_nonagriculture, "label") <- "Non-monetary income from nonagriculture"
attr(HH76$income_nm_public, "label") <- "Non-monetary income from public sector job"
attr(HH76$income_nm_private, "label") <- "Non-monetary income from private sector job"
attr(HH76$income_nm_homemade, "label") <- "Non-monetary income from home production"
attr(HH76$netincome_w_m, "label") <- "wage-earning job: net income previous month"
attr(HH76$netincome_w_y, "label") <- "wage-earning job: net income previous year"
attr(HH76$income_w_y, "label") <- "wage-earning job: gross income previous year"
attr(HH76$income_s_y, "label") <- "non-wage job: net income previous year"
attr(HH76$income, "label") <- "total household income in previous year"
attr(HH76$expenditure, "label") <- "total monthly expendiuture of the household"

saveRDS(HH76, file = "./exported/HH76.Rds")


#######################################
# Exporting Rds files to CSV, STATA, etc.

#install.packages("haven") # uncomment if not already installed

df <- readRDS("./exported/HH76.Rds") # Specify the Rds file to convert here

#haven::write_dta(df, "./exported/HH76.dta") # to export in STATA
#write_csv2(df, "./exported/HH76.csv") # to export in CSV

