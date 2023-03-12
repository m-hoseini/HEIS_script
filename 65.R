library(tidyverse)
library(data.table)
library(readxl)

rm(list = ls())

load("./RAW/65.RData")

##############################
Province <- c(Markazi="00", Gilan="01", Mazandaran="02", AzarbaijanSharghi="03", AzarbaijanGharbi="04",
              Kermanshah="05", Kouzestan="06", Fars="07", Kerman="08", KhorasanRazavi="09",
              Esfahan="10", SistanBalouchestan="11", Kordestan="12", Hamedan="13", CharmahalBakhtiari="14",
              Lorestan="15", Ilam="16", KohkilouyeBoyerahamad="17", Boushehr="18", Zanjan="19", 
              Semnan="20", Yazd="21", Hormozgan="22", Tehran="23")

RW <- readRDS("pop63_75.rds") |>
  filter(R_U=="rural") |>
  select(province, pop=Pop1365)

UW <- readRDS("pop63_75.rds") |>
  filter(R_U=="urban") |>
  select(province, pop=Pop1365)  


R65Data <- R65P1 |> 
  mutate(Address = str_pad(ADDRESS,7,pad="0"),
         province = fct_recode(as.factor(substr(Address,2,3)), !!!Province)) |>
  left_join(RW) |>
  group_by(province) |>
  mutate(population=n(),
         weight=round(pop/population)) |>
  select(Address, weight, province) |>
  mutate_if(is.character, as.numeric)


U65Data <- U65P1 |> 
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
yesno <- c(Yes="1", No="2", NULL="", NULL="-", NULL=" ", NULL="_", NULL="0")
occupation <- c(employed="1", seasonalunemployed="2", IncomeWOJob="3", unemployed="4", Student="5", Housewife="6", Other="7", Oldmember="8")
marital <- c(Married ="1", Widowed="2", Divorced="3", Single="4")
yesno <- c(Yes="1", No="2", NULL="", NULL="-", NULL=" ", NULL="_", NULL="0")
status <- c(employer="1", selfemployed="2", public="3", private="4", familyworker="5")

R65P1 <- R65P1 |> 
  rename(
    Address = ADDRESS,
    member = COL01,
    relation = COL03,
    gender = COL04,
    age = COL05,
    maritalst = COL06,
    weeksinhousehold = COL08,
    degree = COL10,
    occupational = COL11,
    ISCO = COL12,
    ISIC = COL13,
    status = COL14,
    weeksinjob = COL15,
    nationality = COL07) |>
  mutate(literacy = ifelse(COL09 %in% c(1,2),1,2),
         studying = ifelse(COL09 == 1,1,2),
         occupationalst = str_sub(as.character(occupational),1,1),
         occupationalst2 = str_sub(as.character(occupational),2,2),
         across(where(is.character), as.integer),
         across(c(relation,gender,literacy,studying,degree,occupationalst,occupationalst2,maritalst,status), as.factor),
         relation = fct_recode(relation, !!!relation), 
         gender = fct_recode(gender, !!!gender),
         maritalst = fct_recode(maritalst, !!!marital),
         literacy = fct_recode(literacy, !!!literacy), 
         studying = fct_recode(studying, !!!yesno),
         occupationalst = fct_recode(occupationalst, !!!occupation),
         occupationalst2 = fct_recode(occupationalst2, !!!occupation),
         status = fct_recode(status, !!!status),
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

U65P1 <- U65P1 |> 
  rename(
    Address = ADDRESS,
    member = COL01,
    relation = COL03,
    gender = COL04,
    age = COL05,
    maritalst = COL06,
    weeksinhousehold = COL08,
    degree = COL10,
    occupational = COL11,
    ISCO = COL12,
    ISIC = COL13,
    status = COL14,
    weeksinjob = COL15,
    nationality = COL07) |>
  mutate(literacy = ifelse(COL09 %in% c(1,2),1,2),
         studying = ifelse(COL09 == 1,1,2),
         occupationalst = str_sub(as.character(occupational),1,1),
         occupationalst2 = str_sub(as.character(occupational),2,2),
         across(where(is.character), as.integer),
         across(c(relation,gender,literacy,studying,degree,occupationalst,occupationalst2,maritalst,status), as.factor),
         relation = fct_recode(relation, !!!relation), 
         gender = fct_recode(gender, !!!gender),
         maritalst = fct_recode(maritalst, !!!marital),
         literacy = fct_recode(literacy, !!!literacy), 
         studying = fct_recode(studying, !!!yesno),
         occupationalst = fct_recode(occupationalst, !!!occupation),
         occupationalst2 = fct_recode(occupationalst2, !!!occupation),
         status = fct_recode(status, !!!status),
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
material <- c(NULL="1",MetalBlock="3", BrickWood="4", Cement="2", WoodKesht="5", Other="6")
fuel <- c(Oil="1", Gasoline="2", NaturalGas="3", Electricity="4", Wood="5", AnimalOil="6", Other="7" )


R65P2 <- R65P2 |>
  rename(
    Address = ADDRESS,
    tenure = Q3,
    room = Q5,
    material = Q4,
    normal_home = Q1,
    household_numbers = Q2,
    vehicle = Q6_01,
    motorcycle = Q6_02,
    bicycle = Q6_03,
    radio = Q6_05,
    radiotape = Q6_06,
    TVbw = Q6_07,
    refridgerator = Q6_08,
    stove = Q6_09,
    pipewater = Q7_1,
    electricity = Q7_2,
    pipegas = Q7_3,
    bathroom = Q7_4,
    evapcooling = Q7_5) |>
  mutate(across(where(is.character), as.integer),
         tenure = fct_recode(as.factor(tenure), !!!tenure), 
         construction = material==1,
         material = fct_recode(as.factor(material), !!!material),
         across(c(vehicle:evapcooling), ~(.x == 1)) )

U65P2 <- U65P2 |>
  rename(
    Address = ADDRESS,
    tenure = Q3,
    room = Q5,
    material = Q4,
    space = Q8,
    apartment = Q6,
    second_home = Q7,
    normal_home = Q1,
    household_numbers = Q2,
    vehicle = Q9_01,
    motorcycle = Q9_02,
    bicycle = Q9_03,
    radio = Q9_05,
    radiotape = Q9_06,
    TVbw = Q9_07,
    TV = Q9_08,
    VHS_VCD_DVD = Q9_09,
    freezer = Q9_10,
    refridgerator = Q9_11,
    stove = Q9_12,
    vacuum = Q9_13,
    dishwasher = Q9_15,
    washingmachine = Q9_14,
    sewingmachine = Q9_04,
    pipewater = Q10_1,
    electricity = Q10_2,
    pipegas = Q10_3,
    telephone = Q10_6,
    bathroom = Q10_4,
    centralcooling = Q10_8,
    evapcooling = Q10_5,
    centralheating = Q10_7,
    swimmingpool = Q10_9) |>
  mutate(across(where(is.character), as.integer),
         tenure = fct_recode(as.factor(tenure), !!!tenure), 
         construction = material==1,
         material = fct_recode(as.factor(material), !!!material),
         across(c(vehicle:swimmingpool), ~(.x == 1)))

###################################
# Part 3

CPI <- readRDS("CPI.rds") |>
  filter(year == 1365) |>
  group_by(Global) |>
  summarise(across(cpi:cpi_y,~mean(.x, na.rm=T))) 

GlobalR <- readRDS("Global.rds") |>  # Global code for items
  select(Global, code = "R65") |>
  filter(!is.na(code))

GlobalU <- readRDS("Global.rds") |>  # Global code for items
  select(Global, code = "U65") |>
  filter(!is.na(code))

GlobalMR <- GlobalR |> filter(Global %/% 1e6 != 2)
GlobalYR <- GlobalR |> filter(Global %/% 1e6 == 2)
GlobalMU <- GlobalU |> filter(Global %/% 1e6 != 2)
GlobalYU <- GlobalU |> filter(Global %/% 1e6 == 2)

# Part 3, Table 1

R65P3S01 <- R65P3S01 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    kilogram = COL4_5,
    value = COL6) |>
  mutate(
    across(c(value,kilogram),  ~ as.numeric(as.character(.x)) ),
    table = 1L) |> 
  left_join(GlobalMR) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)


U65P3S01 <- U65P3S01 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    kilogram = COL4_5,
    value = COL6) |>
  mutate(
    across(c(value,kilogram),  ~ as.numeric(as.character(.x)) ),
    table = 1L) |> 
  left_join(GlobalMU) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)


# Part 3, Table 2

R65P3S02 <- R65P3S02 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    value = COL4) |>
  mutate(
    table = 2L) |> 
  left_join(GlobalMR) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U65P3S02 <- U65P3S02 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    value = COL4) |>
  mutate(
    table = 2L) |> 
  left_join(GlobalMU) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 3

R65P3S03 <- R65P3S03 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    value = COL4) |>
  mutate(
    table = 3L) |> 
  left_join(GlobalMR) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U65P3S03 <- U65P3S03 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    value = COL4) |>
  mutate(
    table = 3L) |> 
  left_join(GlobalMU) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 4

R65P3S04 <- R65P3S04 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    value = COL4) |>
  mutate(
    table = 4L) |> 
  left_join(GlobalMR) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U65P3S04 <- U65P3S04 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    value = COL4) |>
  mutate(
    table = 4L) |> 
  left_join(GlobalMU) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 5

R65P3S05 <- R65P3S05 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    value = COL4) |>
  mutate(
    table = 5L) |> 
  left_join(GlobalMR) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U65P3S05 <- U65P3S05 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    value = COL4) |>
  mutate(
    table = 5L) |> 
  left_join(GlobalMU) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 6

R65P3S06 <- R65P3S06  |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    value = COL4) |>
  mutate(
    table = 6L) |> 
  left_join(GlobalMR) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U65P3S06 <- U65P3S06  |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    value = COL4) |>
  mutate(
    table = 6L) |> 
  left_join(GlobalMU) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 7

R65P3S07 <- R65P3S07 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    value = COL4) |>
  mutate(
    table = 7L) |> 
  left_join(GlobalMR) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U65P3S07 <- U65P3S07 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    value = COL4) |>
  mutate(
    table = 7L) |> 
  left_join(GlobalMU) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)


# Part 3, Table 8 

R65P3S08 <- R65P3S08 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    value = COL4) |>
  mutate(
    table = 8L) |> 
  left_join(GlobalMR) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U65P3S08 <- U65P3S08 |>
  rename(
    Address = ADDRESS,
    code = COL1,
    purchased = COL3,
    value = COL4) |>
  mutate(
    table = 8L) |> 
  left_join(GlobalMU) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

# Part 3, Table 9


R65P3S09 <- R65P3S09 |>
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
  left_join(GlobalYR) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)

U65P3S09 <- U65P3S09 |>
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
  left_join(GlobalYU) |>
  left_join(CPI) |>
  mutate(value_r=value*100/cpi) |>
  select(Address:Global, value_r)


list2env(lapply(mget(ls(pattern = "P3S.*")),
                function(x) {x |> mutate(purchased=factor(purchased,
                                                           levels = c(1,2,3,4,5,6,7,8),
                                                           labels = c("purchased_official",
                                                                      "purchased_market",
                                                                      "homemade",
                                                                      "publicservice",
                                                                      "privateservice",
                                                                      "agriculture",
                                                                      "nonagriculture",
                                                                      "free")))} )
         , .GlobalEnv)

#############################
# Part 4, Table 1

R65P4S1$COL02 <- as.numeric(as.character(R65P4S1$COL02))
R65P4S01 <- R65P4S1 |>
  rename(
    Address = ADDRESS,
    member = COL01,
    ISCO_w = COL02,
    pastjob = COL03,
    wage_w_y = COL05,
    perk_w_y = COL07,
    netincome_w_y = COL09) |>
  mutate(status_w = 1,
         Global = 1000000) |>
  left_join(CPI) |>
  select(Address:status_w, cpi_m = cpi, cpi_y)

U65P4S1$COL02 <- as.numeric(as.character(U65P4S1$COL02))
U65P4S01 <- U65P4S1 |>
  rename(
    Address = ADDRESS,
    member = COL01,
    ISCO_w = COL02,
    pastjob = COL03,
    income_w_y = COL05,
    wage_w_y = COL07,
    perk_w_y = COL09,
    netincome_w_y = COL11) |>
  mutate(status_w = 1,
         Global = 1000000) |>
  left_join(CPI) |>
  select(Address:status_w, cpi_m = cpi, cpi_y)


# Part 4, Table 2
R65P4S2$COL02 <- as.numeric(as.character(R65P4S2$COL02))
R65P4S02 <- R65P4S2 |>
  rename(
    Address = ADDRESS,
    member = COL01,
    ISCO_w = COL02,
    pastjob = COL03,
    wage_w_y = COL05,
    perk_w_y = COL07,
    netincome_w_y = COL09) |>
  mutate(status_w = 2,
         Global = 1000000) |>
  left_join(CPI) |>
  select(Address:status_w, cpi_m = cpi, cpi_y)

U65P4S2$COL02 <- as.numeric(as.character(U65P4S2$COL02))
U65P4S02 <- U65P4S2 |>
  rename(
    Address = ADDRESS,
    member = COL01,
    ISCO_w = COL02,
    pastjob = COL03,
    income_w_y = COL05,
    wage_w_y = COL07,
    perk_w_y = COL09,
    netincome_w_y = COL11) |>
  mutate(status_w = 2,
         Global = 1000000) |>
  left_join(CPI) |>
  select(Address:status_w, cpi_m = cpi, cpi_y)

# Part 4, Table 3: For urban total income must be multiplied by 100
R65P4S3$COL02 <- as.numeric(as.character(R65P4S3$COL02))
R65P4S03 <- R65P4S3 |>
  rename(
    Address = ADDRESS,
    member = COL01,
    ISCO_s = COL02,
    pastjob = COL03,
    cost_employment = COL04,
    cost_raw = COL05,
    cost_machinery = COL06,
    cost_others = COL07,
    sale = COL08,
    income_s_y = COL09) |>
  mutate(agriculture = 1,
         Global = 1000000) |>
  left_join(CPI) |>
  select(Address:agriculture, cpi_m = cpi, cpi_y)

U65P4S3$COL02 <- as.numeric(as.character(U65P4S3$COL02))
U65P4S3$COL09 <- U65P4S3$COL09*100
U65P4S03 <- U65P4S3 |>
  rename(
    Address = ADDRESS,
    member = COL01,
    ISCO_s = COL02,
    pastjob = COL03,
    cost_employment = COL04,
    cost_raw = COL05,
    cost_machinery = COL06,
    cost_others = COL07,
    sale = COL08,
    income_s_y = COL09) |>
  mutate(agriculture = 1,
         Global = 1000000) |>
  left_join(CPI) |>
  select(Address:agriculture, cpi_m = cpi, cpi_y)

# Part 4, Table 4: For urban total income must be multiplied by 100
R65P4S4$COL02 <- as.numeric(as.character(R65P4S4$COL02))
R65P4S04 <- R65P4S4 |>
  rename(
    Address = ADDRESS,
    member = COL01,
    ISCO_s = COL02,
    pastjob = COL03,
    cost_employment = COL04,
    cost_raw = COL05,
    cost_machinery = COL06,
    cost_others = COL07,
    cost_tax = COL08,
    sale = COL09,
    income_s_y = COL10) |>
  mutate(agriculture = 2,
         Global = 1000000) |>
  left_join(CPI) |>
  select(Address:agriculture, cpi_m = cpi, cpi_y)

U65P4S4$COL02 <- as.numeric(as.character(U65P4S4$COL02))
U65P4S4$COL10 <- U65P4S4$COL10*100
U65P4S04 <- U65P4S4 |>
  rename(
    Address = ADDRESS,
    member = COL01,
    ISCO_s = COL02,
    pastjob = COL03,
    cost_employment = COL04,
    cost_raw = COL05,
    cost_machinery = COL06,
    cost_others = COL07,
    cost_tax = COL08,
    sale = COL09,
    income_s_y = COL10) |>
  mutate(agriculture = 2,
         Global = 1000000) |>
  left_join(CPI) |>
  select(Address:agriculture, cpi_m = cpi, cpi_y)



# Part 4, Table 5
R65P4S05 <- R65P4S5 |>
  mutate(item = case_when(
    COL01 == 511 ~ "pension",
    COL01 %in% c(512,513,514) ~ "rent",
    COL01 %in% c(515,516,517,518) ~ "interest",
    COL01 %in% c(521,522) ~ "aid",
    COL01 == 519 ~ "resale")) |>
  group_by(Address = ADDRESS, item) |>
  summarize(value=sum(COL04)) |>
  pivot_wider(Address,
              names_from = "item",
              names_prefix = "income_", 
              values_from = "value",
              values_fill = 0) |> 
  as.data.frame() |>
  mutate(Global = 1000000) |>
  left_join(CPI) |>
  select(Address,starts_with("income_"), cpi_y)


U65P4S05 <- U65P4S5 |>
  mutate(item = case_when(
    COL01 == 511 ~ "pension",
    COL01 %in% c(512,513,514) ~ "rent",
    COL01 %in% c(515,516,517,518) ~ "interest",
    COL01 %in% c(521,522) ~ "aid",
    COL01 == 519 ~ "resale")) |>
  group_by(Address = ADDRESS, item) |>
  summarize(value=sum(COL04)) |>
  pivot_wider(Address,
              names_from = "item",
              names_prefix = "income_", 
              values_from = "value",
              values_fill = 0) |> 
  as.data.frame() |>
  mutate(Global = 1000000) |>
  left_join(CPI) |>
  select(Address,starts_with("income_"), cpi_y)

rm(R65P4S1,U65P4S1,R65P4S2,U65P4S2,R65P4S3,U65P4S3)

rm(list = setdiff(ls(), ls(pattern = "65"))) # removing unnecessary objects

save.image(file="./exported/HEIS65.Rdata")

CPI <- readRDS("CPI.rds") |>
  filter(year == 1365) |>
  group_by(Global) |>
  summarise(across(cpi:cpi_y,~mean(.x, na.rm=T))) 


###############################################
# Item-level expenditure table
# Merging tables in a unique expenditure table

R65P3 <- bind_rows(mget(ls(pattern = "R65P3S.*")))
R65P3 <- R65P3 |>
  left_join(R65Data) |>
  mutate(urban = "R")

U65P3 <- bind_rows(mget(ls(pattern = "U65P3S.*")))
U65P3 <- U65P3 |>
  left_join(U65Data) |>
  mutate(urban = "U")

itemcode <- read_excel("Codes.xlsx", sheet = "itemLabel") |> select(-labelFA)
itemcode$item <- factor(itemcode$Global, levels = itemcode$Global, labels = itemcode$labelEN)

EXP65 <- bind_rows(R65P3,U65P3) |>
  mutate(urban = as.factor(urban), 
         recallperiod=case_when(Global %/% 1e6==2 ~ 1/12,  # one-year
                                Global %/% 1e6==3 ~ 15, # two-days
                                Global %/% 1e6==4 ~ 30, # one-day
                                TRUE ~ 1),
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
            Kilogram = sum(kilogram*weight, na.rm = T)) |>
  filter(Value!=0) |>
  left_join(itemcode) |>
  select(-labelEN) |>
  as.data.frame()

attr(EXP65$Table, "label") <- "Table in Part 3"
attr(EXP65$Global, "label") <- "global item code"
attr(EXP65$code, "label") <- "item code in this year"
attr(EXP65$urban, "label") <- "rural or urban"
attr(EXP65$Value, "label") <- "monthly expenditure"
attr(EXP65$Value_r, "label") <- "monthly expenditure in 1390 price"

saveRDS(EXP65, "./exported/EXP65.Rds")

###############################################
# Building individual level data

r65data <- R65Data |> select(Address, weight, province)
u65data <- U65Data |> select(Address, weight, province)

# faster code with data.table
R65P4S01_ <- bind_rows(R65P4S01,R65P4S02) |> arrange(Address,-netincome_w_y)
DT <- data.table(R65P4S01_)
DT <- DT[,.(
  'ISCO_w'= first(ISCO_w),
  'status_w'= first(status_w),
  'wage_w_y'= sum(wage_w_y, na.rm = T),
  'perk_w_y'= sum(perk_w_y, na.rm = T),
  'netincome_w_y'= sum(netincome_w_y, na.rm = T)),
  by=.(Address, member)]
R65P4S01_unique <- as.data.frame(DT) 

R65P4S02_ <- bind_rows(R65P4S03,R65P4S04) |> arrange(Address,-income_s_y)
DT <- data.table(R65P4S02_)
DT <- DT[,.(
  'ISCO_s'= first(ISCO_s),
  'agriculture'= first(agriculture),
  'income_s_y'= sum(income_s_y, na.rm = T),
  'cost_employment'= sum(cost_employment, na.rm = T),
  'cost_raw'= sum(cost_raw, na.rm = T),
  'cost_machinery'= sum(cost_machinery, na.rm = T),
  'cost_others'= sum(cost_others, na.rm = T),
  'cost_tax'= sum(cost_tax, na.rm = T),
  'sale'= sum(sale, na.rm = T)),
  by=.(Address, member)]
R65P4S02_unique <- as.data.frame(DT)

DT <- data.table(R65P4S05)
DT <- DT[,.('income_pension'= sum(income_pension, na.rm = T),
            'income_rent'= sum(income_rent, na.rm = T),
            #'income_interest'= sum(income_interest, na.rm = T),
            'income_aid'= sum(income_aid, na.rm = T),
            'income_resale'= sum(income_resale, na.rm = T)),
         by=.(Address)]
R65P4S03_unique <- as.data.frame(DT) 

U65P4S01_ <- bind_rows(U65P4S01,U65P4S02) |> arrange(Address,-netincome_w_y)
DT <- data.table(U65P4S01_)
DT <- DT[,.(
  'ISCO_w'= first(ISCO_w),
  'status_w'= first(status_w),
  'income_w_y'= sum(income_w_y, na.rm = T),
  'wage_w_y'= sum(wage_w_y, na.rm = T),
  'perk_w_y'= sum(perk_w_y, na.rm = T),
  'netincome_w_y'= sum(netincome_w_y, na.rm = T)),
  by=.(Address, member)]
U65P4S01_unique <- as.data.frame(DT) 

U65P4S02_ <- bind_rows(U65P4S03,U65P4S04) |> arrange(Address,-income_s_y)
DT <- data.table(U65P4S02_)
DT <- DT[,.(
  'ISCO_s'= first(ISCO_s),
  'agriculture'= first(agriculture),
  'income_s_y'= sum(income_s_y, na.rm = T),
  'cost_employment'= sum(cost_employment, na.rm = T),
  'cost_raw'= sum(cost_raw, na.rm = T),
  'cost_machinery'= sum(cost_machinery, na.rm = T),
  'cost_others'= sum(cost_others, na.rm = T),
  'cost_tax'= sum(cost_tax, na.rm = T),
  'sale'= sum(sale, na.rm = T)),
  by=.(Address, member)]
U65P4S02_unique <- as.data.frame(DT)

DT <- data.table(U65P4S05)
DT <- DT[,.('income_pension'= sum(income_pension, na.rm = T),
            'income_rent'= sum(income_rent, na.rm = T),
            'income_interest'= sum(income_interest, na.rm = T),
            'income_aid'= sum(income_aid, na.rm = T),
            'income_resale'= sum(income_resale, na.rm = T)),
         by=.(Address)]
U65P4S03_unique <- as.data.frame(DT) 

Rind65 <- r65data  |>
  left_join(select(R65P1,-degree,-COL09,-occupational)) |>
  left_join(R65P4S01_unique) |>
  left_join(R65P4S02_unique) |>
  left_join(R65P4S03_unique) |> 
  mutate(across(where(is.character),as.integer)) |>
  mutate(urban = "R")

Uind65 <- u65data |>
  left_join(select(U65P1,-degree,-COL09,-occupational)) |>
  left_join(U65P4S01_unique) |>
  left_join(U65P4S02_unique) |>
  left_join(U65P4S03_unique) |> 
  mutate(across(where(is.character),as.integer)) |>
  mutate(urban = "U")

IND65 <- bind_rows(Rind65,Uind65) |>
  mutate(urban = as.factor(urban),
         agriculture = factor(agriculture, levels = c(1,2), labels = c("agriculture","nonagriculture")),
         status_w = factor(status_w, levels = c(1,2), labels = c("public","private")),
         #status_s = ifelse(status %in% c("employer","selfemployed","familyworker"),status,NA),
         #ISIC_w = ifelse(ISIC %in% c("public","private"),ISIC,NA),
         #ISIC_s = ifelse(ISIC %in% c("employer","selfemployed","familyworker"),ISIC,NA),
         Global = 1000000) |>
  left_join(CPI) |>
  select(Address:urban, cpi_m = cpi, cpi_y) |>
  as.data.frame()

attr(IND65$occupationalst, "label") <- "employment status"
attr(IND65$status, "label") <- "Overal job status"
attr(IND65$ISIC, "label") <- "Industry code of job"
attr(IND65$ISCO_w, "label") <- "Occupation code of wage-earning job"
attr(IND65$status_w, "label") <- "Wage-earning job status: 1-public 2-cooperative 3-private"
attr(IND65$netincome_w_y, "label") <- "wage earning job: net income previous year"
attr(IND65$ISCO_s, "label") <- "Occupation code of non-wage job"
attr(IND65$income_s_y, "label") <- "non-wage job: net income previous year"

saveRDS(IND65,"./exported/IND65.Rds")

###############################################
# Building Household Data


r65p1 <- R65P1 |>
  group_by(Address) |>
  summarize(size = sum(!is.na(member)),
            literates = sum(literacy == "literate", na.rm = TRUE),
            students = sum(studying == "Yes", na.rm = TRUE),
            employeds = sum(occupationalst == "employed", na.rm = TRUE)) 

u65p1 <- U65P1 |>
  group_by(Address) |>
  summarize(size = sum(!is.na(member)),
            literates = sum(literacy == "literate", na.rm = TRUE),
            students = sum(studying == "Yes", na.rm = TRUE),
            employeds = sum(occupationalst == "employed", na.rm = TRUE)) 

r_head <- R65P1 |>
  filter(relation == "head") |>
  select(-member,-relation,-studying,-weeksinhousehold,-COL09,-degree,-occupational)

u_head <- U65P1 |>
  filter(relation == "head") |>
  select(-member,-relation,-studying,-weeksinhousehold,-COL09,-degree,-occupational)

# Sum of household expenditure items
r65p3 <- R65P3 |>
  mutate(recallperiod=case_when(Global %/% 1e6==3 ~ 15, # two-days
                                Global %/% 1e6==4 ~ 30, # one-day
                                TRUE ~ 1),
         value = value*recallperiod,
         value_r = value_r*recallperiod,
         nominal_gross = value + sell,
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

u65p3 <- U65P3 |>
  mutate(recallperiod=case_when(Global %/% 1e6==3 ~ 15, # two-days
                                Global %/% 1e6==4 ~ 30, # one-day
                                TRUE ~ 1),
         value = value*recallperiod,
         value_r = value_r*recallperiod,
         nominal_gross = value + sell,
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
r_NM_housing <- R65P3S03 |>
  filter(Global %in% c(1042111,1042211,1042212) ) |>
  group_by(Address) |>
  summarize(income_nm_house = sum(value*12, na.rm = T))

u_NM_housing <- U65P3S03 |>
  filter(Global %in% c(1042111,1042211,1042212) ) |>
  group_by(Address) |>
  summarize(income_nm_house = sum(value*12, na.rm = T))

r_NMincome <- R65P3 |>
  mutate(type = case_when(
    purchased %in% c("publicservice","cooperativeservice") ~ "public",
    purchased == "privateservice" ~ "private",
    purchased == "agriculture" ~ "agriculture",
    purchased == "nonagriculture" ~ "nonagriculture", 
    purchased %in% c("free","homemade") ~ as.character(purchased),
    TRUE ~ NA_character_),
    recallperiod=case_when(Global %/% 1e6==2 ~ 1,  # one-year
                           Global %/% 1e6==3 ~ 180, # two-days
                           Global %/% 1e6==4 ~ 360, # one-day
                           TRUE ~ 12)) |>
  group_by(Address, type) |>
  summarize(value = sum(value*recallperiod, na.rm = T)) |>
  filter(!is.na(type)&value!=0) |> 
  pivot_wider(Address, 
              names_from="type", names_prefix = "income_nm_", 
              values_from = "value",  values_fill = list(value = 0)) |>
  full_join(r_NM_housing) 
r_NMincome[is.na(r_NMincome)] <- 0

u_NMincome <- U65P3 |>
  mutate(type = case_when(
    purchased %in% c("publicservice","cooperativeservice") ~ "public",
    purchased == "privateservice" ~ "private",
    purchased == "agriculture" ~ "agriculture",
    purchased == "nonagriculture" ~ "nonagriculture", 
    purchased %in% c("free","homemade") ~ as.character(purchased),
    TRUE ~ NA_character_),
    recallperiod=case_when(Global %/% 1e6==2 ~ 1,  # one-year
                           Global %/% 1e6==3 ~ 180, # two-days
                           Global %/% 1e6==4 ~ 360, # one-day
                           TRUE ~ 12)) |>
  group_by(Address, type) |>
  summarize(value = sum(value*recallperiod, na.rm = T)) |>
  filter(!is.na(type)&value!=0) |> 
  pivot_wider(Address, 
              names_from="type", names_prefix = "income_nm_", 
              values_from = "value", values_fill = list(value = 0)) |>
  full_join(u_NM_housing) 
u_NMincome[is.na(u_NMincome)] <- 0


# sum of household income with data.table

DT <- data.table(Rind65)
DT <- DT[,.('income_s_y'= sum(income_s_y, na.rm = T),
            'netincome_w_y'= sum(netincome_w_y, na.rm = T),
            'income_pension'= sum(income_pension, na.rm = T),
            'income_rent'= sum(income_rent, na.rm = T),
            #'income_interest'= sum(income_interest, na.rm = T),
            'income_aid'= sum(income_aid, na.rm = T),
            'income_resale'= sum(income_resale, na.rm = T)),
         by=.(Address)]
r_incomeSum <- as.data.frame(DT) 

DT <- data.table(Uind65)
DT <- DT[,.('income_s_y'= sum(income_s_y, na.rm = T),
            'netincome_w_y'= sum(netincome_w_y, na.rm = T),
            'income_pension'= sum(income_pension, na.rm = T),
            'income_rent'= sum(income_rent, na.rm = T),
            'income_interest'= sum(income_interest, na.rm = T),
            'income_aid'= sum(income_aid, na.rm = T),
            'income_resale'= sum(income_resale, na.rm = T)),
         by=.(Address)]
u_incomeSum <- as.data.frame(DT)

# merging household-level data
RHH65 <- r65data |> 
  mutate(urban = "R") |>
  left_join(r65p1, by="Address") |>
  left_join(r_head, by = "Address") |>
  left_join(r65p3, by = "Address") |>
  left_join(r_incomeSum, by = "Address") |>
  left_join(r_NMincome, by = "Address") |>
  left_join(R65P2) |>
  mutate(across(income_s_y:income_nm_house, ~replace_na(.x, 0)))

UHH65 <- u65data |> 
  mutate(urban = "U") |>
  left_join(u65p1, by="Address") |>
  left_join(u_head, by = "Address") |>
  left_join(u65p3, by = "Address") |>
  left_join(u_incomeSum, by = "Address") |>
  left_join(u_NMincome, by = "Address") |>
  left_join(U65P2) |>
  mutate(across(income_s_y:income_nm_house, ~replace_na(.x, 0)))

HH65 <- bind_rows(RHH65, UHH65) |>
  mutate(urban = as.factor(urban)) |>
  mutate(expenditure = cost_food_tobacco + cost_clothing + cost_housing + cost_appliances + cost_health + cost_transport_communication + cost_recreation +  cost_miscellaneous
         + cost_durables/12 ,
         income = income_s_y + netincome_w_y + income_pension + income_rent + income_interest + income_aid  + income_nm_agriculture + income_nm_free + income_nm_homemade + income_nm_public + income_nm_private + income_nm_nonagriculture + income_nm_house,
         Global = 1000000 ) |>
  left_join(CPI) |>
  select(Address:income, cpi_m = cpi, cpi_y) |>
  as.data.frame()

attr(HH65$size, "label") <- "Household size"
attr(HH65$literates, "label") <- "Number of literate members"
attr(HH65$students, "label") <- "Number of student members"
attr(HH65$employeds, "label") <- "Number of employed members"
attr(HH65$gender, "label") <- "Head's gender"
attr(HH65$age, "label") <- "Head's age"
attr(HH65$literacy, "label") <- "Head's literacy"
attr(HH65$education, "label") <- "Head's education"
attr(HH65$occupationalst, "label") <- "Head's job status"
attr(HH65$maritalst, "label") <- "Head's marital status"
attr(HH65$ISIC, "label") <- "Head's Industry code"
attr(HH65$ISCO, "label") <- "Head's Occupation code"
attr(HH65$income_nm_agriculture, "label") <- "Non-monetary income from agriculture"
attr(HH65$income_nm_nonagriculture, "label") <- "Non-monetary income from nonagriculture"
attr(HH65$income_nm_public, "label") <- "Non-monetary income from public sector job"
attr(HH65$income_nm_private, "label") <- "Non-monetary income from private sector job"
attr(HH65$income_nm_homemade, "label") <- "Non-monetary income from home production"
attr(HH65$netincome_w_y, "label") <- "wage-earning job: net income previous year"
attr(HH65$income_s_y, "label") <- "non-wage job: net income previous year"
attr(HH65$income, "label") <- "total household income in previous year"
attr(HH65$expenditure, "label") <- "total monthly expendiuture of the household"

saveRDS(HH65, file = "./exported/HH65.Rds")


#######################################
# Exporting Rds files to CSV, STATA, etc.

#install.packages("haven") # uncomment if not already installed

df <- readRDS("./exported/HH65.Rds") # Specify the Rds file to convert here

#haven::write_dta(df, "./exported/HH65.dta") # to export in STATA
#write_csv2(df, "./exported/HH65.csv") # to export in CSV



