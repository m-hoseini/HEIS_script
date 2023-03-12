library(tidyverse) 
library(knitr)
library(kableExtra)

list_HH <- list.files(path = "exported", pattern = "^HH.*\\Rds$")

HH <- NULL
for (year in list_HH) {
  HH <- assign(year, readRDS(paste0("exported/",year))) |> 
    filter(!is.na(weight)) |>
    mutate(year = parse_number(year)) |>
    bind_rows(HH)
  rm(list=year)
}

saveRDS(HH,"exported/HH.rds")

## Part 1 Tables
HH |>
  mutate(rural = (urban=="R")*100,
         literate_share = literates/size*100,
         student_share = students/size*100,
         worker_share = employeds/size*100,
         female_head = (gender=="Female")*100,
         age_head = age,
         married_head = (maritalst=="Married")*100 ) |>
  group_by(year) |>
  summarize(across(c(khanevartype, rural, size, literate_share, student_share, worker_share, female_head, age_head, married_head),
                   ~weighted.mean(.x,weight,na.rm = T))) |>
  pivot_longer(khanevartype:married_head, names_to = "household_characteristics", values_to = "value") |>
  pivot_wider(household_characteristics, names_from="year", values_from="value")

HH |> group_by(year, education) |>
  summarize(number = sum(weight)) |>
  group_by(year) |>
  mutate(share = number/sum(number)*100) |>
  pivot_wider(education, names_from="year", values_from="share")


HH |> select(wage_earning=income_w_y,self_employment=income_s_y, 
              pension=income_pension, rent=income_rent, interest=income_interest, 
              aid=income_aid, resale=income_resale, transfer=income_transfer,subsidy, 
              weight, cpi_y,year) |>
  group_by(year) |>
  summarize(across(wage_earning:subsidy,~weighted.mean(.x/cpi_y*100,weight,na.rm = T))) |>
  pivot_longer(wage_earning:subsidy, names_to = "Income_type", values_to = "real_value") |>
  ggplot(aes(x=year,y=log(real_value),color=Income_type,shape=Income_type)) +
  geom_line() + geom_point() + theme_bw() + ggtitle("log of mean houshold income")
  
HH |> select(home_production=income_nm_miscellaneous, 
              homeownership=income_nm_house, public_service=income_nm_public,
              private_service=income_nm_private, agriculture=income_nm_agriculture, 
              nonagriculture=income_nm_nonagriculture, weight, cpi_y,year) |>
  group_by(year) |>
  summarize(across(home_production:nonagriculture,~weighted.mean(.x/cpi_y*100,weight,na.rm = T))) |>
  pivot_longer(home_production:nonagriculture, names_to = "source", values_to = "real_value") |>
  ggplot(aes(x=year,y=log(real_value),color=source,shape=source)) +
  geom_line() + geom_point() + theme_bw() + ggtitle("log of mean houshold non-monetary income")

  
#  pivot_wider(Income_type, names_from="year", values_from="value")



## part 2 Tables
HH |> group_by(year) |>
  summarize(across(vehicle:microwave,~weighted.mean(.x,weight,na.rm = T)*100)) |>
  pivot_longer(vehicle:microwave, names_to = "ownership", values_to = "value") |>
  pivot_wider(ownership, names_from="year", values_from="value")

HH |> group_by(year) |>
  summarize(across(pipewater:wastewater,~weighted.mean(.x,weight,na.rm = T)*100)) |>
  pivot_longer(pipewater:wastewater, names_to = "Facilities", values_to = "value") |>
  pivot_wider(Facilities, names_from="year", values_from="value")

HH |> group_by(year) |>
  summarize(across(celebration_m:occasions_other_y,~weighted.mean(.x,weight,na.rm = T)*100)) |>
  pivot_longer(celebration_m:occasions_other_y, names_to = "Events", values_to = "value") |>
  pivot_wider(Events, names_from="year", values_from="value")

HH |> group_by(year, tenure) |>
  summarize(number = sum(weight)) |>
  group_by(year) |>
  mutate(share = number/sum(number)*100) |>
  pivot_wider(tenure, names_from="year", values_from="share")

HH |> group_by(year, material) |>
  summarize(number = sum(weight)) |>
  group_by(year) |>
  mutate(share = number/sum(number)*100,
         material = fct_explicit_na(material, na_level = "Concrete/Metal")) |>
  pivot_wider(material, names_from="year", values_from="share")

HH |> group_by(year) |>
  summarize(across(room:space,~weighted.mean(.x,weight,na.rm = T)))
  

HH |>  group_by(year, province) |>
  summarize(car_ownership=weighted.mean(vehicle,weight)*100) |>
  pivot_wider(province, names_from="year", values_from="car_ownership")

## occupational distribution of by income
HH |> 
  group_by(year) |>
  mutate(Decile=as.factor(ntile(expenditure/size,10))) |> 
  group_by(year,emp=employeds>0,Decile) |> 
  summarize(n=sum(weight)) |> 
  pivot_wider(c(year,Decile),names_from = "emp", values_from = "n") |> 
  mutate(share=`FALSE`/(`TRUE`+`FALSE`)) |> 
  filter(Decile %in% c("1","5","10"),year>75) |>
  ggplot(aes(x=year, y=share, color=Decile, shape=Decile)) +
  geom_point() + geom_line() +
  labs(title = "share of household without an employed person in Deciles")
