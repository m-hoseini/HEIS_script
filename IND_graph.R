library(tidyverse) 
library(knitr)
library(kableExtra)

rm(list = ls())

list_IND <- list.files(path = "exported", pattern = "^IND.*\\Rds$")

IND <- NULL
for (year in list_IND) {
  IND <- assign(year, readRDS(paste0("exported/",year))) %>% 
    filter(!is.na(weight)) %>%
    mutate(year = parse_number(year)) %>%
    bind_rows(IND)
  rm(list=year)
}

IND %>%
  mutate(literate = (literacy=="literate")*100,
         student = (studying=="Yes")*100,
         female = (gender=="Female")*100 ) %>%
  group_by(year) %>%
  summarize(across(c(khanevartype, literate, student, female, age, hours_w, days_w, hours_s, days_s),
                   ~weighted.mean(.x,weight,na.rm = T))) %>%
  pivot_longer(khanevartype:days_s, names_to = "Individual_characteristics", values_to = "value") %>%
  pivot_wider(Individual_characteristics, names_from="year", values_from="value")

IND %>% group_by(year, education) %>%
  summarize(number = sum(weight)) %>%
  group_by(year) %>%
  mutate(share = number/sum(number)*100) %>%
  pivot_wider(education, names_from="year", values_from="share")

IND %>% group_by(year, education) %>%
  summarize(number = sum(weight)) %>%
  group_by(year) %>%
  mutate(share = number/sum(number)) %>%
  ggplot(aes(x=year,y=share,fill=education)) + 
  geom_col() +
  #geom_text(aes(label = scales::percent(share)), position = position_stack(vjust = 0.5)) + 
  theme_bw() 

IND %>% group_by(year, relation) %>%
  summarize(number = sum(weight)) %>%
  group_by(year) %>%
  mutate(share = number/sum(number)*100) %>%
  pivot_wider(relation, names_from="year", values_from="share")

IND %>% group_by(year, maritalst) %>%
  summarize(number = sum(weight)) %>%
  group_by(year) %>%
  mutate(share = number/sum(number)*100) %>%
  pivot_wider(maritalst, names_from="year", values_from="share")

IND %>% group_by(year, occupationalst) %>%
  summarize(number = sum(weight)) %>%
  group_by(year) %>%
  mutate(share = number/sum(number)*100) %>%
  pivot_wider(occupationalst, names_from="year", values_from="share")

IND %>% group_by(year, status_w) %>%
  summarize(number = sum(weight)) %>%
  group_by(year) %>%
  mutate(share = number/sum(number)*100) %>%
  pivot_wider(status_w, names_from="year", values_from="share")

IND %>% group_by(year, status_s) %>%
  summarize(number = sum(weight)) %>%
  group_by(year) %>%
  mutate(share = number/sum(number)*100) %>%
  pivot_wider(status_s, names_from="year", values_from="share")

IND %>% select(income_wageearner=income_w_y,wage=wage_w_y, perks=perk_w_y,income_selfemployed=income_s_y,
              pension=income_pension, rent=income_rent, interest=income_interest, 
              aid=income_aid, resale=income_resale, transfer=income_transfer,subsidy, 
              weight, cpi_y,year) %>%
  group_by(year) %>%
  summarize(across(income_wageearner:subsidy,~weighted.mean(.x/cpi_y*100,weight,na.rm = T))) %>%
  pivot_longer(income_wageearner:subsidy, names_to = "Income_type", values_to = "real_value") %>%
  ggplot(aes(x=year,y=log(real_value),color=Income_type,shape=Income_type)) +
  geom_line() + geom_point() + theme_bw() + ggtitle("log of mean individual income")

IND %>% select(cost_employment:sale,weight, cpi_y,year) %>%
  group_by(year) %>%
  summarize(across(cost_employment:sale,~weighted.mean(.x/cpi_y*100,weight,na.rm = T))) %>%
  pivot_longer(cost_employment:sale, names_to = "type", values_to = "real_value") %>%
  ggplot(aes(x=year,y=log(real_value),color=type,shape=type)) +
  geom_line() + geom_point() + theme_bw() + ggtitle("log of mean cost for selfemployment income")
