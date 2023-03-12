library(tidyverse) 
library(knitr)
library(kableExtra)

rm(list = ls())

# list_IND <- list.files(path = "exported", pattern = "^IND.*\\Rds$")
# 
# IND <- NULL
# for (year in list_IND) {
#   IND <- assign(year, readRDS(paste0("exported/",year))) |> 
#     filter(!is.na(weight)) |>
#     mutate(year = parse_number(year)) |>
#     bind_rows(IND)
#   rm(list=year)
# }
# 
#saveRDS(IND,"IND.rds")
IND <- readRDS("IND.rds")
IND |>
  mutate(literate = (literacy=="literate")*100,
         student = (studying=="Yes")*100,
         female = (gender=="Female")*100 ) |>
  group_by(year) |>
  summarize(across(c(khanevartype, literate, student, female, age, hours_w, days_w, hours_s, days_s),
                   ~weighted.mean(.x,weight,na.rm = T))) |>
  pivot_longer(khanevartype:days_s, names_to = "Individual_characteristics", values_to = "value") |>
  pivot_wider(Individual_characteristics, names_from="year", values_from="value")

IND |> group_by(year, education) |>
  summarize(number = sum(weight)) |>
  group_by(year) |>
  mutate(share = number/sum(number)*100) |>
  pivot_wider(education, names_from="year", values_from="share")

IND |> group_by(year, education) |>
  summarize(number = sum(weight)) |>
  group_by(year) |>
  mutate(share = number/sum(number)) |>
  ggplot(aes(x=year,y=share,fill=education)) + 
  geom_col() +
  #geom_text(aes(label = scales::percent(share)), position = position_stack(vjust = 0.5)) + 
  theme_bw() 

IND |> group_by(year, relation) |>
  summarize(number = sum(weight)) |>
  group_by(year) |>
  mutate(share = number/sum(number)*100) |>
  pivot_wider(relation, names_from="year", values_from="share")

IND |> group_by(year, maritalst) |>
  summarize(number = sum(weight)) |>
  group_by(year) |>
  mutate(share = number/sum(number)*100) |>
  pivot_wider(maritalst, names_from="year", values_from="share")

IND |> group_by(year, occupationalst) |>
  summarize(number = sum(weight)) |>
  group_by(year) |>
  mutate(share = number/sum(number)*100) |>
  pivot_wider(occupationalst, names_from="year", values_from="share")

IND |> group_by(year, status_w) |>
  summarize(number = sum(weight)) |>
  group_by(year) |>
  mutate(share = number/sum(number)*100) |>
  pivot_wider(status_w, names_from="year", values_from="share")

IND |> group_by(year, status_s) |>
  summarize(number = sum(weight)) |>
  group_by(year) |>
  mutate(share = number/sum(number)*100) |>
  pivot_wider(status_s, names_from="year", values_from="share")

IND |> 
  select(income_wageearner=netincome_w_y,wage=wage_w_y, perks=perk_w_y,income_selfemployed=income_s_y,
              pension=income_pension, rent=income_rent, interest=income_interest, 
              aid=income_aid, resale=income_resale, transfer=income_transfer,subsidy, 
              weight, cpi_y,year) |>
  group_by(year) |>
  summarize(across(income_wageearner:subsidy,~weighted.mean(.x/cpi_y*100,weight,na.rm = T))) |>
  pivot_longer(income_wageearner:subsidy, names_to = "Income_type", values_to = "real_value") |>
  ggplot(aes(x=year,y=log(real_value),color=Income_type,shape=Income_type)) +
  geom_line() + geom_point() + theme_bw() + ggtitle("log of mean individual income")

IND |> select(cost_employment:sale,weight, cpi_y,year) |>
  group_by(year) |>
  summarize(across(cost_employment:sale,~weighted.mean(.x/cpi_y*100,weight,na.rm = T))) |>
  pivot_longer(cost_employment:sale, names_to = "type", values_to = "real_value") |>
  ggplot(aes(x=year,y=log(real_value),color=type,shape=type)) +
  geom_line() + geom_point() + theme_bw() + ggtitle("log of mean cost for selfemployment income")

IND |> 
  select(netincome_w_y, income_s_y, weight, cpi_y,year, status_s, status_w, status) |>
  mutate(status=case_when(
    status_w=="public" |status=="public" ~ "public sector",
    status_s=="employer" | status=="employer" ~ "employer",
    status_w %in% c("private","cooperative")|status=="private" ~ "wage earner, private",
    is.na(status_w)&netincome_w_y>0 ~ "wage earner, private",
    #status_s=="familyworker" | status=="familyworker" ~ "familyworker",
    !is.na(status_s) | status_s %in% c("self-employed","familyworker") ~ "self-employed",
    !is.na(status) | status %in% c("self-employed","familyworker") ~ "self-employed",
    TRUE ~ NA_character_),
         income=ifelse(is.na(income_s_y), netincome_w_y, income_s_y),
    year = 1300+year) |>
  group_by(year, status) |>
  summarize(across(income,~weighted.mean(.x/cpi_y*100,weight,na.rm = T))) |>
  filter(!is.na(status)) |>
  ggplot(aes(x=year,y=log(income),color=status, shape=status, linetype=status, size=status)) +
  geom_line() + geom_point() + theme_bw() + ggtitle("log of mean individual income") +
  scale_size_manual( values = c(1,2,1,.5) ) 

## number of conscripted
IND |> 
  filter(year %in% 89:91) |> 
  mutate(ISCO_w=ifelse(is.na(ISCO_w),0,ISCO_w)) |> 
  select(Address, member, ISCO_w, year) |> 
  pivot_wider(id_cols = c(Address,member), names_from = "year", values_from = "ISCO_w", names_prefix = "isco") |> 
  filter(isco90==0112) |>
  mutate(length=case_when(
    isco91==112&isco89==112 ~ 3,
    isco91==112&(is.na(isco89)) ~ 2,
    (isco91!=112)&isco89==112 ~ 2,
    (isco91!=112)&(isco89!=112) ~ 1,
    TRUE ~ NA_real_)) |>
  filter(length!=3) |>
  group_by(length) |>
  summarise(L=sum(!is.na(isco90)))

IND |> 
  mutate(birth_y=year-age) |>
  filter(birth_y>=56, birth_y<=65) |>
  filter(age>=20, age<=23) |> 
  group_by(birth_y, gender) |> 
  summarise(in_school = weighted.mean((studying=="Yes"),weight,na.rm=TRUE)) |>
ggplot(mapping = aes(x=birth_y+1921, y=in_school, color=gender, linetype=gender)) +
  geom_line(size=1) + # geom_point() +
  labs(
    # title="Proportion of Men and Women Still in School Aged 20-23 Across Birth Cohorts",
    x="birth year",
    y="schooling proportion at 20-23 years old") +
  scale_x_continuous(breaks = seq(1976,1987,1)) + theme_bw() +
  theme(legend.position = c(0.25,0.92),
        legend.direction = "horizontal")
  
ggsave("proportion by birth cohort.pdf", width=6, height = 3.5)

IND |> filter(!is.na(hours_w)) |>
  mutate(birth_y=year-age,
         hours=ifelse(!is.na(hours_w),hours_w*days_w*4,hours_s*days_s*52),
         wage=ifelse(!is.na(hours_w),wage_w_m/hours,income_s_y/hours),
         Education=fct_explicit_na(education, "illiterate"),
         Education=fct_collapse(Education,
                                `below high-school`=c("illiterate","Elemantry","Other","Secondary"),
                                `high-school diploma`=c("HighSchool","Diploma"),
                                university=c("College","Bachelor","Master","PhD")),
         ocp = as.integer(str_sub(str_pad(ifelse(!is.na(hours_w),ISCO_w,ISCO_s),4,pad = "0"),1,1)),
         occupation = ifelse(ocp %in% c(1,2,3), "white-collar", "blue-collar"),
         wage=ifelse((Education=="university"|occupation=="white-collar")&age<23|
                      Education=="below high-school"&occupation=="white-collar",
                     NA,wage)) |>
  filter(#birth_y>=64, birth_y<=69,
         gender=="Male", age>=20, age<35,
         !is.na(wage)) |>
  group_by(age, Education, occupation) |>
  summarize(`earning mean (log)`=log(mean(wage/cpi_y,na.rm=T)), 
            `earning standard deviation (log)`=log(sd(wage/cpi_y,na.rm=T))) |>
  pivot_longer(ends_with("(log)"), names_to = "stat", values_to = "wage") |>
  ggplot(aes(x=age,y=wage, color=Education,shape=Education)) +
  geom_line()+geom_point(stroke=1) + facet_grid(stat~occupation, scales = "free") +
  theme_bw()+ theme(legend.position = "bottom")+ ylab("")+
  scale_shape_manual(values=c(NA,1,15)) +  scale_x_continuous(breaks = seq(20,34,2))

ggsave("earning.pdf", width=6, height = 3.5)

