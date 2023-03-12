library(tidyverse)
library(data.table)
library(readxl)
library(emojifont)
library(ggtext)

rm(list = ls())

list_HH <- list.files(path = "exported", pattern = "^HH.*\\Rds$")
list_EXP <- list.files(path = "exported", pattern = "^EXP.*\\Rds$")


## number of households and individuals across years
Pop <- NULL
for (year in list_HH) {
  Pop <- assign(year, readRDS(paste0("exported/",year))) |> 
    group_by(urban) |>
    summarize(hh=sum(weight, na.rm=T), ind=sum(size*weight, na.rm=T)) |>
    mutate(year = parse_number(year)) |>
    bind_rows(Pop)
  rm(list=year)
}

## merging different rounds
EXP <- NULL
for (year in list_EXP) {
  EXP <- assign(year, readRDS(paste0("exported/",year))) |> 
    mutate(year = parse_number(year)) |>
    bind_rows(EXP)
  rm(list=year)
}


Folder <- "C:/Users/Mohammad/Dropbox/HEIS/"

Codes <- read_excel(paste0(Folder,"Codes.xlsx"), sheet = "categories", range = "A1:H250") |> 
  mutate(label = ifelse(classification<1e5,
                        paste(str_sub(classification,2),EN_label),
                        EN_label)) |>
  select(classification, label) |>
  filter(!is.na(classification))

exp <- EXP |>
  mutate(COICOP1 = paste(str_sub(Global,2,3),Table),
         coicop2 = Global %/% 1000,
         coicop3 = Global %/% 100,
         coicop4 = Global %/% 10) |>
  left_join(Codes, by = c("coicop2" = "classification")) |>
  rename(COICOP2 = label) |>
  left_join(Codes, by = c("coicop3" = "classification")) |>
  rename(COICOP3 = label) |>
  left_join(Codes, by = c("coicop4" = "classification")) |>
  rename(COICOP4 = label) |>
  select(year, urban, Global, starts_with("COICOP"), item, Value, Value_r)

exp1 <- exp |>
  mutate(group = COICOP1) |>
  group_by(group, year) |>
  summarise(value_r=sum(Value_r,na.rm = T), value=sum(Value,na.rm = T),.groups = "drop")

exp2 <- exp |>
  mutate(group = COICOP2) |>
  group_by(COICOP1, group, year) |>
  summarise(value_r=sum(Value_r,na.rm = T), value=sum(Value,na.rm = T),.groups = "drop")  |>
  filter(!is.na(group))

exp3 <- exp |>
  mutate(group = COICOP3) |>
  group_by(COICOP1, COICOP2, group, year) |>
  summarise(value_r=sum(Value_r,na.rm = T), value=sum(Value,na.rm = T),.groups = "drop") |>
  filter(!is.na(group))

exp4 <- exp |>
  mutate(group = COICOP4) |>
  group_by(COICOP1, COICOP2, COICOP3, group, year) |>
  summarise(value_r=sum(Value_r,na.rm = T), value=sum(Value,na.rm = T),.groups = "drop") |>
  filter(!is.na(group)) 


exp1 |>
  #filter(group!="investment", group!="transfers") |>
  mutate(GROUP=group) |>
  ggplot(aes(x = year, y = log(value_r), group = GROUP)) + 
  geom_line(data = exp1, aes(group = group), color = "grey75", size=.4) +
  geom_line(color = "#0b53c1", size = .9 ) +
  geom_point(shape = 21, fill = "white", color = "#0b53c1", size=.8) +
  scale_x_continuous(breaks = c(76,80,84,88,92,96), labels = ~ paste0(.x+1921,"-",str_pad(((.x+1922) %% 100),2,pad="0"))) +
  facet_wrap(~ GROUP, nrow = 7) +
  theme_minimal(base_family = "Inconsolata") +
  ylab("") + xlab("") +
  labs(title = "Real expenditure of Iranian households by 1-digit classification",
      # caption = "Data from Household Income and Expenditure Surveys of Iran. <br>
      #  <span style = 'font-family:fontawesome-webfont'> &#xf099;</span>__@HoseyniM__ "
       ) +
  theme(plot.caption = element_markdown(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave("graph/COICOP1.png", width = 2.5, height = 4)

exp2 |>
  filter(str_sub(COICOP1,1,2) %in% paste0("0",1:6)) |>
  mutate(GROUP = group) |>
  ggplot(aes(x = year, y = log(value_r), group = GROUP)) + 
  geom_line(data = exp2, aes(group = group), color = "grey75", size=.4) +
  geom_line(color = "#0b53c1", size = .9 ) +
  geom_point(shape = 21, fill = "white", color = "#0b53c1", size=.8) +
  scale_x_continuous(breaks = c(76,80,84,88,92,96,99), labels = ~ paste0(.x+1921,"-",str_pad(((.x+1922) %% 100),2,pad="0"))) +
  facet_wrap(~ GROUP, nrow = 7) +
  theme_minimal(base_family = "Inconsolata") +
  ylab("") + xlab("") +
  labs(title = "Real expenditure of Iranian households by 2-digit classification",
       # caption = "Data from Household Income and Expenditure Surveys of Iran. <br>
       #  <span style = 'font-family:fontawesome-webfont'> &#xf099;</span>__@HoseyniM__ "
  ) +
  theme(plot.caption = element_markdown(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave("graph/COICOP2_1.png", width = 3.5, height = 5)

`%!in%` <- Negate(`%in%`)

exp2 |>
  filter(str_sub(COICOP1,1,2) %!in% paste0("0",1:6)) |>
  mutate(GROUP = group) |>
  ggplot(aes(x = year, y = log(value_r), group = GROUP)) + 
  geom_line(data = exp2, aes(group = group), color = "grey75", size=.4) +
  geom_line(color = "#0b53c1", size = .9 ) +
  geom_point(shape = 21, fill = "white", color = "#0b53c1", size=.8) +
  scale_x_continuous(breaks = c(76,80,84,88,92,96,99), labels = ~ paste0(.x+1921,"-",str_pad(((.x+1922) %% 100),2,pad="0"))) +
  facet_wrap(~ GROUP, nrow = 7) +
  theme_minimal(base_family = "Inconsolata") +
  ylab("") + xlab("") +
  labs(title = "Real expenditure of Iranian households by 2-digit classification",
       # caption = "Data from Household Income and Expenditure Surveys of Iran. <br>
       #  <span style = 'font-family:fontawesome-webfont'> &#xf099;</span>__@HoseyniM__ "
  ) +
  theme(plot.caption = element_markdown(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave("graph/COICOP2_2.png", width = 5, height = 4)

exp3_food <- exp3 |>
  filter(str_sub(COICOP2,1,3) %in% paste0("01",1:2))

exp3_food |>
  mutate(GROUP = group)  |>
  ggplot(aes(x = year, y = log(value_r), group = GROUP)) + 
  geom_line(data = exp3_food, aes(group = group), color = "grey75", size=.4) +
  geom_line(color = "#0b53c1", size = .9 ) +
  geom_point(shape = 21, fill = "white", color = "#0b53c1", size=.8) +
  scale_x_continuous(breaks = c(76,80,84,88,92,96,99), labels = ~ paste0(.x+1921,"-",str_pad(((.x+1922) %% 100),2,pad="0"))) +
  facet_wrap(~ GROUP, nrow = 7) +
  theme_minimal(base_family = "Inconsolata") +
  ylab("") + xlab("") +
  labs(title = "Real expenditure of Iranian households on food by 3-digit classification",
       # caption = "Data from Household Income and Expenditure Surveys of Iran. <br>
       #  <span style = 'font-family:fontawesome-webfont'> &#xf099;</span>__@HoseyniM__ "
  ) +
  theme(plot.caption = element_markdown(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave("graph/COICOP3_food.png", width = 5, height = 4)

exp4_food <- exp4 |>
  filter(str_sub(COICOP2,1,3) %in% paste0("01",1:2))

exp4_food |>
  mutate(GROUP = group) |>
  ggplot(aes(x = year, y = log(value_r), group = GROUP)) + 
  geom_line(data = exp4_food, aes(group = group), color = "grey75", size=.4) +
  geom_line(color = "#0b53c1", size = .9 ) +
  geom_point(shape = 21, fill = "white", color = "#0b53c1", size=.8) +
  scale_x_continuous(breaks = c(76,80,84,88,92,96,99), labels = ~ paste0(.x+1921,"-",str_pad(((.x+1922) %% 100),2,pad="0"))) +
  facet_wrap(~ GROUP, nrow = 7) +
  theme_minimal(base_family = "Inconsolata") +
  ylab("") + xlab("") +
  labs(title = "Real expenditure of Iranian households by 2-digit classification",
       # caption = "Data from Household Income and Expenditure Surveys of Iran. <br>
       #  <span style = 'font-family:fontawesome-webfont'> &#xf099;</span>__@HoseyniM__ "
  ) +
  theme(plot.caption = element_markdown(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave("graph/COICOP4.png", width = 5, height = 4)


## Old graphs

ggplot(exp1, aes(x=year, y=log(value_r), color=group, shape=group)) +
  geom_point() + geom_line() +
  #facet_wrap(~Table, scales = "free_y") + 
  ggtitle("Total real expenditure (log)") + ylab("") +
  theme_bw()

ggsave("graph/total.png")

exp <- exp |>
  group_by(year) |>
  mutate(share=round(value/sum(value)*100,1))

ggplot(exp, aes(x=year, y=share, fill=group, label = share)) +
  geom_col() +
  geom_text(position = position_stack(vjust = .5), size=3) +
  ggtitle("Share of expenditure items") + ylab("") +
  theme_bw()

ggsave("graph/share.png")

exp <- EXP |>
  mutate(group = Table
    # group = as_factor(case_when(
    # Table %in% c("food","tobacco","food_tobacco") ~ "food \n tobacco",
    # Table %in% c("communication","transport","transport_communication") ~ "transport \n communication",
    # Table %in% c("recreation","restaurant") ~ "recreation \n restaurant",
    # TRUE ~ Table))
    ) |>
  group_by(group, year,urban) |>
  summarise(value_r=sum(Value_r,na.rm = T), value=sum(Value,na.rm = T)) |>
  left_join(Pop) |>
  mutate(value_r_ph=value_r/ind)

ggplot(exp, aes(x=year, y=log(value_r_ph), color=urban, shape=urban)) +
  geom_point() + geom_line() +
  facet_wrap(~group, scales = "free_y") + 
  ggtitle("Real per capita expenditure (log)") + ylab("") +
  theme_bw()
  
ggsave("graph/percapita.png")


EXP |> filter(Table=="miscellaneous") |>
  group_by(gl=as.factor(Global %/% 100),year) |>
  summarise(value=(sum(Value_r))) |>
  ggplot(aes(x=year, y=value,
             color=fct_reorder(gl,value,.desc = T),
             shape=fct_reorder(gl,value,.desc = T),
             linetype=fct_reorder(gl,value,.desc = T))) +
  geom_line() + geom_point()
