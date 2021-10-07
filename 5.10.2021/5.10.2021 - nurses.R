# load the politicaldata package
library(politicaldata)
library(htmltools)
library(ggrepel)
library(usmap)
library(ggplot2)
library(dplyr)
library(tools)
library(rvest)
library(tidytuesdayR)
library(tidyr)
library(ggimage)

tuesdata <- tidytuesdayR::tt_load('2021-10-05')

nurses <- as_tibble(tuesdata$nurses)

names(nurses) <- tolower(gsub(" ", "_", names(nurses))) #no spaces in the names, please

y <- as_tibble(read.csv("1976-2020-president.csv")) %>% 
  filter(year == 2020) %>% 
  filter(party_simplified == "DEMOCRAT" | party_simplified == "REPUBLICAN") %>% 
  mutate(pct_share_democratic = candidatevotes/totalvotes*100,
         state = toTitleCase(tolower(state))) %>% 
  filter(party_simplified == "DEMOCRAT") %>%
  select(state, pct_share_democratic)

nurses <- left_join(nurses, y)

nurses <- nurses %>% drop_na(pct_share_democratic)

nurses[["pct_share_democratic"]] = ifelse(nurses[["pct_share_democratic"]] > 50, "dem", "rep")

nurses$hourly_wage_median


mean((nurses %>% filter(year == 2020))$hourly_wage_median)


ggplot(data = nurses %>% 
         filter(year == 2020), 
       aes(x = reorder(state, hourly_wage_median), 
           y = hourly_wage_median, fill = pct_share_democratic)) + 
  geom_bar(stat = "identity", width = 0.75) + 
  coord_flip() + 
  xlab(element_blank()) + 
  ylab("Median salary for registrered nurses") + 
  labs(fill = "2020 presidential election majority holder party",
       caption = "Dashed line represents the mean of the median salaries across states | Source: Data.World",
       title = "Median salary for registrered nurses in US states", subtitle = "Compared with 2020 presidential election majority holder party") + 
  theme_linedraw() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_y_continuous(labels=scales::dollar_format(), limits = c(0, 62), expand = c(0,0))+
  geom_text(aes(label = scales::dollar(hourly_wage_median)), 
            position = position_dodge(width = 1), hjust = -0.1, size = 2.5) +
  geom_hline(yintercept = mean((nurses %>% filter(year == 2020))$hourly_wage_median), 
             linetype = "dashed", size = 0.5) +
  scale_fill_manual(values = c("#0480c5", "#ff4810"), labels = c("Democratic", "Republican")) 
