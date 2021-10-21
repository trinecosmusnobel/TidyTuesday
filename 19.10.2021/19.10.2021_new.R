library(dplyr)
library(readr)
library(gganimate)
library(ggplot2)

tuesdata <- tidytuesdayR::tt_load(2021, week = 43)

pumpkins <- tuesdata$pumpkins

pumpkins$weight_lbs <- gsub(",", "", pumpkins$weight_lbs) 

pumpkins <- pumpkins %>% 
  mutate(weight_kg = as.numeric(parse_number(weight_lbs))) %>% 
  separate(id, into = c("year", "type"), sep = "-") %>%
  filter(country == "United States", 
         place != "DMG") %>%
  group_by(year, type) %>%
  mutate(mean_weight = mean(weight_kg),
         year = as.numeric(year),
         type = recode(type, 
                       "F" = "Field Pumpkin" ,
                       "P" = "Giant Pumpkin", 
                       "S" = "Giant Squash", 
                       "W" = "Giant Watermelon", 
                       "L" = "Long Gourd", 
                       "T" = "Tomato"))

pumpkins$type <- factor(pumpkins$type, levels = c("Giant Pumpkin", "Giant Squash", "Giant Watermelon",
                                    "Long Gourd", "Field Pumpkin", "Tomato"))

pumpkins <- pumpkins %>% distinct(mean_weight)
  
mean_an <- pumpkins %>%
  ggplot(aes(x=as.factor(year), y=mean_weight, group=type, color=type)) +
  geom_line(size = 1.25) +
  geom_point() +
  labs(x = "Year",
       y = "Weight in kg.",
       color = "Type",
       title = "Development of mean weight of pumpkins \n entered in US GPC contests ", subtitle = "By year and type, 2013 - 2021") +
  theme_linedraw() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, family = "serif", size =15, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  scale_color_manual(values = c("#ff6112",
                                "#f6a339", 
                                "#18ad3f",
                                "#fa8d8d",
                                "#b2efad",
                                "#da1313")) +
  scale_y_continuous(breaks = seq(0, 900, by = 50)) + 
  transition_reveal(year)

animate(mean_an, height = 750, width = 1000, res = 150)

anim_save("mean_an.gif")

