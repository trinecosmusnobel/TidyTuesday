library(dplyr)
library(ggplot2)

#read data
captured_vs_farmed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/capture-fisheries-vs-aquaculture.csv')

#we only want the aggregated world values, and I gather the dataset to make use of color aes.
ny_med_ekstra <- left_join(gather(
  captured_vs_farmed %>% 
    filter(Entity == "World"), 
  type, measure, 
  `Aquaculture production (metric tons)`:`Capture fisheries production (metric tons)`, 
  factor_key = TRUE) %>% mutate(measure = measure/1000000), 
  stock %>% filter(Entity == "World") %>% 
    dplyr::select(Year, "Share of fish stocks that are overexploited"))

global_plot <- 
  ggplot() + 
  geom_point(data = ny_med_ekstra, aes(x = Year, y = measure, color = type)) + 
  geom_line(data = ny_med_ekstra, aes(x = Year, y = measure, color = type)) +
  xlab(element_blank()) +
  ylab("Million metric tonnes") +
  guides(color=FALSE) + 
  labs(caption = "Source: OurWorldinData.org",
       title = "Global aquaculture production \n exceeded capture fisheries in 2013", 
       subtitle = "Development in aquaculture and capture fishing \n and the share of fish that are overexploitated") +
  theme(panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        plot.title = element_text(hjust = 0.5, size = 22, family = "Times", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 13)) +
  scale_x_continuous(breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020), expand = c(0,2)) +
  annotate("text", x = 2004, y = 98, label = "Capture") +
    annotate("text", x = 2000, y = 54, label = "Aquaculture") +
    annotate("text", x = 2003, y = 20, label = "Overexploitation share (%)") +
    geom_vline(xintercept = 2013, linetype = "dashed", alpha = 0.5, size = 0.5) +
  geom_point(data = ny_med_ekstra, aes(x = Year, y = `Share of fish stocks that are overexploited`)) +
  geom_line(data = ny_med_ekstra, aes(x = Year, y = `Share of fish stocks that are overexploited`)) +
  geom_line(data = filter(ny_med_ekstra, is.na(`Share of fish stocks that are overexploited`)==FALSE), aes(x = Year, y = `Share of fish stocks that are overexploited`), linetype = "dashed") +
  scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Share of fish that are overexploited (%)")) 
  
ggsave("final_plot.png", 
         global_plot, 
         width = 25,
         height = 20, units = "cm")