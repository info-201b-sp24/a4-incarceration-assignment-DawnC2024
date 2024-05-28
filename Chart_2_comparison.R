library(tidyverse)
library(dplyr)
library(ggplot2)

inca_rate_wa <- read_csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-jail-rates-1990-WA.csv")

inca_rate_wa_summary <- inca_rate_wa %>% filter(year == 2010) %>%
  select(c(county_name,total_jail_pop_rate, aapi_jail_pop_rate)) %>%
  mutate(county_name = word(county_name, 1))
rate_comparison <- inca_rate_wa_summary %>%
  pivot_longer(-county_name, names_to = "variable", values_to = "rate") %>% 
  ggplot() + 
  geom_bar(aes(x = county_name, y = rate, fill = variable), stat = "identity", position = "dodge", width = 0.5) +
  labs(x = "County Name", y = "Jail Rate (per 100,000)", title = "2010 AAPI vs. Total Jail Rate in Washington State", 
       fill = "Ethnic Group") +
  scale_fill_manual(labels = c("AAPI", "All ethnic groups"), values = c("#f7746c", "#00bfc4")) +
  coord_flip()
rate_comparison
