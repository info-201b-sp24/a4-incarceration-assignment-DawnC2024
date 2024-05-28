library(tidyverse)
library(dplyr)
library(ggplot2)

inca <- read.csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-jail-rates-1990-WA.csv")
inca_state <- inca %>% 
  mutate(total_jail_pop = total_pop*total_jail_pop_rate/100000) %>% group_by(year) %>%
  summarise(total_jail_pop = sum(total_jail_pop),
            total_pop = sum(total_pop)) %>%
  mutate(total_jail_pop_rate = round(total_jail_pop*100000/total_pop, digit=2)) %>%
  mutate(county_name = "WA") %>% mutate(Location = county_name)
inca_king <- inca %>% filter(county_name == "King County") %>% mutate(Location = county_name)
totle_rate_trend <- ggplot() +
  geom_line(inca_state, mapping=aes(x=year, y=total_jail_pop_rate, linetype = Location)) +
  geom_line(inca_king, mapping=aes(x=year, y=total_jail_pop_rate, linetype = Location)) +
  labs(x="Year", y="Jail Rate (per 100,000)", title = "Jail Rate Over Time in Washington State") +
  theme(legend.position="right")

totle_rate_trend
