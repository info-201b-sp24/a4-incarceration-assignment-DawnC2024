library(tidyverse)
library(dplyr)
library(stringr)
library(ggplot2)

inca_map_wa <- read_csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-jail-rates-1990-WA.csv")

inca_map_wa <- inca_map_wa %>% filter(year == "2010")
county_shape <-map_data("county") 
county_shape <-county_shape %>% filter(region == "washington") %>% mutate(county_name = str_to_title(paste(subregion, "County", sep=" "))) %>%
  left_join(inca_rate_wa, by = "county_name")
rate_map <- ggplot(county_shape) +
  geom_polygon(mapping = aes(x=long, y=lat, group=group,fill = total_jail_pop_rate.x), color="white",size=.1) +
  coord_map() +
  scale_fill_continuous(low = "#00bfc4", high = "#f7746c") +
  labs(x = "Longitude", y = "Latitude", fill = "Jail Rate", title="2010 Jail Rate in Washington State by County")

rate_map