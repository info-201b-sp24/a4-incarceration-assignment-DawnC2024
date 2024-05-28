library(tidyverse)
library(dplyr)
library(ggplot2)

inca_nation <- read.csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-jail-rates-1990.csv")
inca_wa <- read_csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-jail-rates-1990-WA.csv")
summary <- data_frame()
summary_1990 <- list()
summary_1990$year <- 1990
summary_2010 <- list()
summary_2010$Year <- 2018
# Filter data for 1990 and 2018
inca_nation <- inca_nation %>% filter(year == 1990 | year == 2018)
inca_wa <- inca_wa %>% filter(county_name == "King County" & ( year == 1990 | year == 2018))

inca_nation_summary <- inca_nation %>% group_by(year, state) %>%
  summarize(total_jail_pop_rate = round(sum(total_pop*total_jail_pop_rate, na.rm = TRUE)/sum(total_pop, na.rm = TRUE), digits=2),
            aapi_jail_pop_rate = round(sum(total_pop*aapi_jail_pop_rate, na.rm = TRUE)/sum(total_pop, na.rm = TRUE), digits=2))

inca_nation_total <- inca_nation %>% group_by(year) %>%
  summarize(total_jail_pop_rate = round(sum(total_pop*total_jail_pop_rate, na.rm = TRUE)/sum(total_pop, na.rm = TRUE), digits=2),
            aapi_jail_pop_rate = round(sum(total_pop*aapi_jail_pop_rate, na.rm = TRUE)/sum(total_pop, na.rm = TRUE), digits=2))

summary_1990$nation_total = inca_nation_total %>% filter(year == 1990) %>% pull(total_jail_pop_rate)

nation_1990 <- inca_nation_summary %>% filter(year==1990) %>% arrange(desc(total_jail_pop_rate))
nation_1990_aapi <- inca_nation_summary %>% filter(year==1990) %>% arrange(desc(aapi_jail_pop_rate))
wa_1990 <- nation_1990 %>% filter(state == "WA")
summary_1990$wa_total = wa_1990 %>% pull(total_jail_pop_rate)
summary_1990$wa_rank = which(grepl("WA", nation_1990$state))
summary_1990$king_total = inca_wa %>% filter(year == 1990) %>% pull(total_jail_pop_rate)
summary_1990$nation_aapi = inca_nation_total %>% filter(year == 1990) %>% pull(aapi_jail_pop_rate)
summary_1990$wa_aapi = wa_1990 %>% pull(aapi_jail_pop_rate)
summary_1990$wa_rank_aapi = which(grepl("WA", nation_1990_aapi$state))
summary_1990$king_aapi = inca_wa %>% filter(year == 1990) %>% pull(aapi_jail_pop_rate)
nation_2010 <- inca_nation_summary %>% filter(year==2018) %>% arrange(desc(total_jail_pop_rate)) 
nation_2010_aapi <- inca_nation_summary %>% filter(year==2018) %>% arrange(desc(aapi_jail_pop_rate))
summary_2010$nation_total = inca_nation_total %>% filter(year == 2018) %>% pull(total_jail_pop_rate)
wa_2010 <- nation_2010 %>% filter(state == "WA") 
summary_2010$wa_total = wa_2010 %>% pull(total_jail_pop_rate)
summary_2010$wa_rank = which(grepl("WA", nation_2010$state))
summary_2010$king_total = inca_wa %>% filter(year == 2018) %>% pull(total_jail_pop_rate)
summary_2010$nation_aapi = inca_nation_total %>% filter(year == 2018) %>% pull(aapi_jail_pop_rate)
summary_2010$wa_aapi = wa_2010 %>% pull(aapi_jail_pop_rate)
summary_2010$wa_rank_appi = which(grepl("WA", nation_2010_aapi$state))
summary_2010$king_aapi = inca_wa %>% filter(year == 2018) %>% pull(aapi_jail_pop_rate)

df1 = data_frame()
df1 <- summary_1990
names(df1)<-c("year", "national total", "wa total", "wa rank",
              "king county total","nation aapi","wa aapi", "wa aapi rank", "king county aapi")
df2 = data.frame()
df2 <- summary_2010
names(df2)<-c("year", "national total", "wa total", "wa rank", "king county total",
              "nation aapi","wa aapi", "wa aapi rank", "king county aapi")

summary <- rbind(df1, df2)
rownames(summary) <- NULL
write.csv(summary, "summary.csv")
