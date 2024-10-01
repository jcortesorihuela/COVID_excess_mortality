library(dplyr)
library(fastDummies)
library(tidyr)
library(nlme)
library(reshape2)
library(ggplot2)
library(zoo)
library(cowplot)
library(lubridate)

setwd("C:/Users/gmrad/Dropbox/COVID-19 excess mortality/Paper Repository")

# US data -----------------------------------------------------------------

df_govt <- read.csv("./Input data/Govt data/time_series_covid19_deaths_US.csv")
df_govt <- df_govt[,-c(1,2,3,4,5,6,8,9,10,11,12)]
df_govt <- melt(df_govt, id.vars = c("Province_State"))
df_govt <- df_govt %>%
  group_by(Province_State, variable) %>%
  summarize(cum_deaths_gov = sum(value, na.rm = T))

colnames(df_govt) <- c("country_name", "date", "cum_deaths_gov")
df_govt$date <- substr(df_govt$date, 2, nchar(as.character(df_govt$date)))
df_govt$date <- as.Date(df_govt$date, "%m.%d.%y")
df_govt$year <- year(df_govt$date)
df_govt$month <- month(df_govt$date)
df_govt$week <- isoweek(df_govt$date)

df_death <- read.csv("./Input data/Govt data/Weekly_Provisional_Counts_of_Deaths_by_State_and_Select_Causes__2020-2022.csv")
df_death$date <- as.Date(df_death$Week.Ending.Date)

df_govt <- df_govt[df_govt$date %in% df_death$date,]

write.csv(df_govt, "./Output data/Govt data/us_govt.csv", row.names = F)

# World data --------------------------------------------------------------

df_govt <- read.csv("./Input data/Govt data/time_series_covid19_deaths_global.csv")
df_govt <- df_govt[,-c(3,4)]
df_govt <- melt(df_govt, id.vars = c("Province.State", "Country.Region"))
df_govt <- df_govt %>%
  group_by(Country.Region, variable) %>%
  summarize(cum_deaths_gov = sum(value, na.rm = T))

colnames(df_govt) <- c("country_name", "date", "cum_deaths_gov")
df_govt$date <- substr(df_govt$date, 2, nchar(as.character(df_govt$date)))
df_govt$date <- as.Date(df_govt$date, "%m.%d.%y")
df_govt$year <- year(df_govt$date)
df_govt$month <- month(df_govt$date)
df_govt$week <- isoweek(df_govt$date)

df_death <- read.csv("./Input data/Govt data/Weekly_Provisional_Counts_of_Deaths_by_State_and_Select_Causes__2020-2022.csv")
df_death$date <- as.Date(df_death$Week.Ending.Date)

df_govt <- df_govt[df_govt$date %in% df_death$date,]

write.csv(df_govt, "./Output data/Govt data/world_govt.csv", row.names = F)
