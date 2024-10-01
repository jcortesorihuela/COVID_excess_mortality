library(reshape2)
library(readxl)
library(data.table)
library(tidyverse)

setwd("C:/Users/gmrad/Dropbox/COVID-19 excess mortality/Paper Repository")

# US states -----------------------------------------------------------------

df <- fread("./Input data/Death counts data/Multiple Cause of Death, 1999-2020.txt")

df <- df[df$Notes != "Total",]

df$year <- substr(df$`Month Code`,1,4)
df$time <- substr(df$`Month Code`,6,7)
df$country_name <- df$State
df$deaths <- df$Deaths

df <- df[,c("country_name", "year", "time", "deaths")]

df_covid <- fread("./Input data/Death counts data/Provisional Mortality Statistics, 2018 through Last Month.txt")

df_covid$year <- substr(df_covid$`Month Code`,1,4)
df_covid$time <- substr(df_covid$`Month Code`,6,7)
df_covid$country_name <- df_covid$`Residence State`
df_covid$deaths <- df_covid$Deaths

df_covid <- df_covid[,c("country_name", "year", "time", "deaths")]
df_covid <- df_covid[df_covid$year %in% 2021,]

df <- rbind(df, df_covid)
df <- arrange(df, country_name, year, time)

write.csv(df, "./Output data/Death counts data/us_states_mortality.csv", row.names = F)

rm(list = ls())

# Europe ------------------------------------------------------------------

i <- 1

df_all <- c()

for (i in 1:12){
  
  df <- read_excel("./Input data/Death counts data/demo_mmonth_spreadsheet.xlsx", sheet = i + 3, skip = 7)
  
  df <- cbind(df[,1], df[,as.character(2000:2021)])
  df[,2:ncol(df)] <- apply(df[,2:ncol(df)], MARGIN = 2, FUN = as.numeric)
  df <- df[complete.cases(df),]
  df <- df[df$TIME != "Germany including former GDR",]
  colnames(df)[1] <- "country_name"
  
  df <- melt(df, id.vars = "country_name", variable.name = "year", value.name = "deaths")
  df$month <- i
  
  df_all <- rbind(df_all, df)
  
}

df_all <- arrange(df_all, country_name, year, month)

df_all <- df_all %>%
  group_by(country_name) %>%
  filter(n() == 264)

df_all <- df_all[,c("country_name", "year", "month", "deaths")]

colnames(df_all)[3] <- "time" 

write.csv(df_all, "./Output data/Death counts data/europe_mortality.csv", row.names = F)






