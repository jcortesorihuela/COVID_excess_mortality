library(tidyverse)

round2 = function(x, digits) {
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^digits
  z*posneg
}

setwd("C:/Users/gmrad/Dropbox/COVID-19 excess mortality/Paper Repository/")

# US ----------------------------------------------------------------------

df_constants <- read.csv("./Tables/summary_us_geographic.csv")

df_table <- data.frame("model" = c("karlinsky", "linear", "who", "negbin", "negbin_lin_trend"),
                       "share_significant_ed" = NA,
                       "share_significant_ed_c" = NA,
                       "share_significant_id" = NA,
                       "share_significant_id_c" = NA)

df_table_freq <- data.frame("model" = c("karlinsky", "linear", "who", "negbin", "negbin_lin_trend"),
                       "freq_significant_ed" = NA,
                       "freq_significant_ed_c" = NA,
                       "freq_significant_id" = NA,
                       "freq_significant_id_c" = NA)

model <- "karlinsky"

for (model in df_table$model){
  
  df_constants_aux <- df_constants[df_constants$model == model,]
  
  df_ed <- read.csv(paste0("./Output data/Excess death data/excess_death_", 
                           model,
                           "_monthly_us.csv"))
  df_ed <- df_ed %>%
    group_by(country_name) %>%
    mutate_at(vars(matches("iter") | matches("deaths")), cumsum)
  df_ed[,-c(1,2,3,4,5)] <- df_ed$deaths - df_ed[,-c(1,2,3,4,5)]
  df_sum <- df_ed[,c(1,2,3,4,5)]
  df_sum$ed <- df_ed$deaths - df_ed$deaths_pred
  df_sum$ci_low <- apply(df_ed[,-c(1,2,3,4,5)], MARGIN = 1, FUN = quantile, prob = 0.025)
  df_sum$ci_high <- apply(df_ed[,-c(1,2,3,4,5)], MARGIN = 1, FUN = quantile, prob = 1-0.025)
  df_sum <- df_sum[df_sum$year == 2021 & df_sum$time == 12,]
  
  df_sum$I_ed <- as.numeric(!(df_sum$ci_low <= 0 & 0 <= df_sum$ci_high))
  df_sum <- merge(df_sum, df_constants_aux, by = "country_name")
  df_sum$ci_low_c <- df_sum$ed-df_sum$constant*((df_sum$ed - df_sum$ci_low))
  df_sum$ci_high_c <- df_sum$ed+df_sum$constant*((df_sum$ci_high - df_sum$ed))
  df_sum$I_ed_c <- as.numeric(!(df_sum$ci_low_c <= 0 & 
                                  0 <= df_sum$ci_high_c))
  
  df_govt <- read.csv("./Output data/Govt data/us_govt.csv")
  df_govt <- df_govt[df_govt$year == 2021 & df_govt$week == 51,]
  df_govt <- df_govt[,c("country_name", "cum_deaths_gov")]
  
  df_sum <- merge(df_sum, df_govt, by = "country_name", all.x = T)
  
  df_sum$id <- df_sum$ed - df_sum$cum_deaths_gov
  
  print(df_sum$country_name[df_sum$id < 0])
  
  df_sum$I_id <- as.numeric(!(df_sum$ci_low <= df_sum$cum_deaths_gov & 
                                df_sum$cum_deaths_gov <= df_sum$ci_high))
  
  df_sum$I_id_c <- as.numeric(!(df_sum$ci_low_c <= df_sum$cum_deaths_gov & 
                                  df_sum$cum_deaths_gov <= df_sum$ci_high_c))
  
  df_table$share_significant_ed[which(df_table$model == model)] <- mean(df_sum$I_ed)
  df_table$share_significant_ed_c[which(df_table$model == model)] <- mean(df_sum$I_ed_c)
  df_table$share_significant_id[which(df_table$model == model)] <- mean(df_sum$I_id)
  df_table$share_significant_id_c[which(df_table$model == model)] <- mean(df_sum$I_id_c)
  
  df_table_freq$freq_significant_ed[which(df_table$model == model)] <- sum(df_sum$I_ed)
  df_table_freq$freq_significant_ed_c[which(df_table$model == model)] <- sum(df_sum$I_ed_c)
  df_table_freq$freq_significant_id[which(df_table$model == model)] <- sum(df_sum$I_id)
  df_table_freq$freq_significant_id_c[which(df_table$model == model)] <- sum(df_sum$I_id_c)
  
}

#write.csv(df_table, "./Paper vF/Tables/ed_id_correction_us.csv", row.names = F)
#write.csv(df_table_freq, "./Paper vF/Tables/ed_id_freq_correction_us.csv", row.names = F)

df_table_final <- df_table_freq[,c(1,4,5)]
df_table_final$freq_significant_id <- paste0(df_table_final$freq_significant_id,
                                             " (",
                                             round2(df_table$share_significant_id*100,1),
                                             "\\%)")

df_table_final$freq_significant_id_c <- paste0(df_table_final$freq_significant_id_c,
                                               " (",
                                               round2(df_table$share_significant_id_c*100,1),
                                               "\\%)")

write.csv(df_table_final, "./Tables/ed_id_final_correction_us_geographic.csv", row.names = F)

# Europe ----------------------------------------------------------------------

df_constants <- read.csv("./Tables/summary_europe_geographic.csv")

df_table <- data.frame("model" = c("karlinsky", "linear", "who", "negbin", "negbin_lin_trend"),
                       "share_significant_ed" = NA,
                       "share_significant_ed_c" = NA,
                       "share_significant_id" = NA,
                       "share_significant_id_c" = NA)

df_table_freq <- data.frame("model" = c("karlinsky", "linear", "who", "negbin", "negbin_lin_trend"),
                            "freq_significant_ed" = NA,
                            "freq_significant_ed_c" = NA,
                            "freq_significant_id" = NA,
                            "freq_significant_id_c" = NA)

model <- "karlinsky"

for (model in df_table$model){
  
  df_constants_aux <- df_constants[df_constants$model == model,]
  
  df_ed <- read.csv(paste0("./Output data/Excess death data/excess_death_", 
                           model,
                           "_monthly_europe.csv"))
  df_ed <- df_ed %>%
    group_by(country_name) %>%
    mutate_at(vars(matches("iter") | matches("deaths")), cumsum)
  df_ed[,-c(1,2,3,4,5)] <- df_ed$deaths - df_ed[,-c(1,2,3,4,5)]
  df_sum <- df_ed[,c(1,2,3,4,5)]
  df_sum$ed <- df_ed$deaths - df_ed$deaths_pred
  df_sum$ci_low <- apply(df_ed[,-c(1,2,3,4,5)], MARGIN = 1, FUN = quantile, prob = 0.025)
  df_sum$ci_high <- apply(df_ed[,-c(1,2,3,4,5)], MARGIN = 1, FUN = quantile, prob = 1-0.025)
  df_sum <- df_sum[df_sum$year == 2021 & df_sum$time == 12,]
  
  df_sum$I_ed <- as.numeric(!(df_sum$ci_low <= 0 & 0 <= df_sum$ci_high))
  df_sum <- merge(df_sum, df_constants_aux, by = "country_name")
  df_sum$ci_low_c <- df_sum$ed-df_sum$constant*((df_sum$ed - df_sum$ci_low))
  df_sum$ci_high_c <- df_sum$ed+df_sum$constant*((df_sum$ci_high - df_sum$ed))
  df_sum$I_ed_c <- as.numeric(!(df_sum$ci_low_c <= 0 & 
                                  0 <= df_sum$ci_high_c))
  
  df_govt <- read.csv("./Output data/Govt data/world_govt.csv")
  df_govt <- df_govt[df_govt$year == 2021 & df_govt$week == 51,]
  df_govt <- df_govt[,c("country_name", "cum_deaths_gov")]
  
  df_sum <- merge(df_sum, df_govt, by = "country_name", all.x = T)
  
  df_sum$id <- df_sum$ed - df_sum$cum_deaths_gov
  
  print(df_sum$country_name[df_sum$id < 0])
  
  df_sum$I_id <- as.numeric(!(df_sum$ci_low <= df_sum$cum_deaths_gov & 
                                df_sum$cum_deaths_gov <= df_sum$ci_high))
  
  df_sum$I_id_c <- as.numeric(!(df_sum$ci_low_c <= df_sum$cum_deaths_gov & 
                                  df_sum$cum_deaths_gov <= df_sum$ci_high_c))
  
  df_table$share_significant_ed[which(df_table$model == model)] <- mean(df_sum$I_ed)
  df_table$share_significant_ed_c[which(df_table$model == model)] <- mean(df_sum$I_ed_c)
  df_table$share_significant_id[which(df_table$model == model)] <- mean(df_sum$I_id)
  df_table$share_significant_id_c[which(df_table$model == model)] <- mean(df_sum$I_id_c)
  
  df_table_freq$freq_significant_ed[which(df_table$model == model)] <- sum(df_sum$I_ed)
  df_table_freq$freq_significant_ed_c[which(df_table$model == model)] <- sum(df_sum$I_ed_c)
  df_table_freq$freq_significant_id[which(df_table$model == model)] <- sum(df_sum$I_id)
  df_table_freq$freq_significant_id_c[which(df_table$model == model)] <- sum(df_sum$I_id_c)
  
}


df_table_final <- df_table_freq[,c(1,4,5)]
df_table_final$freq_significant_id <- paste0(df_table_final$freq_significant_id,
                                             " (",
                                             round2(df_table$share_significant_id*100,1),
                                             "\\%)")

df_table_final$freq_significant_id_c <- paste0(df_table_final$freq_significant_id_c,
                                               " (",
                                               round2(df_table$share_significant_id_c*100,1),
                                               "\\%)")

write.csv(df_table_final, "./Tables/ed_id_final_correction_europe_geographic.csv", row.names = F)

