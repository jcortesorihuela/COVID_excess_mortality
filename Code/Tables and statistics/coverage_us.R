library(tidyverse)
library(reshape2)

setwd("C:/Users/gmrad/Dropbox/COVID-19 excess mortality/Paper Repository/")

# Computing coverage and correction constant ----------------------------------------------------

df_constant <- data.frame(model = c("karlinsky", "linear", "who", "negbin", "negbin_lin_trend"),
                          constant = NA,
                          constant_q = NA,
                          coverage = NA)

j <- 1

for (j in 1:nrow(df_constant)){
  
  df <-  read.csv("./Output data/Death counts data/us_states_mortality.csv")
  
  files <- list.files(path = "./Output data/Cross-validation data/",
                      pattern = paste0(df_constant$model[j], "_us_"),
                      full.names = T)
  
  df <- c()
  
  for (i in 1:length(files)){
    
    print(i)
    
    df_aux <- read.csv(files[i]) 
    colnames(df_aux)[5] <- "deaths_pred"
    
    df_aux <- df_aux %>%
      group_by(country_name) %>%
      mutate_at(vars(matches("iter") | matches("deaths")), cumsum)
    
    df_aux <- df_aux[df_aux$year == max(df_aux$year) & df_aux$time == max(df_aux$time),]
    df <- rbind(df, df_aux)
    
  }
  
  df <- arrange(df, country_name, year)
  
  df_sum <- df[,c(1,2,3,4,5)]
  df_sum$ci_low <- apply(df[,-c(1,2,3,4,5)], MARGIN = 1, FUN = quantile, prob = 0.025)
  df_sum$ci_high <- apply(df[,-c(1,2,3,4,5)], MARGIN = 1, FUN = quantile, prob = 1-0.025)
  
  df_sum$I <- as.numeric(df_sum$ci_low <= df_sum$deaths &
                           df_sum$deaths <= df_sum$ci_high)
  
  mean(df_sum$I)
  
  if(mean(df_sum$I) <= 0.95){
    c_seq <- seq(from = 1, to = 10, by = 0.001)
    p_seq <- seq(from = 0.95, to = 1, by = 0.001)
  } else{
    c_seq <- seq(from = 1, to = 0, by = -0.001)
    p_seq <- seq(from = 0.95, to = 0.8, by = -0.001)
  }
  
  for (c in c_seq){
    
    df_sum$ci_low_c <- df_sum$deaths_pred-c*((df_sum$deaths_pred - df_sum$ci_low))
    df_sum$ci_high_c <- df_sum$deaths_pred+c*((df_sum$ci_high - df_sum$deaths_pred))
    df_sum$I_c <- as.numeric(df_sum$ci_low_c <= df_sum$deaths & 
                               df_sum$deaths <= df_sum$ci_high_c)
    
    if (mean(df_sum$I) <= 0.95 & mean(df_sum$I_c) >= 0.95){
      break
    } else if (mean(df_sum$I) >= 0.95 & mean(df_sum$I_c) <= 0.95){
      c <- c + 0.001
      df_sum$ci_low_c <- df_sum$deaths_pred-c*((df_sum$deaths_pred - df_sum$ci_low))
      df_sum$ci_high_c <- df_sum$deaths_pred+c*((df_sum$ci_high - df_sum$deaths_pred))
      df_sum$I_c <- as.numeric(df_sum$ci_low_c <= df_sum$deaths & 
                                 df_sum$deaths <= df_sum$ci_high_c)
      break
    }
    
  }
  
  for (p in p_seq){
    
    df_sum$ci_low_c_q <- apply(df[,-c(1,2,3,4,5)], MARGIN = 1, FUN = quantile, prob = 0.5*(1-p))
    df_sum$ci_high_c_q <- apply(df[,-c(1,2,3,4,5)], MARGIN = 1, FUN = quantile, prob = 1-0.5*(1-p))
    
    df_sum$I_c_q <- as.numeric(df_sum$ci_low_c_q <= df_sum$deaths & 
                                 df_sum$deaths <= df_sum$ci_high_c_q)
    
    mean(df_sum$I_c_q)
    
    if (mean(df_sum$I) <= 0.95 & mean(df_sum$I_c_q) >= 0.95){
      break
    } else if (mean(df_sum$I) >= 0.95 & mean(df_sum$I_c_q) <= 0.95){
      p  <- p  + 0.001
      df_sum$ci_low_c_q <- apply(df[,-c(1,2,3,4,5)], MARGIN = 1, FUN = quantile, prob = 0.5*(1-p))
      df_sum$ci_high_c_q <- apply(df[,-c(1,2,3,4,5)], MARGIN = 1, FUN = quantile, prob = 1-0.5*(1-p))
      
      df_sum$I_c_q <- as.numeric(df_sum$ci_low_c_q <= df_sum$deaths & 
                                   df_sum$deaths <= df_sum$ci_high_c_q)
      break
    }
    
  }
  
  df_constant$coverage[j] <- mean(df_sum$I)
  df_constant$constant[j] <- c
  df_constant$constant_q[j] <- ifelse(mean(df_sum$I_c_q) >= 0.95, p, Inf)
  
}

write.csv(df_constant, "./Tables/summary_us.csv", row.names = F)
