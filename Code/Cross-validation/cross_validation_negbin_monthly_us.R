# library(tidyverse)
# library(fastDummies)
# library(tidyr)
# library(nlme)
# library(reshape2)
# library(ggplot2)
# library(zoo)
# library(cowplot)
library(mgcv)

setwd("C:/Users/gmrad/Dropbox/COVID-19 excess mortality/Paper Repository/")

# Functions ---------------------------------------------------------------

rnbinom_v <- Vectorize(rnbinom, vectorize.args = "mu")

# Mortality data ----------------------------------------------------------

df <-  read.csv("./Output data/Death counts data/us_states_mortality.csv")

countries <- unique(df$country_name)
country_nums <- 1:length(countries)
years <- 1999:2013 

# Estimating expected deaths ------------------------------------------------

# Estimating expected deaths ------------------------------------------------

i <- 1

for (i in 1:length(years)){
  
  print(years[i])
  
  df_all <- c()
  
  j <- 1
  
  for (j in country_nums){
    
    country <- countries[j]
    
    print(c(j, country))
    
    X <- df[(df['country_name']==country), c('year','time','deaths')]
    X <- X[!is.na(X$deaths),]
    X <- X[(X$year >= years[i]),]
    X <- X[(X$year <= years[i] + 6),]
    #X$deaths <- log(X$deaths) # We model levels as negbin, no need to transform to logs
    
    X$pre = (X$year < years[i] + 5)
    X_pre <- X[X$pre == 1,]
    
    # Estimating regression model
    
    reg <- gam(deaths ~ s(year, k = length(unique(X_pre$year))) +
                 s(time, bs = "cc", k = length(unique(X_pre$time))),
               data = X_pre, family = nb(theta = NULL, link = "log"))
    
    # Creating data frame of predicted vs actual deaths during the pandemic
    
    X$post = (X$year == years[i] + 5 | X$year == years[i] + 6)
    baseline <- predict(reg, newdata = X[X$post == 1,], type = "response")
    X_post <- X[X$post == 1,]
    X_post$deaths_pred <- baseline[1:nrow(X_post)]
    
    # Parametric bootstrap under the assumption that deaths follow a negbin distribution
    
    df_test <- matrix(ncol = 1000, nrow = 24)
    colnames(df_test) <- paste0("iter_", 1:1000)
    
    set.seed(1234)
    
    for (k in 1:1000){
      
      # Make bootstrap Y
      y.star <- rnbinom_v(1, size = exp(reg$family$getTheta()), mu = reg$fitted.values)
      
      # Do bootstrap regression
      X_reg_bs <- X[X$pre == 1,]
      X_reg_bs$deaths_star <- y.star
      reg_bs <- gam(deaths_star ~ s(year, k = length(unique(X_reg_bs$year))) +
                      s(time, bs = "cc", k = length(unique(X_reg_bs$time))),
                    data = X_reg_bs, family = nb(theta = NULL, link = "log"))
      
      
      baseline_bs <- predict(reg_bs, newdata = X[X$post == 1,], type = "response")
      
      # Create bootstrapped predictions
      
      df_test[,k] <- rnbinom_v(1, size = exp(reg_bs$family$getTheta()), mu = baseline_bs)
      
    }
    
    df_country <- data.frame("country_name" = rep(country, 24),
                             "year" = X_post$year,
                             "time" = X_post$time,
                             "deaths" = X_post$deaths,
                             "deaths_pred" = X_post$deaths_pred)
    
    df_country <- cbind(df_country, df_test)
    
    df_all <- rbind(df_all, df_country)
    
  }
  
  write.csv(df_all, paste0("./Output data/Cross-validation data/negbin_us_", years[i],".csv"), 
            row.names = F)
  
}

rm(list = ls())
