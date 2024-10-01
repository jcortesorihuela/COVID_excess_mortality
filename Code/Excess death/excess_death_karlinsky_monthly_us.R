setwd("C:/Users/gmrad/Dropbox/COVID-19 excess mortality/Paper Repository/")

# Mortality data ----------------------------------------------------------

df <-  read.csv("./Output data/Death counts data/us_states_mortality.csv")

countries <- unique(df$country_name)
country_nums <- 1:length(countries)

# Estimating expected deaths ------------------------------------------------

df_all <- c()

for (j in country_nums){
  
  country <- countries[j]
  
  print(c(j, country))
  
  # Preparing data
  
  X <- df[(df['country_name']==country), c('year','time','deaths')]
  X <- X[!is.na(X$deaths),]
  X <- X[X$year >= 2015,]
  X <- X[X$year <= 2021,]
  #X$deaths <- log(X$deaths) # Transforming to logs
  
  X$pre = (X$year < 2020)
  
  # Estimating regression model
  
  reg = lm(deaths ~ year + factor(time) + 0, data = X[X$pre == 1,])
  
  # Creating data frame of predicted vs actual deaths during the pandemic
  
  X$post = (X$year >= 2020)
  baseline <- predict(reg, newdata = X[X$post == 1,])
  X_post <- X[X$post == 1,]
  X_post$deaths_pred <- baseline[1:nrow(X_post)]
  
  # Residual bootstrap  
  
  set.seed(1234)
  
  # Creating adjusted residuals
  
  leverage <- influence(reg)$hat
  s <- residuals(reg)/sqrt(1-leverage)
  s <- s - mean(s)
  
  the.replication <- function(reg,s){
    
    # Make bootstrap residuals
    ep.star <- sample(s,size=length(reg$residuals),replace=TRUE)
    
    # Make bootstrap Y
    y.star <- fitted(reg)+ep.star
    
    # Estimate bootstrap regression
    X_reg_bs <- X[X$pre == 1,]
    X_reg_bs$deaths_star <- y.star
    reg_bs <- lm(deaths_star ~ year + factor(time) + 0, data = X_reg_bs)
    
    baseline_bs <- predict(reg_bs, newdata = X[X$post == 1,])
    
    # Create bootstrapped adjusted residuals
    leverage_bs <- influence(reg_bs)$hat
    s_bs   <- residuals(reg_bs)/sqrt(1-leverage_bs)
    s_bs   <- s_bs - mean(s_bs)
    
    X_post_bs <- X[X$post == 1,]
    X_post_bs$deaths_star <- baseline_bs[1:nrow(X_post)] + sample(s_bs,size=nrow(X_post),replace=TRUE)
    
    return((X_post_bs$deaths_star))
  
}
  
  ep.draws <- replicate(n=1000,the.replication(reg=reg,s=s))
  
  df_test <- as.data.frame(ep.draws)
  colnames(df_test) <- paste0("iter_", 1:ncol(df_test))
  
  df_country <- data.frame("country_name" = rep(country, 24),
                           "year" = X_post$year,
                           "time" = X_post$time,
                           "deaths" = (X_post$deaths),
                           "deaths_pred" = (X_post$deaths_pred))
  
  df_country <- cbind(df_country, df_test)
  
  df_all <- rbind(df_all, df_country)
  
  
}

write.csv(df_all, "./Output data/Excess death data/excess_death_karlinsky_monthly_us.csv", row.names = F)




