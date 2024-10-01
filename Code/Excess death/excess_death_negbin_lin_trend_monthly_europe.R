library(mgcv)

setwd("C:/Users/gmrad/Dropbox/COVID-19 excess mortality/Paper Repository/")

# Functions ---------------------------------------------------------------

rnbinom_v <- Vectorize(rnbinom, vectorize.args = "mu")

# Mortality data ----------------------------------------------------------

df <-  read.csv("./Output data/Death counts data/europe_mortality.csv")

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
  #X$deaths <- log(X$deaths) # We model levels as negbin, no need to transform to logs
  
  X$pre = (X$year < 2020)
  
  X_pre <- X[X$pre == 1,]
  
  reg <- gam(deaths ~ 0 + year + factor(time),
             data = X_pre, family = nb(theta = NULL, link = "log"))
  
  X$post = (X$year >= 2020)
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
    reg_bs <- gam(deaths_star ~ 0 + year + factor(time),
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

write.csv(df_all, "./Output data/Excess death data/excess_death_negbin_lin_trend_monthly_europe.csv", row.names = F)
