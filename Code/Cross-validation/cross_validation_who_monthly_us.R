library(mgcv)
library(tidyverse)
library(INLA) # make sure you have installed the latest testing branch of INLA package
library(posterior)
library(dplyr)

# The purpose of this file is to fit a negative binomial spline model
# to the countries with monthly mortality data to get expected monthly
# mortality in 2020-2021

#### Set the seed for this script ####
setwd("C:/Users/gmrad/Dropbox/COVID-19 excess mortality/Paper Repository/")
set.seed(42)

# Computing expected deaths -----------------------------------------------

#### Load in data ####
exp_obs_bymonthyear <- read.csv("./Output data/Death counts data/us_states_mortality.csv")
colnames(exp_obs_bymonthyear) <- c("iso3", "year", "month", "observed")

# Estimating expected deaths ---------------------------------------------------------------

exp_obs_bymonthyear_full <- exp_obs_bymonthyear
years <- 1999:2013 

k <- 1

for (k in 1:length(years)){
  
  print(years[k])
  
  #### Clean data for use in gam function ####
  exp_obs_bymonthyear <- exp_obs_bymonthyear_full %>% 
    select(iso3, year, month, observed) %>%
    filter(year <= years[k] + 4 & year >= years[k]) %>% 
    mutate(country_num = as.numeric(as.factor(iso3)),
           observed = floor(observed)) %>% 
    arrange(iso3, year, month)
  
  #### Create data frame to store expected monthly mortality in 2020-2021 ####
  # gamma_E and gamma_delta will contain, for each country time period, the 
  # parameters of the gamma distribution for the expected mortality
  countries <- unique(exp_obs_bymonthyear$iso3)
  acm_predictions <- data.frame(iso3 = rep(countries, each = 24),
                                year = rep(c(rep(years[k] + 5, 12), 
                                             rep(years[k] + 6, 12)), 
                                           times = length(countries)),
                                month = rep(c(1:12, 1:12), 
                                            times = length(countries)),
                                expected_acm = NA,
                                expected_acm_se = NA,
                                expected_log_acm = NA,
                                expected_log_acm_se = NA,
                                gamma_E = NA,
                                gamma_delta = NA,
                                gamma_E_nb = NA,
                                gamma_delta_nb = NA) %>%
    mutate(country_num = as.numeric(as.factor(iso3)))
  
  exp_obs_bymonthyear$expected_acm <- NA
  exp_obs_bymonthyear$expected_acm_se <- NA
  exp_obs_bymonthyear$expected_log_acm <- NA
  exp_obs_bymonthyear$expected_log_acm_se <- NA
  exp_obs_bymonthyear$gamma_E <- NA
  exp_obs_bymonthyear$gamma_delta <- NA
  
  num_samples <- 10000
  
  i <- 1
  
  #### Predict expected monthly mortality in 2018-2019 ####
  for(i in 1:max(exp_obs_bymonthyear$country_num)){
    whichs <- which(exp_obs_bymonthyear$country_num == i)
    temp <- exp_obs_bymonthyear[whichs, ]
    #temp <- temp[temp$year <= years[k] + 4,]
    
    # Fit gam
    # If there are less than 3 years of historical data, use a linear annual 
    # trend, rather than a spline trend
    if(length(unique(temp$year)) < 3){
      print(temp$iso3[1])
      annual_model <- gam(observed ~ year +
                            s(month, bs = "cc", k = length(unique(temp$month))),
                          data = temp, family = nb(theta = NULL, link = "log"))
    } else{
      annual_model <- gam(observed ~ s(year, k = length(unique(temp$year))) +
                            s(month, bs = "cc", k = length(unique(temp$month))),
                          data = temp, family = nb(theta = NULL, link = "log"))
    }
    overd <- exp(annual_model$family$getTheta())
    
    # Get predictions 
    pred <- predict(annual_model,
                    se.fit = TRUE,
                    type = "response",
                    newdata = data.frame(year = c(rep(years[k] + 5, 12),
                                                  rep(years[k] + 6, 12)),
                                         month = c(1:12, 1:12)))
    whichs_pred <- which(acm_predictions$country_num == i)
    acm_predictions[whichs_pred, "expected_acm"] <- pred$fit
    acm_predictions[whichs_pred, "expected_acm_se"] <- pred$se.fit
    pred_log <- predict(annual_model,
                        se.fit = TRUE,
                        newdata = data.frame(year = c(rep(years[k] + 5, 12),
                                                      rep(years[k] + 6, 12)),
                                             month = c(1:12, 1:12)))
    acm_predictions[whichs_pred, "expected_log_acm"] <- pred_log$fit
    acm_predictions[whichs_pred, "expected_log_acm_se"] <- pred_log$se.fit
    
    # Get gamma parameters
    gamma_E <- rep(0, 24)
    gamma_delta <- rep(0, 24)
    gamma_E_nb <- rep(0, 24)
    gamma_delta_nb <- rep(0, 24)
    for(j in 1:24){
      samples <- exp(rnorm(num_samples, mean = pred_log$fit[j], 
                           sd = pred_log$se.fit[j]))
      
      gamma_E[j] <- mean(samples)
      gamma_delta[j] <- ((gamma_E[j]) ^ 2) / var(samples)
      
      samples_nb <- rnbinom(num_samples, size = overd, mu = samples)
      
      gamma_E_nb[j] <- mean(samples_nb)
      gamma_delta_nb[j] <- ((gamma_E_nb[j]) ^ 2) / var(samples_nb)
    }
    acm_predictions[whichs_pred, "gamma_E"] <- gamma_E
    acm_predictions[whichs_pred, "gamma_delta"] <- gamma_delta
    acm_predictions[whichs_pred, "gamma_E_nb"] <- gamma_E_nb
    acm_predictions[whichs_pred, "gamma_delta_nb"] <- gamma_delta_nb
    
    # Get fitted values for historical time periods
    pred_hist <- predict(annual_model, se.fit = TRUE, type = "response")
    exp_obs_bymonthyear[whichs, "expected_acm"] <- pred_hist$fit
    exp_obs_bymonthyear[whichs, "expected_acm_se"] <- pred_hist$se.fit
    pred_log_hist <- predict(annual_model, se.fit = TRUE)
    exp_obs_bymonthyear[whichs, "expected_log_acm"] <- pred_log_hist$fit
    exp_obs_bymonthyear[whichs, "expected_log_acm_se"] <- pred_log_hist$se.fit
    
    num_hist <- length(pred_log_hist$fit)
    gamma_E_hist <- rep(0, num_hist)
    gamma_delta_hist <- rep(0, num_hist)
    for(j in 1:num_hist){
      samples <- exp(rnorm(num_samples, mean = pred_log_hist$fit[j], 
                           sd = pred_log_hist$se.fit[j]))
      
      gamma_E_hist[j] <- mean(samples)
      gamma_delta_hist[j] <- ((gamma_E_hist[j]) ^ 2) / var(samples)
      
    }
    exp_obs_bymonthyear[whichs, "gamma_E"] <- gamma_E_hist
    exp_obs_bymonthyear[whichs, "gamma_delta"] <- gamma_delta_hist
  }
  
  #### Save all expecteds, i.e. for pre 2020 and 2020-2021 ####
  
  #### Save expecteds for 2020-2021 ####
  acm_monthly_predictions_tier1 <- acm_predictions %>%
    select(iso3, year, month, expected_acm, expected_acm_se, expected_log_acm,
           expected_log_acm_se, gamma_E, gamma_delta, gamma_E_nb, 
           gamma_delta_nb) %>%
    mutate(gamma_sd = sqrt((gamma_E ^ 2) / gamma_delta),
           gamma_sd_nb = sqrt((gamma_E_nb ^ 2) / gamma_delta_nb))
  save(acm_monthly_predictions_tier1, 
       file = paste0("./Output data/Cross-validation data/acm_monthly_predictions_tier1_us_",years[k], ".RData"))
  
}

rm(list = ls())

# Computing uncertainty ---------------------------------------------------

years <- 1999:2013 

k <- 1

for (k in 1:length(years)){
  
  print(years[k])
  
  load(paste0("./Output data/Cross-validation data/acm_monthly_predictions_tier1_us_",years[k], ".RData"))
  exp_obs_bymonthyear <- read.csv("./Output data/Death counts data/us_states_mortality.csv")
  colnames(exp_obs_bymonthyear) <- c("iso3", "year", "month", "observed")
  exp_obs_bymonthyear <- exp_obs_bymonthyear[exp_obs_bymonthyear$year %in% unique(acm_monthly_predictions_tier1$year),]
  acm_monthly_predictions_tier1 <- acm_monthly_predictions_tier1 %>%
    rename(expected = expected_acm)
  
  ### mutate country, covariate dataframe for covariate model, and standardize relevant variables
  df.inla <- exp_obs_bymonthyear %>% 
    left_join(acm_monthly_predictions_tier1,by=c("iso3","year","month")) %>%
    mutate(observed = round(observed),
           expected = round(expected))
  
  ### Set Up INLA Covariate Model
  
  ### Model parameters
  pc.u <- 1
  pc.alpha <- 0.01
  hyperpc1 <- list(prec = list(prior = "pc.prec", param = c(pc.u, pc.alpha)))
  control.family1 = list(control.link = list(model = "log"))
  
  ### Model formula specification with RW2 and sum to 0 constraint on time-varying covariates
  model.formula = formula(observed ~ 1)
  
  ### Run INLA Model
  ### Uses expecteds as offset, with gamma_delta expecteds uncertainty using "variant = 2" from INLA testing branch
  pois.pred.INLA <- INLA::inla(model.formula,
                               data = df.inla, offset = log(gamma_E), scale = gamma_delta, family = "nbinomial",
                               control.predictor= list(compute = TRUE, link = 1),
                               control.compute = list(config = TRUE, cpo = TRUE),
                               control.family = list(variant = 2,
                                                     control.link = list(model = "log"),
                                                     hyper =  list(theta = list(initial = 0, fixed = TRUE))))
  
  ### Number of samples of estimates
  num_inla_samps = 1000
  ### How many country time points
  country.time.n = nrow(df.inla)
  ### Define models and model names to consider
  models <- c("Final Model")
  pred.in.pois <- pois.pred.INLA
  
  ### Function for Getting ACM / Expecteds  / Excess Estimates
  
  poisson.pred <- pred.in.pois
  mod <- models
  
  INLA.estimates.sampling <- function(poisson.pred,mod,
                                      country.time.n,num_inla_samps){
    
    final.df <- vector(mode = "list", length = 3)
    
    nb.draw <- function(expected.delta,theta.i){
      rnbinom(1, size = expected.delta, mu = exp(theta.i))
    }
    
    ### find indices of transition country-time points from observed to modeled
    ### this is for the purpose of benchmarking
    summarise.df <- df.inla %>%
      mutate(observe.na.ind = ifelse(is.na(observed),1,0)) %>%
      group_by(iso3) %>%
      summarise(sum.na = sum(observe.na.ind)) %>%
      filter(sum.na>0 & sum.na<24)
    trans.first.df <- df.inla %>%
      mutate(ind = 1:nrow(df.inla)) %>%
      filter(iso3 %in% summarise.df$iso3) %>%
      mutate(observe.na.ind = ifelse(is.na(observed),1,0)) %>%
      group_by(iso3) %>%
      filter(observe.na.ind==1) %>%
      slice(1) %>%
      dplyr::select(iso3,month,observe.na.ind,ind)
    partial.country.ind <- trans.first.df$ind
    
    
    # sample of num_inla_samps from posterior
    sample.df <- INLA::inla.posterior.sample(num_inla_samps,poisson.pred,seed = 25)
    
    # create dfs of Country, WHO Region, and months to merge in estimates with later
    excess.df.expec <- df.inla %>%
      dplyr::select(iso3,month)
    excess.df.acm <- df.inla %>%
      dplyr::select(iso3,month)
    excess.df.excess <- df.inla %>%
      dplyr::select(iso3,month)
    
    # loop over samples to replace estimates from subnational and mixed models, and fix benchmarking
    for(i in 1:num_inla_samps){
      
      #print(paste("Starting Iteration: ",i))
      
      # extracting posterior estimates of parameters
      theta.i <- sample.df[[i]]$latent[1:country.time.n]
      
      # for the time points for countries with partial data
      set.seed(42 + i)
      
      # Create iteration i covariate model estimates df
      set.seed(42 + i)
      excess.df.i <- data.frame(theta.i=theta.i,expected.delta=df.inla$gamma_delta,
                                expected.E=df.inla$gamma_E,Country=df.inla$iso3,
                                months=df.inla$month,
                                observed.ind=ifelse(!is.na(df.inla$observed),TRUE,FALSE),
                                observed=df.inla$observed,
                                expected.sim=rgamma(country.time.n,shape=df.inla$gamma_delta,rate=df.inla$gamma_delta/df.inla$gamma_E))
      
      # iteration i vectors for expecteds, acm, and excess
      # estimated acm are replaced by observed when available
      expec.i <- excess.df.i$expected.sim
      acm.i <- ifelse(excess.df.i$observed.ind,
                      df.inla$observed,
                      rnbinom(country.time.n, size = excess.df.i$expected.delta, mu = exp(theta.i)))
      excess.i <- acm.i- excess.df.i$expected.sim 
      
      
      # Add Sample i excess for each country, time point to df
      excess.df.expec <- cbind(excess.df.expec,as.data.frame(expec.i))
      excess.df.acm <- cbind(excess.df.acm,as.data.frame(unlist(acm.i)))
      excess.df.excess <- cbind(excess.df.excess,as.data.frame(unlist(excess.i)))
    }
    
    final.df <- list(excess.df.acm, excess.df.expec, excess.df.excess)
    return(final.df)
    
  } #INLA.estimates.sampling function
  
  
  
  ## Setup and Run the Country Estimates Function
  
  country.df <- INLA.estimates.sampling(pred.in.pois,models,country.time.n,
                                        num_inla_samps)
  
  ### Format Estimates
  
  expected = country.df[[2]]
  acm = country.df[[1]]
  excess = country.df[[3]]
  colnames(expected) <- c("Country", "months", 
                          paste0("expec", 1:num_inla_samps))
  colnames(acm) <- c("Country", "months",
                     paste0("acm", 1:num_inla_samps))
  colnames(excess) <- c("Country", "months",
                        paste0("excess", 1:num_inla_samps))
  
  df_aux <- data.frame("country_name" = acm[,1],
                       year = rep(rep(unique(acm_monthly_predictions_tier1$year), each = 12), 51),
                       "time" = acm[,2],
                       "deaths" = (acm$acm1),
                       "deaths_pred" = (rowMeans(expected[,3:(num_inla_samps+2)])))
  
  df_aux <- cbind(df_aux, expected[,-c(1,2)])
  
  colnames(df_aux)[-c(1,2,3,4,5)] <- paste0("iter_", 1:num_inla_samps)
  
  write.csv(df_aux, paste0("./Output data/Cross-validation data/who_us_", years[k],".csv"), 
            row.names = F)
  
  
}
