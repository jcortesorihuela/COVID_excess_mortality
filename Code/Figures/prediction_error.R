library(dplyr)
library(stringr)
library(countrycode)
library(ggplot2)

setwd("C:/Users/gmrad/Dropbox/COVID-19 excess mortality/Paper Repository/")

models <- c("karlinsky", "linear", "who", "negbin", "negbin_lin_trend")

# Pennsylvania --------------------------------------------------------------

df_plot <- c()

for (model in models){
  
  df <- read.csv(paste0("./Output data/Cross-validation data/",
                        model, "_us_2013.csv"))
  
  df <- df %>%
    group_by(country_name) %>%
    mutate_at(vars(matches("iter") | matches("deaths")), cumsum)
  
  df_plot_aux <- df[df$country_name == "Pennsylvania",]
  df_plot_aux[,-c(1,2,3,4,5)] <- df_plot_aux$deaths - df_plot_aux[,-c(1,2,3,4,5)]
  df_plot_aux$ci_low <- apply(df_plot_aux[,-c(1,2,3,4,5)], MARGIN = 1, FUN = quantile, prob = 0.025)
  df_plot_aux$ci_high <- apply(df_plot_aux[,-c(1,2,3,4,5)], MARGIN = 1, FUN = quantile, prob = 1-0.025)
  df_plot_aux$yearmon <- zoo::as.yearmon(paste0(df_plot_aux$year, "-", df_plot_aux$time))
  df_plot_aux <- df_plot_aux[,c("country_name", "yearmon", "ci_low", "ci_high")]
  
  
  df_plot_aux$model <- model
  
  df_plot <- rbind(df_plot_aux, df_plot)
  
}

df_plot$Model <-  NA
df_plot$Model[df_plot$model == "karlinsky"] <- "Linear model with a linear time trend"
df_plot$Model[df_plot$model == "linear"] <- "Log-linear model with a linear time trend"
df_plot$Model[df_plot$model == "who"] <- "Bayesian negative binomial model with a spline trend"
df_plot$Model[df_plot$model == "negbin"] <- "Frequentist negative binomial model with a spline trend"
df_plot$Model[df_plot$model == "negbin_lin_trend"] <- "Frequentist negative binomial model with a linear trend"

df_plot$Model <- factor(df_plot$Model, levels = c("Linear model with a linear time trend",
                                                  "Log-linear model with a linear time trend",
                                                  "Bayesian negative binomial model with a spline trend",
                                                  "Frequentist negative binomial model with a spline trend",
                                                  "Frequentist negative binomial model with a linear trend"))

p <- ggplot(data = df_plot) +
  facet_wrap(~Model, ncol = 5) + 
  geom_ribbon(aes(x = yearmon, ymin = ci_low, ymax = ci_high), 
              fill = "grey", color = "black") +
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  theme_test() +
  xlab("Time") + 
  ylab("Cumulative excess death interval") +
  scale_x_continuous(breaks = 2018:2019)

p

ggsave("./Figures/pennsylvania_2013.png", p,
       width = 16.5, heigh = 3, dpi = 400)
ggsave("./Figures/pennsylvania_2013.eps", p,
       width = 16.5, heigh = 3, dpi = 400)

# Greece --------------------------------------------------------------

df_plot <- c()

for (model in models){
  
  df <- read.csv(paste0("./Output data/Cross-validation data/",
                        model, "_europe_2013.csv"))
  
  df <- df %>%
    group_by(country_name) %>%
    mutate_at(vars(matches("iter") | matches("deaths")), cumsum)
  
  df_plot_aux <- df[df$country_name == "Greece",]
  df_plot_aux[,-c(1,2,3,4,5)] <- df_plot_aux$deaths - df_plot_aux[,-c(1,2,3,4,5)]
  df_plot_aux$ci_low <- apply(df_plot_aux[,-c(1,2,3,4,5)], MARGIN = 1, FUN = quantile, prob = 0.025)
  df_plot_aux$ci_high <- apply(df_plot_aux[,-c(1,2,3,4,5)], MARGIN = 1, FUN = quantile, prob = 1-0.025)
  df_plot_aux$yearmon <- zoo::as.yearmon(paste0(df_plot_aux$year, "-", df_plot_aux$time))
  df_plot_aux <- df_plot_aux[,c("country_name", "yearmon", "ci_low", "ci_high")]
  
  
  df_plot_aux$model <- model
  
  df_plot <- rbind(df_plot_aux, df_plot)
  
}

df_plot$Model <-  NA
df_plot$Model[df_plot$model == "karlinsky"] <- "Linear model with a linear time trend"
df_plot$Model[df_plot$model == "linear"] <- "Log-linear model with a linear time trend"
df_plot$Model[df_plot$model == "who"] <- "Bayesian negative binomial model with a spline trend"
df_plot$Model[df_plot$model == "negbin"] <- "Frequentist negative binomial model with a spline trend"
df_plot$Model[df_plot$model == "negbin_lin_trend"] <- "Frequentist negative binomial model with a linear trend"

df_plot$Model <- factor(df_plot$Model, levels = c("Linear model with a linear time trend",
                                                  "Log-linear model with a linear time trend",
                                                  "Bayesian negative binomial model with a spline trend",
                                                  "Frequentist negative binomial model with a spline trend",
                                                  "Frequentist negative binomial model with a linear trend"))


p <- ggplot(data = df_plot) +
  facet_wrap(~Model, ncol = 5) + 
  geom_ribbon(aes(x = yearmon, ymin = ci_low, ymax = ci_high), 
              fill = "grey", color = "black") +
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  theme_test() +
  xlab("Time") + 
  ylab("Cumulative excess death interval") +
  scale_x_continuous(breaks = 2018:2019)

plot(p)


ggsave("./Figures/greece_2013.png", p,
       width = 16.5, heigh = 3, dpi = 400)
ggsave("./Figures/greece_2013.eps", p,
       width = 16.5, heigh = 3, dpi = 400)
