install.packages("ggplot2")
install.packages("dplyr")
install.packages("broom")
install.packages("ggpubr")
install.packages("readxl")
install.packages("sandwich")
install.packages("lmtest")
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(readxl)
library(sandwich)
library(lmtest)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#regression_data.xlsx for the original sample, and regression_data_recent.xlsx for the extended sample
#Load dataframe from excel
VRP_dataset <- read_excel("regression_data.xlsx") %>%
  mutate(VRP = as.numeric(VRP)) %>%
  select(VRP,excess_return)


get_horizon_returns <- function(returns, h) {
  summed_returns <- zoo::rollapply(returns, width = h, FUN = sum, align = "left", fill = NA)/h
  return(summed_returns)
}

run_regression <- function(data, h) {
  horizon_returns <- get_horizon_returns(data$excess_return, h)
  
  regression_df <- data.frame(
    excess_return = horizon_returns,
    VRP = VRP_dataset$VRP
  )
  
  simple_VRP_model <- lm(excess_return ~ VRP, data = regression_df)
  summary(simple_VRP_model)
  
  nw_se <- NeweyWest(simple_VRP_model, lag = h - 1, prewhite = FALSE)
  coefs <- coeftest(simple_VRP_model, vcov. = nw_se)
  
  list(
    coef = coefs[, "Estimate"],
    tstat = coefs[, "t value"],
    r2 = summary(simple_VRP_model)$adj.r.squared,
    se = coefs[, "Std. Error"]
  )
}

regression_df <- data.frame(
  excess_return = get_horizon_returns(VRP_dataset$excess_return, 6),
  VRP = VRP_dataset$VRP
)

simple_VRP_model <- lm(excess_return ~ VRP, data = regression_df)
summary(simple_VRP_model)

regression_results <- tibble(
  horizon = 1:24,
  coefs = vector("list", 24),
  tstat = vector("list", 24),
  r2 = numeric(24),
  se = vector("list", 24)
)


result <- run_regression(VRP_dataset, 1)

for (i in 1:24) {
  result <- run_regression(VRP_dataset, i)
  regression_results$coefs[[i]] <- result$coef
  regression_results$tstat[[i]] <- result$tstat
  regression_results$r2[i] <- result$r2
  regression_results$se[[i]] <- result$se
}

