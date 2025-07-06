install.packages(c("readxl", "dplyr", "tibble", "zoo", "sandwich", "lmtest"))
library(readxl)     
library(dplyr)    
library(tibble)  
library(zoo)         
library(sandwich)     
library(lmtest) 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#Load dataframe from excel; full_data.xlsx for original sample, full_data_recent.xlsx for extended sample
regression_data <- read_excel("full_data.xlsx") %>%
  mutate(PEsc = PE*12,
         PDsc = PD*12,
         DFSPsc = DFSP*12,
         CAYsc = CAY
  )%>%
  select(-PE,-PD,-DFSP, -CAY)

get_horizon_returns <- function(returns, h) {
  summed_returns <- zoo::rollapply(returns, width = h, FUN = sum, align = "left", fill = NA)/h
  return(summed_returns)
}

simple_regressions <- function(dependent, independent, h) {
    horizon_returns <- get_horizon_returns(dependent, h)
    
    regression_df <- data.frame(
      excess_return = horizon_returns,
      expl = independent
    )
    
    simple_model <- lm(excess_return ~ expl, data = regression_df)
    summary(simple_model)
    
    Hodrick_se <- NeweyWest(simple_model, lag = h - 1, prewhite = FALSE)
    coefs <- coeftest(simple_model, vcov. = Hodrick_se)
    
    list(
      coef = coefs[, "Estimate"],
      tstat = coefs[, "t value"],
      r2 = summary(simple_model)$adj.r.squared,
      se = coefs[, "Std. Error"]
    )
}

predictors <- setdiff(colnames(regression_data), "Excess")

regression_results_simple_monthly <- tibble(
  predictor = predictors,
  coefs = vector("list", length(predictors)),
  tstat = vector("list", length(predictors)),
  r2 = numeric(length(predictors)),
  se = vector("list", length(predictors))
)

regression_results_simple_quarterly <- tibble(
  predictor = predictors,
  coefs = vector("list", length(predictors)),
  tstat = vector("list", length(predictors)),
  r2 = numeric(length(predictors)),
  se = vector("list", length(predictors))
)

regression_results_simple_yearly <- tibble(
  predictor = predictors,
  coefs = vector("list", length(predictors)),
  tstat = vector("list", length(predictors)),
  r2 = numeric(length(predictors)),
  se = vector("list", length(predictors))
)

for (i in seq_along(predictors)) {
  pred_name <- predictors[i]
  if(pred_name == "CAYsc") {
    result <- simple_regressions(regression_data$Excess[1:360], regression_data[[pred_name]][1:360], 1)
  } else {
    result <- simple_regressions(regression_data$Excess, regression_data[[pred_name]], 1)
  }
  
  regression_results_simple_monthly$coefs[[i]] <- result$coef
  regression_results_simple_monthly$tstat[[i]] <- result$tstat
  regression_results_simple_monthly$r2[i] <- result$r2
  regression_results_simple_monthly$se[[i]] <- result$se
}

regression_results_simple_quarterly <- tibble(
  predictor = predictors,
  coefs = vector("list", length(predictors)),
  tstat = vector("list", length(predictors)),
  r2 = numeric(length(predictors)),
  se = vector("list", length(predictors))
)


for (i in seq_along(predictors)) {
  pred_name <- predictors[i]
  if(pred_name == "CAYsc") {
    result <- simple_regressions(regression_data$Excess[1:360], regression_data[[pred_name]][1:360], 3)
  } else {
    result <- simple_regressions(regression_data$Excess, regression_data[[pred_name]], 3)
  }
  regression_results_simple_quarterly$coefs[[i]] <- result$coef
  regression_results_simple_quarterly$tstat[[i]] <- result$tstat
  regression_results_simple_quarterly$r2[i] <- result$r2
  regression_results_simple_quarterly$se[[i]] <- result$se
}

for (i in seq_along(predictors)) {
  pred_name <- predictors[i]
  if(pred_name == "CAYsc") {
    result <- simple_regressions(regression_data$Excess[1:360], regression_data[[pred_name]][1:360], 12)
  } else {
    result <- simple_regressions(regression_data$Excess, regression_data[[pred_name]], 12)
  }
  regression_results_simple_yearly$coefs[[i]] <- result$coef
  regression_results_simple_yearly$tstat[[i]] <- result$tstat
  regression_results_simple_yearly$r2[i] <- result$r2
  regression_results_simple_yearly$se[[i]] <- result$se
}



multiple_regressions <- function(dependent, independent, h) {
  horizon_returns <- get_horizon_returns(dependent, h)
  
  regression_df <- data.frame(excess_return = horizon_returns)

  for (i in 1:length(independent)) {
    regression_df[[paste0("expl_", i)]] <- independent[[i]]
  }
  
  simple_model <- lm(excess_return ~ ., data = regression_df)
  summary(simple_model)
  
  nw_se <- NeweyWest(simple_model, lag = h - 1, prewhite = FALSE)
  coefs <- coeftest(simple_model, vcov. = nw_se)
  
  list(
    coef = coefs[, "Estimate"],
    tstat = coefs[, "t value"],
    r2 = summary(simple_model)$adj.r.squared,
    se = coefs[, "Std. Error"]
  )
}


regression_results_multiple_monthly <- tibble(
  mult_pred_nr = 1:5,
  coefs = vector("list", 5),
  tstat = vector("list", 5),
  r2 = numeric(5),
  se = vector("list", 5)
)

regression_results_multiple_quarterly <- tibble(
  mult_pred_nr = 1:5,
  coefs = vector("list", 5),
  tstat = vector("list", 5),
  r2 = numeric(5),
  se = vector("list", 5)
)

regression_results_multiple_yearly <- tibble(
  mult_pred_nr = 1:5,
  coefs = vector("list", 5),
  tstat = vector("list", 5),
  r2 = numeric(5),
  se = vector("list", 5)
)






mult_1 <- list(regression_data$VRP,regression_data$PEsc)
mult_2 <- list(regression_data$VRP[1:360],regression_data$CAYsc[1:360])
mult_3 <- list(regression_data$PEsc[1:360], regression_data$CAYsc[1:360])
mult_4 <- list(regression_data$VRP[1:360], regression_data$PEsc[1:360], regression_data$CAYsc[1:360])
mult_5 <- list(regression_data$VRP, regression_data$PEsc,regression_data$TMSP,regression_data$RREL)

#Monthly
for (i in 1:5) {
  expl <- get(paste0('mult_',i))
  if(i == 2 | i == 3 | i == 4) {
    result <- multiple_regressions(regression_data$Excess[1:360],expl,1)
  } else {
    result <- multiple_regressions(regression_data$Excess,expl,1)
  }
  
  
  regression_results_multiple_monthly$coefs[[i]] <- result$coef
  regression_results_multiple_monthly$tstat[[i]] <- result$tstat
  regression_results_multiple_monthly$r2[i] <- result$r2
  regression_results_multiple_monthly$se[[i]] <- result$se
}

#Quarterly
for (i in 1:5) {
  expl <- get(paste0('mult_',i))
  if(i == 2 | i == 3 | i == 4) {
    result <- multiple_regressions(regression_data$Excess[1:360],expl,3)
  } else {
    result <- multiple_regressions(regression_data$Excess,expl,3)
  }
  
  regression_results_multiple_quarterly$coefs[[i]] <- result$coef
  regression_results_multiple_quarterly$tstat[[i]] <- result$tstat
  regression_results_multiple_quarterly$r2[i] <- result$r2
  regression_results_multiple_quarterly$se[[i]] <- result$se
}

#Yearly
for (i in 1:5) {
  expl <- get(paste0('mult_',i))
  if(i == 2 | i == 3 | i == 4) {
    result <- multiple_regressions(regression_data$Excess[1:360],expl,12)
  } else {
    result <- multiple_regressions(regression_data$Excess,expl,12)
  }
  
  regression_results_multiple_yearly$coefs[[i]] <- result$coef
  regression_results_multiple_yearly$tstat[[i]] <- result$tstat
  regression_results_multiple_yearly$r2[i] <- result$r2
  regression_results_multiple_yearly$se[[i]] <- result$se
}


