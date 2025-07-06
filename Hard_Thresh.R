install.packages("glmnet")
install.packages("coefplot")
install.packages("lars")
install.packages("VSURF")
install.packages("dplyr")
install.packages("openxlsx")
install.packages("stats")
library(glmnet)
library(coefplot)
library(lars)
library(VSURF)
library(dplyr)
library(openxlsx)
library(stats)

#Notes: consider removing excess return as an explanatory lagged variable?

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#correctly import data here
load("C:/Users/580155jg/Desktop/CORRECT VRP/DATA no RF for MACRO.RData")
load("C:/Users/580155jg/Desktop/CORRECT VRP/DATA RF for MACRO.RData")

fin_data <- read.xlsx("full_extension_data.xlsx", sheet= 1) %>%
  mutate(excess_return = (ret - Rfree)*100) %>%
  select(-dtoat,-eqis,-skew,-accrul,-cfacc,-csp) #Removal of non-monthly variables

mac_data <- read.xlsx("macvar_data.xlsx", sheet= 1) %>%
  select(-AAA, - BAA)

###############################GET MACRO VARS############################
get_macro_vars <- function(macro_data, window_length, t, y, x, threshold){
  index <- as.numeric(which(macro_data$date == t))
  z_full <- macro_data %>%
    filter(date >= macro_data$date[index-window_length] & date <= macro_data$date[index-1])%>%
    select(-date)
  colnames(x) <- make.names(colnames(x))
  model_data <- cbind(as.data.frame(z_full), x)
  model_data$y_response <- unlist(y)
  model <- lm(model_data$y_response ~ . , data = model_data)
  tvals <- coef(summary(model))[, "t value"]
  tvals <- tvals[names(tvals) != "(Intercept)"]
  
  significant_vars <- names(tvals[abs(tvals) > threshold])
  significant_vars <- setdiff(significant_vars, colnames(x))
  return(significant_vars)
}

##############################PCA###########################################

get_factors <- function(macro_data, window_length, t,z_names, max_factors, max_lags, y, x) {
  index <- as.numeric(which(macro_data$date == t))
  
  z_full <- macro_data %>%
    filter(date >= macro_data$date[index-window_length] & date <= macro_data$date[index])%>%
    select(all_of(z_names))
  if(length(z_names) > 0) {
    pca <- prcomp(z_full, center = TRUE, scale. = TRUE)
  } else {
    valid_idx <- complete.cases(x, y)
    X_model <- as.matrix(x[valid_idx, ])
    y_model <- y$excess_return[valid_idx]
    
    model <- lm(y_model ~ X_model)
    bic <- BIC(model)
    
    return(list(
      model = model,
      best_k = 0,
      best_lags = 0,
      bic = bic,
      factors = matrix(0, nrow = nrow(x), ncol = 0)
    ))
  }

  
  T_obs <- nrow(z_full)
  best_bic <- Inf
  best_model <- NULL
  best_k <- NULL
  best_lags <- NULL
  max_factors <- min(max_factors,length(z_names))
  for (k in 1:max_factors) {
    PC_matrix <- pca$x[, 1:k]
    for (lag_order in 0:max_lags) {
      full_factors <- NULL
      for (lag in 0:lag_order) {
        lagged_PC <- stats::lag(PC_matrix, lag)
        full_factors <- cbind(full_factors, lagged_PC)
      }
      
      rows <- 0:nrow(y)
      rows <- rows[rows > 0]
      
      if (is.null(dim(full_factors))) {
        full_factors <- matrix(full_factors, ncol = 1)
      }
      
      regress_factors <- full_factors[rows, , drop = FALSE]
      valid_idx <- complete.cases(regress_factors, y)
      combined_predictors <- cbind(regress_factors, x)
      X_model <- as.matrix(combined_predictors[valid_idx, ])
      y_model <- y$excess_return[valid_idx]
      
      
      model <- lm(y_model ~ X_model)
      k_bic <- BIC(model)
      
      if (k_bic < best_bic) {
        best_bic <- k_bic
        best_model <- model
        best_k <- k
        best_lags <- lag_order
      }
    }
    
  }
  return(list(
    model = best_model,  
    best_k = best_k,
    best_lags = best_lags,
    bic = best_bic,
    factors = full_factors[1:nrow(y),],
    forecast_factor = full_factors[nrow(y),]
  ))
}  

forecast_with_factors_and_selected_vars <- function(time, fin_data, macro_data,selected_vars, window_length, 
                                                    threshold, max_factors, max_lags) {
  index <- which(fin_data$date == time)
  
  x_reg <- fin_data  %>%
    filter(date >= fin_data$date[index - window_length] & date <= fin_data$date[index - 1]) %>%
    select(all_of(selected_vars))
  
  y_reg <- fin_data %>%
    filter(date >= fin_data$date[index - window_length + 1] & date <= fin_data$date[index]) %>%
    mutate(excess_return = (ret - Rfree) * 100) %>%
    select(excess_return)
  
  z_names <- get_macro_vars(macro_data, window_length, time, y_reg, x_reg, threshold)
  factors_model <- get_factors(macro_data, window_length, time, z_names, max_factors, max_lags, y_reg, x_reg)
  fit <- factors_model$model
  beta <- fit$coefficients
  factors_used <- factors_model$best_k
  lags_used <- factors_model$best_lags
  
  x_forecast <- fin_data %>%
    mutate(excess_return = (ret - Rfree) * 100) %>%
    filter(date == fin_data$date[index]) %>%
    select(all_of(selected_vars)) %>%
    as.numeric()
  
  if (factors_used == 0) {
    valid <- !is.na(beta[-1])
    forecast <- beta[1] + sum(beta[-1][valid] * x_forecast[valid])
  } else {
    selected_cols <- unlist(lapply(0:lags_used, function(lag) {
      start_idx <- lag * factors_used + 1
      end_idx <- (lag + 1) * factors_used
      start_idx:end_idx
    }))
    
    n_fin <- length(x_forecast)
    n_factors <- length(beta) - 1 - n_fin
    
    beta_factors <- beta[2:(1 + n_factors)]
    beta_fin   <- beta[(2 + n_factors):length(beta)]
    
    valid_factors <- !is.na(beta_factors)
    valid_fin   <- !is.na(beta_fin)

    factors_forecast <- factors_model$forecast_factor[selected_cols]
    
    forecast <- beta[1] +
      sum(beta_factors[valid_factors] * factors_forecast[valid_factors]) +
      sum(beta_fin[valid_fin] * x_forecast[valid_fin])
  }
  
  actual <- fin_data %>%
    filter(date == fin_data$date[index + 1]) %>%
    mutate(excess_return = (ret - Rfree) * 100) %>%
    pull(excess_return)
  
  return(list(
    forecast = forecast,
    actual = actual,
    date = time,
    beta = beta,
    selected_vars = selected_vars,
    factors_used = factors_used,
    lags_used = lags_used,
    z_names = z_names
  ))
}


########################################Forecasting regressions#########################################################


hard_threshold_regression <- function(selected_set, fin_data, window_length, start,end, nof_variables_selected, selection_method, lambda_grid, LASSOtype, threshold, mac_data, k_search) {
    dates <- selected_set$plot_forecasts$date
    SE <- 0
    plot_forecasts <- data.frame(
      date = numeric(),  
      actuals = numeric(),       
      forecasts = numeric(),
      factors = numeric(),
      macro_vars = list()
    )
    for(time in dates[1:length(dates)]) {
      selected_vars <- selected_set$selected_vars[[which(selected_set$plot_forecasts$date == time)]]
      forecast_list <- forecast_with_factors_and_selected_vars(time, fin_data, mac_data,selected_vars, window_length,threshold, max_factors = 6, max_lags = 2)
      if(length(forecast_list$z_names) != 0){
        plot_forecasts <- rbind(plot_forecasts, data.frame(
          date = time,
          actuals = forecast_list$actual,
          forecasts = forecast_list$forecast,
          factors = forecast_list$factors_used,
          macro_vars = I(list(forecast_list$z_names))
        ))
      } else {
        plot_forecasts <- rbind(plot_forecasts, data.frame(
          date = time,
          actuals = forecast_list$actual,
          forecasts = forecast_list$forecast,
          factors = forecast_list$factors_used,
          macro_vars = 0
        ))
      }
      
      SE <- SE + (forecast_list$actual - forecast_list$forecast)^2
      print(paste0("Date: ", time,
                   " | Forecast: ", round(forecast_list$forecast, 4),
                   " | Actual: ", round(forecast_list$actual, 4),
                   " | Cumulative SE: ", round(SE, 4)))
      
      if (time == end) {
        return(list(plot_forecasts = plot_forecasts, SE = SE))
      }
    }
}



#multi period

# CORR
Corr_1_104_with_macro <- hard_threshold_regression(selected_set = mult_period_corr_1, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
Corr_3_104_with_macro <- hard_threshold_regression(selected_set = mult_period_corr_3, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
Corr_5_104_with_macro <- hard_threshold_regression(selected_set = mult_period_corr_5, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
Corr_10_104_with_macro <- hard_threshold_regression(selected_set = mult_period_corr_10, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
Corr_15_104_with_macro <- hard_threshold_regression(selected_set = mult_period_corr_15, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
Corr_20_104_with_macro <- hard_threshold_regression(selected_set = mult_period_corr_20, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
Corr_val_104_with_macro <- hard_threshold_regression(selected_set = mult_period_corr_val,
                                                      fin_data = fin_data, window_length = 120,
                                                     start = 199001, end = 202111,
                                                     threshold = 1.04, mac_data = mac_data)
# LARS
LARS_1_104_with_macro <- hard_threshold_regression(selected_set = mult_period_lars_1, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
LARS_3_104_with_macro <- hard_threshold_regression(selected_set = mult_period_lars_3, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
LARS_5_104_with_macro <- hard_threshold_regression(selected_set = mult_period_lars_5, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
LARS_10_104_with_macro <- hard_threshold_regression(selected_set = mult_period_lars_10, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
LARS_15_104_with_macro <- hard_threshold_regression(selected_set = mult_period_lars_15, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
LARS_20_104_with_macro <- hard_threshold_regression(selected_set = mult_period_lars_20, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
LARS_val_104_with_macro <- hard_threshold_regression(selected_set = mult_period_lars_val,
                                                     fin_data = fin_data, window_length = 120,
                                                     start = 199001, end = 202111,
                                                     threshold = 1.04, mac_data = mac_data)

# LASSO Good
LASSO_good_1_104_with_macro <- hard_threshold_regression(selected_set = mult_period_lasso_good_1, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
LASSO_good_3_104_with_macro <- hard_threshold_regression(selected_set = mult_period_lasso_good_3, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
LASSO_good_5_104_with_macro <- hard_threshold_regression(selected_set = mult_period_lasso_good_5, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
LASSO_good_10_104_with_macro <- hard_threshold_regression(selected_set = mult_period_lasso_good_10, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
LASSO_good_15_104_with_macro <- hard_threshold_regression(selected_set = mult_period_lasso_good_15, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
LASSO_good_20_104_with_macro <- hard_threshold_regression(selected_set = mult_period_lasso_good_20, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
LASSO_good_val_104_with_macro <- hard_threshold_regression(selected_set = mult_period_lasso_good_val,
                                                           fin_data = fin_data, window_length = 120,
                                                           start = 199001, end = 202111,
                                                           threshold = 1.04, mac_data = mac_data)
# RF
RF_1_104_with_macro <- hard_threshold_regression(selected_set = mult_period_rf_1, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
RF_3_104_with_macro <- hard_threshold_regression(selected_set = mult_period_rf_3, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
RF_5_104_with_macro <- hard_threshold_regression(selected_set = mult_period_rf_5, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
RF_10_104_with_macro <- hard_threshold_regression(selected_set = mult_period_rf_10, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
RF_15_104_with_macro <- hard_threshold_regression(selected_set = mult_period_rf_15, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
RF_20_104_with_macro <- hard_threshold_regression(selected_set = mult_period_rf_20, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
RF_val_104_with_macro <- hard_threshold_regression(selected_set = mult_period_rf_val,
                                                   fin_data = fin_data, window_length = 120,
                                                   start = 199001, end = 202111,
                                                   threshold = 1.04, mac_data = mac_data)

# CORR
Corr_1_128_with_macro <- hard_threshold_regression(selected_set = mult_period_corr_1, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
Corr_3_128_with_macro <- hard_threshold_regression(selected_set = mult_period_corr_3, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
Corr_5_128_with_macro <- hard_threshold_regression(selected_set = mult_period_corr_5, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
Corr_10_128_with_macro <- hard_threshold_regression(selected_set = mult_period_corr_10, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
Corr_15_128_with_macro <- hard_threshold_regression(selected_set = mult_period_corr_15, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
Corr_20_128_with_macro <- hard_threshold_regression(selected_set = mult_period_corr_20, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
Corr_val_128_with_macro <- hard_threshold_regression(selected_set = mult_period_corr_val,
                                                     fin_data = fin_data, window_length = 120,
                                                     start = 199001, end = 202111,
                                                     threshold = 1.28, mac_data = mac_data)

# LARS
LARS_1_128_with_macro <- hard_threshold_regression(selected_set = mult_period_lars_1, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
LARS_3_128_with_macro <- hard_threshold_regression(selected_set = mult_period_lars_3, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
LARS_5_128_with_macro <- hard_threshold_regression(selected_set = mult_period_lars_5, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
LARS_10_128_with_macro <- hard_threshold_regression(selected_set = mult_period_lars_10, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
LARS_15_128_with_macro <- hard_threshold_regression(selected_set = mult_period_lars_15, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
LARS_20_128_with_macro <- hard_threshold_regression(selected_set = mult_period_lars_20, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
LARS_val_128_with_macro <- hard_threshold_regression(selected_set = mult_period_lars_val,
                                                     fin_data = fin_data, window_length = 120,
                                                     start = 199001, end = 202111,
                                                     threshold = 1.28, mac_data = mac_data)
# LASSO Good
LASSO_good_1_128_with_macro <- hard_threshold_regression(selected_set = mult_period_lasso_good_1, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
LASSO_good_3_128_with_macro <- hard_threshold_regression(selected_set = mult_period_lasso_good_3, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
LASSO_good_5_128_with_macro <- hard_threshold_regression(selected_set = mult_period_lasso_good_5, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
LASSO_good_10_128_with_macro <- hard_threshold_regression(selected_set = mult_period_lasso_good_10, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
LASSO_good_15_128_with_macro <- hard_threshold_regression(selected_set = mult_period_lasso_good_15, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
LASSO_good_20_128_with_macro <- hard_threshold_regression(selected_set = mult_period_lasso_good_20, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
LASSO_good_val_128_with_macro <- hard_threshold_regression(selected_set = mult_period_lasso_good_val,
                                                           fin_data = fin_data, window_length = 120,
                                                           start = 199001, end = 202111,
                                                           threshold = 1.28, mac_data = mac_data)
# RF
RF_1_128_with_macro <- hard_threshold_regression(selected_set = mult_period_rf_1, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
RF_3_128_with_macro <- hard_threshold_regression(selected_set = mult_period_rf_3, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
RF_5_128_with_macro <- hard_threshold_regression(selected_set = mult_period_rf_5, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
RF_10_128_with_macro <- hard_threshold_regression(selected_set = mult_period_rf_10, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
RF_15_128_with_macro <- hard_threshold_regression(selected_set = mult_period_rf_15, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
RF_20_128_with_macro <- hard_threshold_regression(selected_set = mult_period_rf_20, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
RF_val_128_with_macro <- hard_threshold_regression(selected_set = mult_period_rf_val,
                                                   fin_data = fin_data, window_length = 120,
                                                   start = 199001, end = 202111,
                                                   threshold = 1.28, mac_data = mac_data)














#This is the bad way of selecting each variable in each period

Corr_1_128_with_macro <- hard_threshold_regression(selected_set = SE_corr_1, fin_data =fin_data, window_length = 120, start = 199001,end = 202111,threshold = 1.28, mac_data = mac_data)
Corr_3_128_with_macro <- hard_threshold_regression(selected_set = SE_corr_3, fin_data =fin_data, window_length = 120, start = 199001,end = 202111,threshold = 1.28, mac_data = mac_data)
Corr_5_128_with_macro <- hard_threshold_regression(selected_set = SE_corr_5, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
Corr_10_128_with_macro <- hard_threshold_regression(selected_set = SE_corr_10, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
Corr_15_128_with_macro <- hard_threshold_regression(selected_set = SE_corr_15, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
Corr_20_128_with_macro <- hard_threshold_regression(selected_set = SE_corr_20, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
Corr_val_128_with_macro <- hard_threshold_regression(selected_set = SE_corr_val, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)

LARS_1_128_with_macro <- hard_threshold_regression(selected_set = SE_LARS_1, fin_data =fin_data, window_length = 120, start = 199001,end = 202111,threshold = 1.28, mac_data = mac_data)
LARS_3_128_with_macro <- hard_threshold_regression(selected_set = SE_LARS_3, fin_data =fin_data, window_length = 120, start = 199001,end = 202111,threshold = 1.28, mac_data = mac_data)
LARS_5_128_with_macro <- hard_threshold_regression(selected_set = SE_LARS_5, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
LARS_10_128_with_macro <- hard_threshold_regression(selected_set = SE_LARS_10, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
LARS_15_128_with_macro <- hard_threshold_regression(selected_set = SE_LARS_15, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
LARS_20_128_with_macro <- hard_threshold_regression(selected_set = SE_LARS_20, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
LARS_val_128_with_macro <- hard_threshold_regression(selected_set = SE_LARS_val, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)

LASSO_good_1_128_with_macro <- hard_threshold_regression(selected_set = SE_LASSO_good_1, fin_data =fin_data, window_length = 120, start = 199001,end = 202111,threshold = 1.28, mac_data = mac_data)
LASSO_good_3_128_with_macro <- hard_threshold_regression(selected_set = SE_LASSO_good_3, fin_data =fin_data, window_length = 120, start = 199001,end = 202111,threshold = 1.28, mac_data = mac_data)
LASSO_good_5_128_with_macro <- hard_threshold_regression(selected_set = SE_LASSO_good_5, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
LASSO_good_10_128_with_macro <- hard_threshold_regression(selected_set = SE_LASSO_good_10, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
LASSO_good_15_128_with_macro <- hard_threshold_regression(selected_set = SE_LASSO_good_15, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
LASSO_good_20_128_with_macro <- hard_threshold_regression(selected_set = SE_LASSO_good_20, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
LASSO_good_val_128_with_macro <- hard_threshold_regression(selected_set = SE_LASSO_val, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)

RF_1_128_with_macro <- hard_threshold_regression(selected_set = SE_RF_1, fin_data =fin_data, window_length = 120, start = 199001,end = 202111,threshold = 1.28, mac_data = mac_data)
RF_3_128_with_macro <- hard_threshold_regression(selected_set = SE_RF_3, fin_data =fin_data, window_length = 120, start = 199001,end = 202111,threshold = 1.28, mac_data = mac_data)
RF_5_128_with_macro <- hard_threshold_regression(selected_set = SE_RF_5, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
RF_10_128_with_macro <- hard_threshold_regression(selected_set = SE_RF_10, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
RF_15_128_with_macro <- hard_threshold_regression(selected_set = SE_RF_15, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
RF_20_128_with_macro <- hard_threshold_regression(selected_set = SE_RF_20, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)
RF_val_128_with_macro <- hard_threshold_regression(selected_set = SE_RF_val, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.28, mac_data = mac_data)


Corr_1_104_with_macro <- hard_threshold_regression(selected_set = SE_corr_1, fin_data =fin_data, window_length = 120, start = 199001,end = 202111,threshold = 1.04, mac_data = mac_data)
Corr_3_104_with_macro <- hard_threshold_regression(selected_set = SE_corr_3, fin_data =fin_data, window_length = 120, start = 199001,end = 202111,threshold = 1.04, mac_data = mac_data)
Corr_5_104_with_macro <- hard_threshold_regression(selected_set = SE_corr_5, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
Corr_10_104_with_macro <- hard_threshold_regression(selected_set = SE_corr_10, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
Corr_15_104_with_macro <- hard_threshold_regression(selected_set = SE_corr_15, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
Corr_20_104_with_macro <- hard_threshold_regression(selected_set = SE_corr_20, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
Corr_val_104_with_macro <- hard_threshold_regression(selected_set = SE_corr_val, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)

LARS_1_104_with_macro <- hard_threshold_regression(selected_set = SE_LARS_1, fin_data =fin_data, window_length = 120, start = 199001,end = 202111,threshold = 1.04, mac_data = mac_data)
LARS_3_104_with_macro <- hard_threshold_regression(selected_set = SE_LARS_3, fin_data =fin_data, window_length = 120, start = 199001,end = 202111,threshold = 1.04, mac_data = mac_data)
LARS_5_104_with_macro <- hard_threshold_regression(selected_set = SE_LARS_5, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
LARS_10_104_with_macro <- hard_threshold_regression(selected_set = SE_LARS_10, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
LARS_15_104_with_macro <- hard_threshold_regression(selected_set = SE_LARS_15, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
LARS_20_104_with_macro <- hard_threshold_regression(selected_set = SE_LARS_20, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
LARS_val_104_with_macro <- hard_threshold_regression(selected_set = SE_LARS_val, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)

LASSO_good_1_104_with_macro <- hard_threshold_regression(selected_set = SE_LASSO_good_1, fin_data =fin_data, window_length = 120, start = 199001,end = 202111,threshold = 1.04, mac_data = mac_data)
LASSO_good_3_104_with_macro <- hard_threshold_regression(selected_set = SE_LASSO_good_3, fin_data =fin_data, window_length = 120, start = 199001,end = 202111,threshold = 1.04, mac_data = mac_data)
LASSO_good_5_104_with_macro <- hard_threshold_regression(selected_set = SE_LASSO_good_5, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
LASSO_good_10_104_with_macro <- hard_threshold_regression(selected_set = SE_LASSO_good_10, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
LASSO_good_15_104_with_macro <- hard_threshold_regression(selected_set = SE_LASSO_good_15, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
LASSO_good_20_104_with_macro <- hard_threshold_regression(selected_set = SE_LASSO_good_20, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
LASSO_good_val_104_with_macro <- hard_threshold_regression(selected_set = SE_LASSO_val, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)


RF_1_104_with_macro <- hard_threshold_regression(selected_set = SE_RF_1, fin_data =fin_data, window_length = 120, start = 199001,end = 202111,threshold = 1.04, mac_data = mac_data)
RF_3_104_with_macro <- hard_threshold_regression(selected_set = SE_RF_3, fin_data =fin_data, window_length = 120, start = 199001,end = 202111,threshold = 1.04, mac_data = mac_data)
RF_5_104_with_macro <- hard_threshold_regression(selected_set = SE_RF_5, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
RF_10_104_with_macro <- hard_threshold_regression(selected_set = SE_RF_10, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
RF_15_104_with_macro <- hard_threshold_regression(selected_set = SE_RF_15, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
RF_20_104_with_macro <- hard_threshold_regression(selected_set = SE_RF_20, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)
RF_val_104_with_macro <- hard_threshold_regression(selected_set = SE_RF_val, fin_data = fin_data, window_length = 120, start = 199001, end = 202111, threshold = 1.04, mac_data = mac_data)



