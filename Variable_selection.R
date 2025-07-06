install.packages("glmnet")
install.packages("coefplot")
install.packages("lars")
install.packages("VSURF")
install.packages("dplyr")
install.packages("openxlsx")
install.packages("tidyr")
install.packages("car")
library(glmnet)
library(coefplot)
library(lars)
library(VSURF)
library(dplyr)
library(openxlsx)
library(tidyr)
library(car)

set.seed(123)

#Notes: consider removing excess return as an explanatory lagged variable?

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

full_extension_data <- read.xlsx("full_extension_data.xlsx", sheet= 1) %>%
  select(-dtoat,-eqis,-skew,-accrul,-cfacc,-price,-csp) #Removal of non-monthly variables
#Consider including the macroeconomic variables excluded in this part

#Function for variable selection with correlation
#Consider whether it should be t-1 or t itself
variable_selection_correlation <- function(data, window_length, t, number_of_selected) {
  index <- as.numeric(which(data$date == t))
  
  X_window <- data %>%
    filter(date >= as.numeric(data$date[index - window_length]) &
             date <= as.numeric(data$date[index-1])) %>%
    mutate(excess_return = (ret - Rfree)*100) %>%
    select(-date, -infl, -cay,-'i/k',-pce,-govik
           ,-crdstd,-ogap,-wtexas,-ndrbl,-gpce,-gip,-house) %>%
    select(where(~ !any(is.na(.))))
  
  
  y_window <- data %>%
    filter(date >= as.numeric(data$date[index - window_length+1]) &
             date <= as.numeric(data$date[index])) %>%
    mutate(excess_return = (ret - Rfree)*100) %>%
    pull(excess_return)
  
  corrs <- sapply(X_window, function(x) cor(x, y_window, use = "pairwise.complete.obs"))
  sorted_corrs <- sort(abs(corrs), decreasing = TRUE)
  var_names <- names(sorted_corrs)[1:number_of_selected]
  corrs_selected <- cor(X_window[var_names], use = "pairwise.complete.obs")
  
  if(length(corrs_selected) > 1) {
    diag(corrs_selected) <- NA
    i <- 1
    while (sort(abs(corrs_selected), decreasing = TRUE)[1] > 0.9) {
      which_max <- which(abs(corrs_selected) == max(abs(corrs_selected), na.rm = TRUE), arr.ind = TRUE)
      var1 <- rownames(corrs_selected)[which_max[1, "row"]]
      var2 <- colnames(corrs_selected)[which_max[1, "col"]]
      
      if (abs(corrs[var1]) < abs(corrs[var2])) {
        var_to_remove <- var1
      } else {
        var_to_remove <- var2
      }
      var_names <- setdiff(var_names, var_to_remove)
      if(i + number_of_selected < length(sorted_corrs)) {
        var_names <- c(var_names, names(sorted_corrs)[i+number_of_selected])
        corrs_selected <- cor(X_window[var_names], use = "pairwise.complete.obs")
        diag(corrs_selected) <- NA
        i <- i+1
      } else {
        corrs_selected <- cor(X_window[var_names], use = "pairwise.complete.obs")
        diag(corrs_selected) <- NA
        while (sort(abs(corrs_selected), decreasing = TRUE)[1] > 0.9) {
          which_max <- which(abs(corrs_selected) == max(abs(corrs_selected), na.rm = TRUE), arr.ind = TRUE)
          var1 <- rownames(corrs_selected)[which_max[1, "row"]]
          var2 <- colnames(corrs_selected)[which_max[1, "col"]]
          
          if (abs(corrs[var1]) < abs(corrs[var2])) {
            var_to_remove <- var1
          } else {
            var_to_remove <- var2
          }
          var_names <- setdiff(var_names, var_to_remove)
          corrs_selected <- cor(X_window[var_names], use = "pairwise.complete.obs")
          diag(corrs_selected) <- NA
        }
      }
    } 
  }
  selected_vars <- var_names
  return(selected_vars)
}

#Function for variable selection with LASSO
variable_selection_LASSO <- function(data, window_length, t, number_of_selected, lambda_grid, LASSO_type) {
  index <- as.numeric(which(data$date == t))
  
  X_window <- data %>%
    filter(date >= as.numeric(data$date[index - window_length]) &
             date <= as.numeric(data$date[index - 1])) %>%
    mutate(excess_return = (ret - Rfree)*100) %>%
    select(-date, -infl, -cay,-'i/k',-pce,-govik
           ,-crdstd,-ogap,-wtexas,-ndrbl,-gpce,-gip,-house) %>%
    select(where(~ !any(is.na(.))))
  
  
  y_window <- data %>%
    filter(date >= as.numeric(data$date[index - window_length]+1) &
             date <= as.numeric(data$date[index])) %>%
    mutate(excess_return = (ret - Rfree)*100) %>%
    pull(excess_return)
  
  if (LASSO_type == "first-k") {
    for (lambda_val in lambda_grid) {
      model <- glmnet(X_window, y_window, alpha = 1, lambda = lambda_val, standardize = TRUE)
      coefs <- coef(model, s = lambda_val)
      selected <- coefs@i[-1]
      names_selected <- coefs@Dimnames[[1]][selected + 1]
      
      if (length(selected) == number_of_selected) {
        if(lambda_val == min(lambda_grid)) {
          return("#################STOP####################")
        }
        return(names_selected)
      } else if (length(selected) > number_of_selected) {
        low <- lambda_val
        indx_high <- which(lambda_val == lambda_grid)
        high <- lambda_grid[indx_high-1]
        new_lambda_grid <- seq(high,low,length=1000)
        for (lambda_val in new_lambda_grid) {
          model <- glmnet(X_window, y_window, alpha = 1, lambda = lambda_val, standardize = TRUE)
          coefs <- coef(model, s = lambda_val)
          selected <- coefs@i[-1]
          names_selected <- coefs@Dimnames[[1]][selected + 1]
          if (length(selected) == number_of_selected) {
            return(names_selected)
          }
          
        }
      }
    }
  }
  if (LASSO_type == "CV-FULL") {
    X_window <- as.matrix(X_window)
    n_obs <- nrow(X_window)
    nfolds <- min(5, floor(n_obs / 5))
    iter <- 0
    max_iter <- 1000
    names_selected <- list()
    while (length(names_selected) == 0 && iter < max_iter) {
      model <- cv.glmnet(X_window, y_window, alpha = 1, lambda = lambda_grid, standardize = TRUE, nfolds = nfolds)
      lambda_val <- model$lambda.min
      coefs <- coef(model, s = lambda_val)
      selected <- coefs@i[-1]
      names_selected <- coefs@Dimnames[[1]][selected + 1]
      iter <- iter + 1
    }
    return(names_selected)
  }
  
  if (LASSO_type == "CV-k") {
    X_window <- as.matrix(X_window)
    n_obs <- nrow(X_window)
    nfolds <- min(5, floor(n_obs / 5))
    model <- cv.glmnet(X_window, y_window, alpha = 1, lambda = lambda_grid, standardize = TRUE, nfolds = nfolds)
    lambda_val <- model$lambda.min
    coefs <- coef(model, s = lambda_val)
    selected <- coefs@i[-1]
    names_selected <- coefs@Dimnames[[1]][selected + 1]
    iter <- 0
    max_iter <- 498
    lambda_grid_1 <- lambda_grid
    lambda_grid_2 <- lambda_grid
    while (length(names_selected) != number_of_selected && iter < max_iter) {
      lambda_grid_1 <- lambda_grid_1[lambda_grid_1 != lambda_val]
      
      model <- cv.glmnet(X_window, y_window, alpha = 1, lambda = lambda_grid_1, standardize = TRUE, nfolds = nfolds)
      lambda_val <- model$lambda.min
      coefs <- coef(model, s = lambda_val)
      selected <- coefs@i[-1]
      names_selected <- coefs@Dimnames[[1]][selected + 1]
      iter<-iter+1
    }
    if(length(names_selected) != number_of_selected) {
      iter <- 0
      max_iter <- 1000
      while (length(names_selected) != number_of_selected && iter < max_iter) {
        lambda_grid_2 <- lambda_grid_2[lambda_grid_2 != lambda_val]
        
        model <- cv.glmnet(X_window, y_window, alpha = 1, lambda = lambda_grid_2, standardize = TRUE, nfolds = nfolds)
        lambda_val <- model$lambda.min
        coefs <- coef(model, s = lambda_val)
        selected <- coefs@i[-1]
        names_selected <- coefs@Dimnames[[1]][selected + 1]
        if(length(names_selected) > number_of_selected) {
          while(length(names_selected) != number_of_selected) {
            nonzero_indices <- which(coefs != 0)
            nonzero_indices <- nonzero_indices[-1]
            coefs_nz <- coefs[nonzero_indices]
            
            top_k <- order(abs(coefs_nz), decreasing = TRUE)[1:number_of_selected]
            names_selected <- names_selected[top_k]
          }
        }
        iter<-iter+1
      }
    }
    return(names_selected)
  }
  return("VALUE NOT FOUND IN THIS GRID AND WINDOW")
}

########################################LARS########################################################################
variable_selection_LARS <- function(data, window_length, t, number_of_selected) {
  index <- as.numeric(which(data$date == t))
  
  X_window <- data %>%
    filter(date >= as.numeric(data$date[index - window_length]) &
             date <= as.numeric(data$date[index - 1])) %>%
    mutate(excess_return = (ret - Rfree)*100) %>%
    select(-date, -infl, -cay,-'i/k',-pce,-govik
           ,-crdstd,-ogap,-wtexas,-ndrbl,-gpce,-gip,-house) %>%
    select(where(~ !any(is.na(.))))
  X_window <- as.matrix(X_window)
  
  y_window <- data %>%
    filter(date >= as.numeric(data$date[index - window_length]+1) &
             date <= as.numeric(data$date[index])) %>%
    mutate(excess_return = (ret - Rfree)*100) %>%
    pull(excess_return)
  
  lars_model <- lars(X_window, y_window, type = "lar", normalize = TRUE, intercept = TRUE)
  if (number_of_selected > nrow(lars_model$beta)-1) {
    return(NULL)
  }
  if(number_of_selected > 0) {
    coefs_k <- coef(lars_model, s = number_of_selected, mode = "step")
    selected_vars <- names(coefs_k[coefs_k != 0])
    if(length(selected_vars) != number_of_selected && length(selected_vars) < nrow(lars_model$beta)-1) {
      coefs_k <- coef(lars_model, s = number_of_selected+1, mode = "step")
      selected_vars <- names(coefs_k[coefs_k != 0])
    } else {
      return(selected_vars <- NULL)
    }
  } else {
    #We need to lag the data as we cannot choose k which fits the data best for the future because we then have a look ahead bias.
    index <- as.numeric(which(data$date == t))
    error_df <- data.frame(k = integer(), MAE = numeric())
    forecast_store <- list()
    lag_time <- data$date[index-1]
    k_search <- 1:length(X_window[1,])
    for (k in k_search) {
      selected_vars <- variable_selection_LARS(data, window_length, t, k)
      if (!is.null(selected_vars)) {
        forecast_df <- regression_forecast(data, selected_vars, window_length, lag_time)
        
        error <- abs(forecast_df$forecast - forecast_df$excess_return)
        error_df <- rbind(error_df, data.frame(k = k, MAE = error))
        forecast_store[[as.character(k)]] <- forecast_df
      }
    }
    best_row <- error_df[which.min(error_df$MAE), ]
    best_k <- best_row$k
    
    selected_vars <- variable_selection_correlation(data, window_length, t, best_k)
  }
  return(selected_vars)
}

####################################################RF##################################################################
variable_selection_RF <- function(data, window_length, t, number_of_selected) {
  index <- as.numeric(which(data$date == t))
  
  X_window <- data %>%
    filter(date >= as.numeric(data$date[index - window_length]) &
             date <= as.numeric(data$date[index - 1])) %>%
    mutate(excess_return = (ret - Rfree)*100) %>%
    select(-date, -infl, -cay,-'i/k',-pce,-govik
           ,-crdstd,-ogap,-wtexas,-ndrbl,-gpce,-gip,-house) %>%
    select(where(~ !any(is.na(.))))
  
  
  y_window <- data %>%
    filter(date >= as.numeric(data$date[index - window_length]+1) &
             date <= as.numeric(data$date[index])) %>%
    mutate(excess_return = (ret - Rfree)*100) %>%
    pull(excess_return)
  
  vsurf_result <- VSURF(X_window, y_window)
  
  interpretation <- vsurf_result$varselect.interp 
  prediction <- vsurf_result$varselect.pred 
  
  if(number_of_selected < 0) {
    vars <- colnames(X_window)[prediction]
    return(vars)
  } else {
    importance_scores <- vsurf_result$imp.mean.dec.ind
    top_k_vars <- order(importance_scores,decreasing = TRUE)[1:number_of_selected]
    vars <- colnames(X_window)[top_k_vars]
    return(vars)
  }
}

get_var_importance <- function(data, window_length, start, end) {
  var_list <- data.frame(
    dates = numeric(),
    vars_selected = I(list())
  )
  for (t in data$date[which(data$date == start):which(data$date == end)]) {
    index <- as.numeric(which(data$date == t))
    
    X_window <- data %>%
      filter(date >= as.numeric(data$date[index - window_length]) &
               date <= as.numeric(data$date[index - 1])) %>%
      mutate(excess_return = (ret - Rfree)*100) %>%
      select(-date, -infl, -cay,-'i/k',-pce,-govik
             ,-crdstd,-ogap,-wtexas,-ndrbl,-gpce,-gip,-house) %>%
      select(where(~ !any(is.na(.))))
    
    
    y_window <- data %>%
      filter(date >= as.numeric(data$date[index - window_length]+1) &
               date <= as.numeric(data$date[index])) %>%
      mutate(excess_return = (ret - Rfree)*100) %>%
      pull(excess_return)
    
    vsurf_result <- VSURF(X_window, y_window)
    
    importance_scores <- vsurf_result$imp.mean.dec.ind
    sorted_vars <- order(importance_scores,decreasing = TRUE)
    
    vars <- colnames(X_window)[sorted_vars]
    new_row <- data.frame(
      dates = t,
      vars_selected = I(list(vars))
    )
    var_list <- rbind(var_list, new_row)
  }
  return(var_list)
}
####################################Rolling window regression forecasting###################################################
regression_forecast <- function(data, selected_vars, window_length, time) {
  index <- which(as.numeric(data$date) == time)
  x_reg <- data %>%
    mutate(excess_return = (ret - Rfree)*100)%>%
    filter(date >= data$date[index-window_length] & date <= data$date[index-1]) %>%
    select(all_of(selected_vars))
  
  x_forecast <- data %>%
    mutate(excess_return = (ret - Rfree)*100)%>%
    filter(date == data$date[index]) %>%
    select(all_of(selected_vars))
  
  y_reg <- data %>%
    filter(date >= data$date[index-window_length+1] & date <= data$date[index]) %>%
    mutate(excess_return = (ret - Rfree)*100) %>%
    select(excess_return)
  
  x_reg <- x_reg[, colSums(is.na(x_reg)) == 0]
  if (length(x_reg)==0) {
    actual <- full_extension_data  %>%
      mutate(excess_return = (ret - Rfree)*100) %>%
      filter(date == data$date[index+1]) %>%
      select(excess_return)
    return(final <- data.frame(
      actual = actual,
      forecast = Inf
    ))
  }
  model <- lm(y_reg$excess_return ~ as.matrix(x_reg))
  beta <- coef(model) 
  
  #Added for LASSO method as too many observations for the LASSO model
  beta[is.na(beta)] <- 0
  forecast <- beta[1] + sum(beta[-1] * x_forecast)
  actual <- data  %>%
    mutate(excess_return = (ret - Rfree)*100) %>%
    filter(date == data$date[index+1]) %>%
    select(excess_return)
  final <- data.frame(
    actual = actual,
    forecast = forecast
  )
  return(final)
}

#Hier nog data voor cross validation veranderen want hij weet anders van tevoren al hoeveel variabelen het beste zijn toch?

top_k_correlation <- function(data, window_length, time, k_search) {
  error_df <- data.frame(k = integer(), MAE = numeric())
  forecast_store <- list()
  
  #We need to lag the data as we cannot choose k which fits the data best for the future because we then have a look ahead bias.
  lag_time <- data$date[as.numeric(which(data$date == time))-1]
  
  for (k in 1:k_search) {
    selected_vars <- variable_selection_correlation(data, window_length, time, k)
    forecast_df <- regression_forecast(data, selected_vars, window_length, lag_time)
    
    error <- abs(forecast_df$forecast - forecast_df$excess_return)
    error_df <- rbind(error_df, data.frame(k = k, MAE = error))
    forecast_store[[as.character(k)]] <- forecast_df
  }
  
  best_row <- error_df[which.min(error_df$MAE), ]
  best_k <- best_row$k
  
  
  vars_using_best_k <- variable_selection_correlation(data, window_length, time, best_k)
  forecast <- regression_forecast(data, vars_using_best_k, window_length, time)
  
  return(list(
    best_k = best_k,
    best_mae = best_row$MAE,
    all_errors = error_df,
    best_forecast = forecast,
    vars_selected = vars_using_best_k
  ))
}

get_hist_average <- function(data,window_length,start,end){
  selected_vars_list <- list()
  dates <- data %>%
    filter(date >= start & date <= end) %>%
    select(date)
  SE <- 0
  plot_forecasts <- data.frame(
    date = numeric(),  
    actuals = numeric(),       
    forecasts = numeric()
  )
  for(time in dates$date[1:length(dates$date)]) {
    index <- which(data$date == time)
    y <- data %>% 
      mutate(excess_return = (ret - Rfree)*100) %>%
      filter(date >= data$date[index - window_length] & date <= data$date[index - 1]) %>%
      pull(excess_return)
    forecast <- mean(y,na.rm = TRUE)
    
    actual <- data %>% 
      mutate(excess_return = (ret - Rfree)*100) %>%
      filter(date == data$date[index]) %>%
      pull(excess_return)
    
    SE <- SE + (actual - forecast)^2
    plot_forecasts <- rbind(plot_forecasts, data.frame(
      date = time,
      actuals = actual,
      forecasts = forecast
    ))
    print(paste0("Date: ", time,
                 " | Forecast: ", round(forecast, 4),
                 " | Actual: ", round(actual, 4),
                 " | Cumulative SE: ", round(SE, 4)))
    if (time == end) {
      return(list(plot_forecasts = plot_forecasts, SE = SE, selected_vars = selected_vars_list))
    }
  }
}

########################################Forecasting regressions#########################################################

finvar_regression <- function(data, window_length, start,end, nof_variables_selected, selection_method, lambda_grid, LASSOtype, k_search) {
  if(selection_method == "Correlation") {
    total_vars_selected <- 0
    iteration <- 0
    selected_vars_list <- list()
    dates <- data %>%
      filter(date >= start & date <= end) %>%
      select(date)
    SE <- 0
    plot_forecasts <- data.frame(
      date = numeric(),  
      actuals = numeric(),       
      forecasts = numeric()
    )
    for(time in dates$date[1:length(dates$date)]) {
      selected_vars <- variable_selection_correlation(data, window_length, time, nof_variables_selected)
      total_vars_selected <- total_vars_selected + length(selected_vars)
      iteration <- iteration + 1
      selected_vars_list[[as.character(time)]] <- selected_vars
      result <- regression_forecast(data, selected_vars, window_length,time)
      
      SE <- SE + (result$excess_return - result$forecast)^2
      plot_forecasts <- rbind(plot_forecasts, data.frame(
        date = time,
        actuals = result$excess_return,
        forecasts = result$forecast
      ))
      print(paste0("Date: ", time,
                   " | Forecast: ", round(result$forecast, 4),
                   " | Actual: ", round(result$excess_return, 4),
                   " | Cumulative SE: ", round(SE, 4),
                   "| Average Selected: ", total_vars_selected/iteration ))
      if (time == end) {
        return(list(plot_forecasts = plot_forecasts, SE = SE, selected_vars = selected_vars_list))
      }
    }
  }
  
  if(selection_method == "Correlation_validation") {
    total_vars_selected <- 0
    iteration <- 0
    selected_vars_list <- list()
    dates <- data %>%
      filter(date >= start & date <= end) %>%
      select(date)
    SE <- 0
    plot_forecasts <- data.frame(
      date = numeric(),  
      actuals = numeric(),       
      forecasts = numeric()
    )
    for(time in dates$date[1:length(dates$date)]) {
      corr_valid <- top_k_correlation(data, window_length, time, k_search)
      total_vars_selected <-total_vars_selected + corr_valid$best_k
      iteration <- iteration + 1
      selected_vars_list[[as.character(time)]] <- corr_valid$vars_selected
      best_list <- corr_valid$best_forecast
      SE <- SE + (best_list$excess_return - best_list$forecast)^2
      plot_forecasts <- rbind(plot_forecasts, data.frame(
        date = time,
        actuals = best_list$excess_return,
        forecasts = best_list$forecast
      ))
      print(paste0("Date: ", time,
                   " | Forecast: ", round(best_list$forecast, 4),
                   " | Actual: ", round(best_list$excess_return, 4),
                   " | Cumulative SE: ", round(SE, 4),
                   "| Average Selected: ", total_vars_selected/iteration ))
      if (time == end) {
        return(list(plot_forecasts = plot_forecasts, SE = SE, selected_vars = selected_vars_list))
      }
    }
  }
  
  if(selection_method == "LARS") {
    total_vars_selected <- 0
    iteration <- 0
    selected_vars_list <- list()
    dates <- data %>%
      filter(date >= start & date <= end) %>%
      select(date)
    SE <- 0
    plot_forecasts <- data.frame(
      date = numeric(),  
      actuals = numeric(),       
      forecasts = numeric()
    )
    for(time in dates$date[1:length(dates$date)]) {
      selected_vars <- variable_selection_LARS(data, window_length, time, nof_variables_selected)
      total_vars_selected <- total_vars_selected + length(selected_vars)
      iteration <- iteration + 1
      selected_vars_list[[as.character(time)]] <- selected_vars
      selected_vars_list[[as.character(time)]] <- selected_vars
      result <- regression_forecast(data, selected_vars, window_length,time)
      
      SE <- SE + (result$excess_return - result$forecast)^2
      plot_forecasts <- rbind(plot_forecasts, data.frame(
        date = time,
        actuals = result$excess_return,
        forecasts = result$forecast
      ))
      print(paste0("Date: ", time,
                   " | Forecast: ", round(result$forecast, 4),
                   " | Actual: ", round(result$excess_return, 4),
                   " | Cumulative SE: ", round(SE, 4),
                   "| Average Selected: ", total_vars_selected/iteration ))
      if (time == end) {
        return(list(plot_forecasts = plot_forecasts, SE = SE, selected_vars = selected_vars_list))
      }
    }
  }
  
  if(selection_method == "LASSO") {
    total_vars_selected <- 0
    iteration <- 0
    selected_vars_list <- list()
    dates <- data %>%
      filter(date >= start & date <= end) %>%
      select(date)
    SE <- 0
    plot_forecasts <- data.frame(
      date = numeric(),  
      actuals = numeric(),       
      forecasts = numeric()
    )
    for(time in dates$date[1:length(dates$date)]) {
      selected_vars <- variable_selection_LASSO(data, window_length, time, nof_variables_selected, lambda_grid, LASSOtype)
      total_vars_selected <- total_vars_selected + length(selected_vars)
      iteration <- iteration + 1
      selected_vars_list[[as.character(time)]] <- selected_vars
      result <- regression_forecast(data, selected_vars, window_length,time)
      
      SE <- SE + (result$excess_return - result$forecast)^2
      plot_forecasts <- rbind(plot_forecasts, data.frame(
        date = time,
        actuals = result$excess_return,
        forecasts = result$forecast
      ))
      print(paste0("Timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                   " | Date: ", time,
                   " | Forecast: ", round(result$forecast, 4),
                   " | Actual: ", round(result$excess_return, 4),
                   " | Cumulative SE: ", round(SE, 4),
                   "| Average Selected: ", total_vars_selected/iteration))
      if (time == end) {
        return(list(plot_forecasts = plot_forecasts, SE = SE, selected_vars = selected_vars_list))
      }
    }
  }
  
  if(selection_method == "RF") {
    total_vars_selected <- 0
    iteration <- 0
    selected_vars_list <- list()
    dates <- data %>%
      filter(date >= start & date <= end) %>%
      select(date)
    SE <- 0
    plot_forecasts <- data.frame(
      date = numeric(),  
      actuals = numeric(),       
      forecasts = numeric()
    )
    for(time in dates$date[1:length(dates$date)]) {
      selected_vars <- variable_selection_RF(data, window_length, time, nof_variables_selected, lambda_grid, LASSOtype)
      total_vars_selected <- total_vars_selected + length(selected_vars)
      iteration <- iteration + 1
      selected_vars_list[[as.character(time)]] <- selected_vars
      result <- regression_forecast(data, selected_vars, window_length,time)
      
      SE <- SE + (result$excess_return - result$forecast)^2
      plot_forecasts <- rbind(plot_forecasts, data.frame(
        date = time,
        actuals = result$excess_return,
        forecasts = result$forecast
      ))
      print(paste0("Timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                   " | Date: ", time,
                   " | Forecast: ", round(result$forecast, 4),
                   " | Actual: ", round(result$excess_return, 4),
                   " | Cumulative SE: ", round(SE, 4),
                   "| Average Selected: ", total_vars_selected/iteration ))
      if (time == end) {
        return(list(plot_forecasts = plot_forecasts, SE = SE, selected_vars = selected_vars_list))
      }
    }
  }
  
  
  
  if(selection_method == "Ex post") {
    total_vars_selected <- 0
    iteration <- 0
    selected_vars_list <- list()
    dates <- data %>%
      filter(date >= start & date <= end) %>%
      select(date)
    SE <- 0
    plot_forecasts <- data.frame(
      date = numeric(),  
      actuals = numeric(),       
      forecasts = numeric()
    )
    for(time in dates$date[1:length(dates$date)]) {
      min_SE <- Inf
      names_pos_var <- data %>%
        filter(date >= start & date <= end)%>%
        select(-date, -infl, -cay,-'i/k',-pce,-govik
               ,-crdstd,-ogap,-wtexas,-ndrbl,-gpce,-gip,-house) %>%
        select(where(~ !any(is.na(.))))
      
      for(var in colnames(names_pos_var)) {
        result <- regression_forecast(data, var, window_length, time)
        
        SE_t <- (result$excess_return - result$forecast)^2
        if (SE_t < min_SE) {
          selected_vars <- var
          min_SE <- SE_t
        }
      }
      selected_vars_list[[as.character(time)]] <- selected_vars
      result <- regression_forecast(data, selected_vars, window_length,time)
      
      SE <- SE + (result$excess_return - result$forecast)^2
      plot_forecasts <- rbind(plot_forecasts, data.frame(
        date = time,
        actuals = result$excess_return,
        forecasts = result$forecast
      ))
      print(paste0("Timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                   " | Date: ", time,
                   " | Forecast: ", round(result$forecast, 4),
                   " | Actual: ", round(result$excess_return, 4),
                   " | Cumulative SE: ", round(SE, 4),
                   "| Average Selected: ", total_vars_selected/iteration ))
      if (time == end) {
        return(list(plot_forecasts = plot_forecasts, SE = SE, selected_vars = selected_vars_list))
      }
    }
  }
    if(selection_method == "Ex post single var") {
      results_all <- list()
      selected_vars_list_var <- list()
      dates <- data %>%
        filter(date >= start & date <= end) %>%
        select(date)
      SE <- 0
      plot_forecasts_var <- data.frame(
        date = numeric(),  
        actuals = numeric(),       
        forecasts = numeric()
      )
      min_SE <- Inf
      names_pos_var <- data %>%
        filter(date >= start & date <= end)%>%
        select(-date, -infl, -cay,-'i/k',-pce,-govik
               ,-crdstd,-ogap,-wtexas,-ndrbl,-gpce,-gip,-house,-disag) %>%
        select(where(~ !any(is.na(.))))
      
      for(var in colnames(names_pos_var)) {
        SE_var <- 0
        if(var == "vp" | var == "vrp") {
          for(time in dates$date[121:length(dates$date)]) {
            result <- regression_forecast(data, var, window_length, time)
            SE_var  <- SE_var + (result$excess_return - result$forecast)^2
            selected_vars_list_var[[as.character(time)]] <- var
            result <- regression_forecast(data, var, window_length,time)
            
            plot_forecasts_var <- rbind(plot_forecasts_var, data.frame(
              date = time,
              actuals = result$excess_return,
              forecasts = result$forecast
            ))
            print(paste0("Timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                         " | Date: ", time,
                         " | Forecast: ", round(result$forecast, 4),
                         " | Actual: ", round(result$excess_return, 4),
                         " | Cumulative SE: ", round(SE, 4)))
          }
        } else {
        for(time in dates$date[1:length(dates$date)]) {
          result <- regression_forecast(data, var, window_length, time)
          SE_var  <- SE_var + (result$excess_return - result$forecast)^2
          selected_vars_list_var[[as.character(time)]] <- var
          result <- regression_forecast(data, var, window_length,time)
        
          plot_forecasts_var <- rbind(plot_forecasts_var, data.frame(
                                date = time,
                                actuals = result$excess_return,
                                  forecasts = result$forecast
          ))
          print(paste0("Timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                       " | Date: ", time,
                       " | Forecast: ", round(result$forecast, 4),
                       " | Actual: ", round(result$excess_return, 4),
                       " | Cumulative SE: ", round(SE_var, 4)))
          }
        }
        if (SE_var < min_SE) {
          plot_forecasts <- plot_forecasts_var
          selected_vars_list <- selected_vars_list_var
          selected_vars <- var
          min_SE <- SE_var
        }
        results_all[[var]] <- list(
          SE = SE_var,
          plot = plot_forecasts_var,
          selected_vars_list = selected_vars_list_var
        )
      }
      return(list(plot_forecasts = plot_forecasts, SE = min_SE, selected_vars = selected_vars_list, all_results = results_all))
  }
  }

#Example of the different types of input dependent on the selection method which we want to use
#Change for other windows etc. 1 year 3 year 5 year 10 year? No robustness check as we need 100+ observations for the macroeconomic variables reduction

#Benchmarks
#Historical Average
historical_average <- get_hist_average(data = full_extension_data, window_length = 120,start = 199001,end =202111)

ex_post_optimal <- finvar_regression(data = full_extension_data,window_length=120,start = 199001,end =202111,nof_variables_selected = 1,selection_method = "Ex post")

ex_post_optimal_single_var <- finvar_regression(data = full_extension_data,window_length=120,start = 199001,end =202111,nof_variables_selected = 1,selection_method = "Ex post single var")

for (i in names(ex_post_optimal_single_var$all_results)) {
  cat("Variable:", i, " | SE:", ex_post_optimal_single_var$all_results[[i]]$SE, "\n")
  
  print(ex_post_optimal_single_var$all_results[[i]]$SE/length(ex_post_optimal_single_var$all_results[[i]]$selected_vars_list))
}

SE_corr_1 <- finvar_regression(data = full_extension_data,window_length=120,start = 199001,end =202111,nof_variables_selected = 1,selection_method = "Correlation")
SE_corr_3 <- finvar_regression(data = full_extension_data,window_length=120,start = 199001,end =202111,nof_variables_selected = 3,selection_method = "Correlation")
SE_corr_5 <- finvar_regression(data = full_extension_data,window_length=120,start = 199001,end =202111,nof_variables_selected = 5,selection_method = "Correlation")
SE_corr_10 <- finvar_regression(data = full_extension_data,window_length=120,start = 199001,end =202111,nof_variables_selected = 10,selection_method = "Correlation")
SE_corr_15 <- finvar_regression(data = full_extension_data,window_length=120,start = 199001,end =202111,nof_variables_selected = 15,selection_method = "Correlation")
SE_corr_20 <- finvar_regression(data = full_extension_data,window_length=120,start = 199001,end =202111,nof_variables_selected = 20,selection_method = "Correlation")
SE_corr_val <- finvar_regression(data = full_extension_data,window_length=120,start = 199001,end =202111,nof_variables_selected = 10,selection_method = "Correlation_validation", k_search = 30)

SE_LARS_1 <- finvar_regression(data = full_extension_data,window_length=120,start = 199001,end =202111,nof_variables_selected = 1,selection_method = "LARS")
SE_LARS_3 <- finvar_regression(data = full_extension_data,window_length=120,start = 199001,end =202111,nof_variables_selected = 3,selection_method = "LARS")
SE_LARS_5 <- finvar_regression(data = full_extension_data,window_length=120,start = 199001,end =202111,nof_variables_selected = 5,selection_method = "LARS")
SE_LARS_10 <- finvar_regression(data = full_extension_data,window_length=120,start = 199001,end =202111,nof_variables_selected = 10,selection_method = "LARS")
SE_LARS_15 <- finvar_regression(data = full_extension_data,window_length=120,start = 199001,end =202111,nof_variables_selected = 15,selection_method = "LARS")
SE_LARS_20 <- finvar_regression(data = full_extension_data,window_length=120,start = 199001,end =202111,nof_variables_selected = 20,selection_method = "LARS")
SE_LARS_val <- finvar_regression(data = full_extension_data,window_length=120,start = 199001,end =202111,nof_variables_selected = -1,selection_method = "LARS")

SE_LASSO_bad_1 <- finvar_regression(data = full_extension_data,window_length=120,start = 199001,end =202111,nof_variables_selected = 1,selection_method = "LASSO", lambda_grid = 10^seq(4, -6, length = 1000),LASSOtype = "first-k")
SE_LASSO_bad_3 <- finvar_regression(data = full_extension_data,window_length=120,start = 199001,end =202111,nof_variables_selected = 3,selection_method = "LASSO", lambda_grid = 10^seq(4, -6, length = 1000),LASSOtype = "first-k")
SE_LASSO_bad_5 <- finvar_regression(data = full_extension_data,window_length=120,start = 199001,end =202111,nof_variables_selected = 5,selection_method = "LASSO", lambda_grid = 10^seq(4, -6, length = 1000),LASSOtype = "first-k")
SE_LASSO_bad_10 <- finvar_regression(data = full_extension_data,window_length=120,start = 199001,end =202111,nof_variables_selected = 10,selection_method = "LASSO", lambda_grid = 10^seq(4, -6, length = 1000),LASSOtype = "first-k")
SE_LASSO_bad_15 <- finvar_regression(data = full_extension_data,window_length=120,start = 199001,end =202111,nof_variables_selected = 15,selection_method = "LASSO", lambda_grid = 10^seq(4, -6, length = 1000),LASSOtype = "first-k")
SE_LASSO_bad_20 <- finvar_regression(data = full_extension_data,window_length=120,start = 199001,end =202111,nof_variables_selected = 20,selection_method = "LASSO", lambda_grid = 10^seq(4, -6, length = 1000),LASSOtype = "first-k")

set.seed(123)
SE_LASSO_good_1 <- finvar_regression(data = full_extension_data,window_length=120,start = 199001,end =202111,nof_variables_selected = 1,selection_method = "LASSO", lambda_grid = 10^seq(1, -0.001, length = 500),LASSOtype = "CV-k")
set.seed(123)
SE_LASSO_good_3 <- finvar_regression(data = full_extension_data,window_length=120,start = 199001,end =202111,nof_variables_selected = 3,selection_method = "LASSO", lambda_grid = 10^seq(0.6, -0.9, length = 500),LASSOtype ="CV-k")
set.seed(123)
SE_LASSO_good_5 <- finvar_regression(data = full_extension_data,window_length=120,start = 199001,end =202111,nof_variables_selected = 5,selection_method = "LASSO", lambda_grid = 10^seq(1.2, -0.9, length = 500),LASSOtype = "CV-k")
set.seed(123)
SE_LASSO_good_10 <- finvar_regression(data = full_extension_data,window_length=120,start = 199001,end =202111,nof_variables_selected = 10,selection_method = "LASSO", lambda_grid = 10^seq(0.5, -1.5, length = 500),LASSOtype = "CV-k")
set.seed(123)
SE_LASSO_good_15 <- finvar_regression(data = full_extension_data,window_length=120,start = 199001,end =202111,nof_variables_selected = 15,selection_method = "LASSO", lambda_grid = 10^seq(0.1, -2, length = 500),LASSOtype = "CV-k")
set.seed(123)
SE_LASSO_good_20 <- finvar_regression(data = full_extension_data,window_length=120,start = 199001,end =202111,nof_variables_selected = 20,selection_method = "LASSO", lambda_grid = 10^seq(2, -4, length = 500),LASSOtype = "CV-FULL")

SE_LASSO_val <- finvar_regression(data = full_extension_data,window_length=120,start = 199001,end =202111,selection_method = "LASSO",lambda_grid = 10^seq(4, -6, length = 1000),LASSOtype = "CV-FULL")

set.seed(123)
importance_vars_sorted <- get_var_importance(data = full_extension_data,window_length=120,start = 199001,end =202111)


SE_RF_1 <- finvar_regression(data = full_extension_data,window_length=120,start = 199001,end =202111,nof_variables_selected = 1,selection_method = "RF", importance_vars_sorted = importance_vars_sorted)
SE_RF_3 <- finvar_regression(data = full_extension_data,window_length=120,start = 199001,end =202111,nof_variables_selected = 3,selection_method = "RF", importance_vars_sorted = importance_vars_sorted)
SE_RF_5 <- finvar_regression(data = full_extension_data,window_length=120,start = 199001,end =202111,nof_variables_selected = 5,selection_method = "RF", importance_vars_sorted = importance_vars_sorted)
SE_RF_10 <- finvar_regression(data = full_extension_data,window_length=120,start = 199001,end =202111,nof_variables_selected = 10,selection_method = "RF", importance_vars_sorted = importance_vars_sorted)
SE_RF_15 <- finvar_regression(data = full_extension_data,window_length=120,start = 199001,end =202111,nof_variables_selected = 15,selection_method = "RF", importance_vars_sorted = importance_vars_sorted)
SE_RF_20 <- finvar_regression(data = full_extension_data,window_length=120,start = 199001,end =202111,nof_variables_selected = 20 ,selection_method = "RF", importance_vars_sorted = importance_vars_sorted)

set.seed(123)
SE_RF_val <- finvar_regression(data = full_extension_data,window_length=120,start = 199001,end =202111,nof_variables_selected = -1,selection_method = "RF")





selected_df <- purrr::map_dfr(
  .x = names(SE$selected_vars),
  .f = function(time) {
    data.frame(
      Quarter = time,
      Variable = selected_vars_list[[time]],
      stringsAsFactors = FALSE
    )
  }
)

plot_selected_variables <- function(selected_df, title = "Selected Variables Over Time") {
  # Ensure Quarter is character (for discrete axis)
  selected_df$Quarter <- as.character(selected_df$Quarter)
  
  # Extract Q1s for labeling years
  q1_quarters <- unique(selected_df$Quarter[endsWith(selected_df$Quarter, "Q1")])
  year_labels <- substr(q1_quarters, 1, 4)
  
  # Assign numeric ID to each variable for y-axis
  if (!"Variable_ID" %in% names(selected_df)) {
    selected_df <- selected_df %>%
      dplyr::mutate(Variable_ID = as.numeric(as.factor(Variable)))
  }
  
  ggplot(selected_df, aes(x = Quarter, y = Variable_ID)) +
    geom_point(shape = 3) +
    labs(
      x = "Year",
      y = "Selected Variable ID",
      title = title
    ) +
    scale_x_discrete(breaks = q1_quarters, labels = year_labels) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
      panel.grid.minor = element_blank()
    )
}

set_for_macro <- list(historical_average = historical_average, corr_1 = SE_corr_1, corr_3 = SE_corr_3, LARS_1 = SE_LARS_1, LARS_3 = SE_LARS_3, LASSO_1 = SE_LASSO_bad_1, LASSO_3 = SE_LASSO_bad_3, LASSO_1 = SE_LASSO_good_1, LASSO_3 = SE_LASSO_good_3,LARS_1 = SE_LARS_1, LARS_3 = SE_LARS_3,RF_1 = SE_RF_1, RF_3 = SE_RF_3)

saveRDS(set_for_macro, file = "set_for_macro.rds")






#Multi period analysis

multiperiod_variables_selected <- function(number_of_periods, selected_set) {
  total_vars_selected <- 0
  iteration <- 0
  selected_vars_list <- list()
  
  dates <- data %>%
    filter(date >= start & date <= end) %>%
    select(date) %>%
    pull()
  
  SE <- 0
  plot_forecasts <- data.frame(
    date = numeric(),
    actuals = numeric(),
    forecasts = numeric()
  )
  
  current_vars <- NULL
  chunk_start_index <- 1
  
  for (i in seq_along(dates)) {
    time <- dates[i]
    chunk_index <- ceiling(i / number_of_periods)
    
    if ((i - 1) %% number_of_periods == 0) {
      current_vars <- selected_set$selected_vars[[as.character(time)]]
      chunk_start_index <- i
    }
    
    selected_vars_list[[as.character(time)]] <- current_vars
    total_vars_selected <- total_vars_selected + length(current_vars)
    iteration <- iteration + 1
    
    result <- regression_forecast(data, current_vars, window_length, time)
    SE <- SE + (result$excess_return - result$forecast)^2
    plot_forecasts <- rbind(plot_forecasts, data.frame(
      date = time,
      actuals = result$excess_return,
      forecasts = result$forecast
    ))
    
    print(paste0("Timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                 " | Date: ", time,
                 " | Forecast: ", round(result$forecast, 4),
                 " | Actual: ", round(result$excess_return, 4),
                 " | Cumulative SE: ", round(SE, 4),
                 " | Average Selected: ", total_vars_selected / iteration,
                 " | Current chunk vars: ", paste(current_vars, collapse = ", ")))
    
    if (time == end) {
      return(list(
        plot_forecasts = plot_forecasts,
        SE = SE,
        selected_vars = selected_vars_list
      ))
    }
  }
}


# CORR
mult_period_corr_1  <- multiperiod_variables_selected(60, SE_corr_1)
mult_period_corr_3  <- multiperiod_variables_selected(60, SE_corr_3)
mult_period_corr_5  <- multiperiod_variables_selected(60, SE_corr_5)
mult_period_corr_10 <- multiperiod_variables_selected(60, SE_corr_10)
mult_period_corr_15 <- multiperiod_variables_selected(60, SE_corr_15)
mult_period_corr_20 <- multiperiod_variables_selected(60, SE_corr_20)
mult_period_corr_val <- multiperiod_variables_selected(60, SE_corr_val)


# LARS
mult_period_lars_1  <- multiperiod_variables_selected(60, SE_LARS_1)
mult_period_lars_3  <- multiperiod_variables_selected(60, SE_LARS_3)
mult_period_lars_5  <- multiperiod_variables_selected(60, SE_LARS_5)
mult_period_lars_10 <- multiperiod_variables_selected(60, SE_LARS_10)
mult_period_lars_15 <- multiperiod_variables_selected(60, SE_LARS_15)
mult_period_lars_20 <- multiperiod_variables_selected(60, SE_LARS_20)
mult_period_lars_val <- multiperiod_variables_selected(60, SE_LARS_val)

# LASSO Good
mult_period_lasso_good_1  <- multiperiod_variables_selected(60, SE_LASSO_good_1)
mult_period_lasso_good_3  <- multiperiod_variables_selected(60, SE_LASSO_good_3)
mult_period_lasso_good_5  <- multiperiod_variables_selected(60, SE_LASSO_good_5)
mult_period_lasso_good_10 <- multiperiod_variables_selected(60, SE_LASSO_good_10)
mult_period_lasso_good_15 <- multiperiod_variables_selected(60, SE_LASSO_good_15)
mult_period_lasso_good_20 <- multiperiod_variables_selected(60, SE_LASSO_good_20)
mult_period_lasso_good_val <- multiperiod_variables_selected(60, SE_LASSO_val)


# RF
mult_period_rf_1  <- multiperiod_variables_selected(60, SE_RF_1)
mult_period_rf_3  <- multiperiod_variables_selected(60, SE_RF_3)
mult_period_rf_5  <- multiperiod_variables_selected(60, SE_RF_5)
mult_period_rf_10 <- multiperiod_variables_selected(60, SE_RF_10)
mult_period_rf_15 <- multiperiod_variables_selected(60, SE_RF_15)
mult_period_rf_20 <- multiperiod_variables_selected(60, SE_RF_20)
mult_period_rf_val <- multiperiod_variables_selected(60, SE_RF_val)
