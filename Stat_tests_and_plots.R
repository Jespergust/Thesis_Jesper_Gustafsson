install.packages("ggplot2")
install.packages("lubridate")
install.packages("zoo")
library(zoo)
library(ggplot2)
library(lubridate)

proportion_correct_sign_product <- function(actuals, forecasts) {
  hits <- (actuals * forecasts) > 0
  return(mean(hits))
}

proportion_correct_sign_single <- function(data) {
  hits <- (data) > 0
  return(mean(hits))
}

DA_test <- function(actuals, forecasts) {
  P_hat <- proportion_correct_sign_product(actuals, forecasts)
  P_hat_r <- proportion_correct_sign_single(actuals)
  P_hat_r_star <- proportion_correct_sign_single(forecasts)
  
  P_hat_star <- P_hat_r*P_hat_r_star + (1-P_hat_r)*(1-P_hat_r_star)
  
  var_P_hat <- P_hat*(1-P_hat) / length(actuals)
  var_P_hat_star <- ((2*P_hat_r_star - 1)^2 * P_hat_r * (1 - P_hat_r) + (2*P_hat_r - 1)^2 * P_hat_r_star * (1 - P_hat_r_star)) / length(actuals)
  
  test_stat <- (P_hat - P_hat_star)/ sqrt(var_P_hat - var_P_hat_star)
  p_value <- 2 * (1 - pnorm(abs(test_stat)))
  return(list(statistic = test_stat, p_value = p_value))
}

EP_test <- function(actuals, forecasts) {
  T <- length(actuals)
  forecasts_sign <- sign(forecasts)
  
  active_returns <- ifelse(forecasts > 0, actuals, -actuals)
  passive_returns <- actuals
  
  return_diff <- active_returns - passive_returns
  mean_diff <- mean(return_diff)
  
  p_hat <- 0.5 * (1 + mean(forecasts_sign))
  
  var_y <- sum((actuals - mean(actuals))^2)

  var_ep <- (4 / T^2) * p_hat * (1 - p_hat) * var_y

  EP_stat <- mean_diff / sqrt(var_ep)

  p_value <- 2 * (1 - pnorm(abs(EP_stat)))
  
  return(list(statistic = EP_stat, p_value = p_value))
}

list_forecasts_corr_only_finvar <- list(SE_corr_1, SE_corr_3, SE_corr_5, SE_corr_10, SE_corr_15, SE_corr_20, SE_corr_val)
list_forecasts_LARS_only_finvar <- list(SE_LARS_1,SE_LARS_3,SE_LARS_5,SE_LARS_10,SE_LARS_15,SE_LARS_20,SE_LARS_val)
list_forecasts_LASSO_only_finvar <- list(SE_LASSO_good_1,SE_LASSO_good_3,SE_LASSO_good_5,SE_LASSO_good_10,SE_LASSO_good_15,SE_LASSO_good_20,SE_LASSO_val)
list_forecasts_RF_only_finvar <- list(SE_RF_1,SE_RF_3,SE_RF_5,SE_RF_10,SE_RF_15,SE_RF_20,SE_RF_val)

list_forecasts_corr_mult_period <- list(mult_period_corr_1, mult_period_corr_3, mult_period_corr_5, 
                                        mult_period_corr_10, mult_period_corr_15, mult_period_corr_20, mult_period_corr_val)

list_forecasts_lars_mult_period <- list(mult_period_lars_1, mult_period_lars_3, mult_period_lars_5, 
                                        mult_period_lars_10, mult_period_lars_15, mult_period_lars_20, mult_period_lars_val)

list_forecasts_lasso_mult_period <- list(mult_period_lasso_good_1, mult_period_lasso_good_3, mult_period_lasso_good_5, 
                                         mult_period_lasso_good_10, mult_period_lasso_good_15, mult_period_lasso_good_20, mult_period_lasso_good_val)

list_forecasts_rf_mult_period <- list(mult_period_rf_1, mult_period_rf_3, mult_period_rf_5, 
                                      mult_period_rf_10, mult_period_rf_15, mult_period_rf_20, mult_period_rf_val)

list_forecasts_historical_average <- list(historical_average)

list_forecasts_single_vars <- ex_post_optimal_single_var$all_results #?

list_forecasts_with_macro_128 <- list(
  Corr_1_128_with_macro, Corr_3_128_with_macro, Corr_5_128_with_macro,
  Corr_10_128_with_macro, Corr_15_128_with_macro, Corr_20_128_with_macro, Corr_val_128_with_macro,
  
  LARS_1_128_with_macro, LARS_3_128_with_macro, LARS_5_128_with_macro,
  LARS_10_128_with_macro, LARS_15_128_with_macro, LARS_20_128_with_macro, LARS_val_128_with_macro,
  
  LASSO_good_1_128_with_macro, LASSO_good_3_128_with_macro, LASSO_good_5_128_with_macro,
  LASSO_good_10_128_with_macro, LASSO_good_15_128_with_macro, LASSO_good_20_128_with_macro, LASSO_good_val_128_with_macro,
  
  RF_1_128_with_macro, RF_3_128_with_macro, RF_5_128_with_macro,
  RF_10_128_with_macro, RF_15_128_with_macro, RF_20_128_with_macro, RF_val_128_with_macro
)

list_forecasts_104_with_macro <- list(
  Corr_1_104_with_macro, Corr_3_104_with_macro, Corr_5_104_with_macro,
  Corr_10_104_with_macro, Corr_15_104_with_macro, Corr_20_104_with_macro, Corr_val_104_with_macro,
  
  LARS_1_104_with_macro, LARS_3_104_with_macro, LARS_5_104_with_macro,
  LARS_10_104_with_macro, LARS_15_104_with_macro, LARS_20_104_with_macro, LARS_val_104_with_macro,
  
  LASSO_good_1_104_with_macro, LASSO_good_3_104_with_macro, LASSO_good_5_104_with_macro,
  LASSO_good_10_104_with_macro, LASSO_good_15_104_with_macro, LASSO_good_20_104_with_macro, LASSO_good_val_104_with_macro,
  
  RF_1_104_with_macro, RF_3_104_with_macro, RF_5_104_with_macro,
  RF_10_104_with_macro, RF_15_104_with_macro, RF_20_104_with_macro, RF_val_104_with_macro
)

list_forecasts_soft_with_macro <- list(
  Corr_1_soft_with_macro, Corr_3_soft_with_macro, Corr_5_soft_with_macro,
  Corr_10_soft_with_macro, Corr_15_soft_with_macro, Corr_20_soft_with_macro, Corr_val_soft_with_macro,
  
  LARS_1_soft_with_macro, LARS_3_soft_with_macro, LARS_5_soft_with_macro,
  LARS_10_soft_with_macro, LARS_15_soft_with_macro, LARS_20_soft_with_macro, LARS_val_soft_with_macro,
  
  LASSO_good_1_soft_with_macro, LASSO_good_3_soft_with_macro, LASSO_good_5_soft_with_macro,
  LASSO_good_10_soft_with_macro, LASSO_good_15_soft_with_macro, LASSO_good_20_soft_with_macro, LASSO_good_val_soft_with_macro,
  
  RF_1_soft_with_macro, RF_3_soft_with_macro, RF_5_soft_with_macro,
  RF_10_soft_with_macro, RF_15_soft_with_macro, RF_20_soft_with_macro, RF_val_soft_with_macro
)

names(list_forecasts_corr_only_finvar) <- paste0("Corr_", c(1, 3, 5, 10, 15, 20, "val"))

names(list_forecasts_LARS_only_finvar) <- paste0("LARS_", c(1, 3, 5, 10, 15, 20, "val"))

names(list_forecasts_LASSO_only_finvar) <- paste0("LASSO_good_", c(1, 3, 5, 10, 15, 20, "val"))

names(list_forecasts_RF_only_finvar) <- paste0("RF_", c(1, 3, 5, 10, 15, 20, "val"))



names(list_forecasts_corr_mult_period) <- paste0("Corr_", c(1, 3, 5, 10, 15, 20, "val"))

names(list_forecasts_lars_mult_period) <- paste0("LARS_", c(1, 3, 5, 10, 15, 20, "val"))

names(list_forecasts_lasso_mult_period) <- paste0("LASSO_good_", c(1, 3, 5, 10, 15, 20, "val"))

names(list_forecasts_rf_mult_period) <- paste0("RF_", c(1, 3, 5, 10, 15, 20, "val"))
names(list_forecasts_historical_average) <- "Historical Average"

names(list_forecasts_with_macro_128) <- c(
  paste0("Corr_", c(1,3,5,10,15,20,"val"), "_128"),
  paste0("LARS_", c(1,3,5,10,15,20,"val"), "_128"),
  paste0("LASSO_good_", c(1,3,5,10,15,20,"val"), "_128"),
  paste0("RF_", c(1,3,5,10,15,20,"val"), "_128")
)

names(list_forecasts_104_with_macro) <- c(
  paste0("Corr_", c(1,3,5,10,15,20,"val"), "_104"),
  paste0("LARS_", c(1,3,5,10,15,20,"val"), "_104"),
  paste0("LASSO_good_", c(1,3,5,10,15,20,"val"), "_104"),
  paste0("RF_", c(1,3,5,10,15,20,"val"), "_104")
)

names(list_forecasts_soft_with_macro) <- c(
  paste0("Corr_", c(1,3,5,10,15,20,"val"), "_soft"),
  paste0("LARS_", c(1,3,5,10,15,20,"val"), "_soft"),
  paste0("LASSO_good_", c(1,3,5,10,15,20,"val"), "_soft"),
  paste0("RF_", c(1,3,5,10,15,20,"val"), "_soft")
)







evaluate_forecasts <- function(forecast_list) {
  results <- data.frame(
    Model = character(),
    MSPE = numeric(),
    MAE = numeric(),
    DA_stat = numeric(),
    DA_p = numeric(),
    EP_stat = numeric(),
    EP_p = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(forecast_list)) {
    f <- forecast_list[[i]][[1]]
    forecast <- f$forecasts
    actual <- f$actuals
    

    errors <- actual - forecast
    mspe <- mean(errors^2, na.rm = TRUE)
    mae <- mean(abs(errors), na.rm = TRUE)

    da <- DA_test(actual,forecast)
    
    # EP test
    ep <- EP_test(actual,forecast)
    
    # Save row
    results[i, ] <- list(
      Model = names(forecast_list)[i],
      MSPE = mspe,
      MAE = mae,
      DA_stat = da$statistic,
      DA_p = da$p_value,
      EP_stat = ep$statistic,
      EP_p = ep$p_value
    )
  }
  
  return(results)
}

Corr_financial   <- evaluate_forecasts(list_forecasts_corr_only_finvar)
LARS_financial   <- evaluate_forecasts(list_forecasts_LARS_only_finvar)
LASSO_financial  <- evaluate_forecasts(list_forecasts_LASSO_only_finvar)
RF_financial     <- evaluate_forecasts(list_forecasts_RF_only_finvar)


Corr_financial_mult   <- evaluate_forecasts(list_forecasts_corr_mult_period)
LARS_financial_mult   <- evaluate_forecasts(list_forecasts_lars_mult_period)
LASSO_financial_mult  <- evaluate_forecasts(list_forecasts_lasso_mult_period)
RF_financial_mult     <- evaluate_forecasts(list_forecasts_rf_mult_period)

Macro_128        <- evaluate_forecasts(list_forecasts_with_macro_128)
Macro_104        <- evaluate_forecasts(list_forecasts_104_with_macro)
Macro_soft       <- evaluate_forecasts(list_forecasts_soft_with_macro)

historical_ave_FULL <- evaluate_forecasts(list_forecasts_historical_average)
View(Corr_financial_mult)
View(LARS_financial_mult)
View(LASSO_financial_mult)
View(RF_financial_mult)
View(Macro_128)
View(Macro_104)
View(Macro_soft)













single_var_FULL <- evaluate_forecasts(list_forecasts_single_vars)


plot_active_vs_passive_cum <- function(title,actuals, forecasts, dates, frequency = 1) {
  # Convert annualized % returns to per-period decimal
  active_ann_pct <- ifelse(forecasts > 0, actuals, -actuals)
  passive_ann_pct <- actuals
  
  active_returns <- (1 + active_ann_pct / 100)^(1 / frequency) - 1
  passive_returns <- (1 + passive_ann_pct / 100)^(1 / frequency) - 1
  
  # Compute cumulative gross return
  cum_active <- cumprod(1 + active_returns) - 1
  cum_passive <- cumprod(1 + passive_returns) - 1
  
  df <- data.frame(
    date = as.Date(paste0(dates, "01"), format = "%Y%m%d"),
    cum_active = cum_active,
    cum_passive = cum_passive
  )
  df <- df[order(df$date), ]
  
  Sys.setlocale("LC_TIME", "C")
  
  plot(df$date, df$cum_active,
       type = "n",
       ylim = range(c(0, 28), na.rm = TRUE),
       xlab = "",
       ylab = "",
       main = title,
       axes = FALSE,
       xaxs = "i", font.main=1, cex.main = 0.95)
  
  abline(h = 0, col = "gray", lwd = 1)
  abline(h = pretty(range(0,25)), col = "lightgray", lty = "dotted")
  
  axis.Date(1, at = seq(from = min(df$date), to = max(df$date), by = "2 years"), format = "%y")
  axis(2)
  
  lines(df$date, df$cum_active, col = "black", lwd = 2, lty = 1)
  lines(df$date, df$cum_passive, col = "black", lwd = 2, lty = 2)
  box()
  
  legend("topleft",
         legend = c("Active Strategy", "Passive Buy-and-Hold"),
         col = c("black", "black"),
         lty = c(1, 2),
         lwd = 2,
         bty = "o",
         cex = 0.8)
}


plot_active_vs_passive <- function(actuals, forecasts, dates) {
  # Build returns
  active_returns <- ifelse(forecasts > 0, actuals, -actuals)
  passive_returns <- actuals
  
  df <- data.frame(
    date = as.Date(paste0(dates, "01"), format = "%Y%m%d"),
    active = active_returns,
    passive = passive_returns
  )
  df <- df[order(df$date), ]
  
  Sys.setlocale("LC_TIME", "C")  # Ensures consistent date labels
  
  # Plot setup
  plot(df$date, df$active, 
       type = "n",
       ylim = range(c(df$active, df$passive), na.rm = TRUE),
       xlab = "Date",
       ylab = "Return",
       main = "Active vs Passive Returns",
       axes = FALSE,
       xaxs = "i")
  
  abline(h = 0, col = "gray", lwd = 1)           # Zero line
  abline(h = seq(min(df$active, df$passive, na.rm = TRUE),
                 max(df$active, df$passive, na.rm = TRUE),
                 by = 1), col = "lightgray")      # Grid lines
  axis.Date(1, at = seq(from = min(df$date), to = max(df$date), by = "2 years"), format = "%b %y")
  axis(2)
  
  lines(df$date, df$active, col = "black", lwd = 2, lty = 1)    # Active returns
  lines(df$date, df$passive, col = "red", lwd = 2, lty = 2)     # Passive returns
  
  legend("topright",
         legend = c("Active Returns", "Passive Returns"),
         col = c("black", "red"),
         lty = c(1, 2),
         lwd = 2,
         bty = "o",
         cex = 0.8)
}
par(mfrow = c(2, 1), mar = c(4, 4, 2, 2))

#Plot active vs passive returns for the Correlation 5 1.04 as this performed best based on E
plot_active_vs_passive_cum("RF model with 1 financial variable, Threshold = 1.04",actuals , RF_1_104_with_macro$plot_forecasts$forecasts,RF_1_104_with_macro$plot_forecasts$date)
plot_active_vs_passive_cum("LASSO model with 15 financial variables, Threshold = soft",actuals , LASSO_good_15_soft_with_macro$plot_forecasts$forecasts,RF_1_104_with_macro$plot_forecasts$date)



plot_hit_data <- function(actuals, forecasts, window_size, dates) {
  n <- length(actuals)
  
  hit_ratios <- rep(NA, n - window_size + 1)
  expected_hit_ratios <- rep(NA, n - window_size + 1)
  plot_dates <- dates[window_size:n]
  
  for (i in 1:(n - window_size + 1)) {
    a_window <- actuals[i:(i + window_size - 1)]
    f_window <- forecasts[i:(i + window_size - 1)]
    
    hit_ratios[i] <- proportion_correct_sign_product(a_window, f_window)
    P_r <- proportion_correct_sign_single(a_window)
    P_f <- proportion_correct_sign_single(f_window)
    expected_hit_ratios[i] <- P_r * P_f + (1 - P_r) * (1 - P_f)
  }
  return(list(hit = hit_ratios,exp_hit = expected_hit_ratios))
}
plot_hits <- function(plot_dates, hit_ratios, expected_hit_ratios, title, ymin = NULL, ymax = NULL) {
  df <- data.frame(
    date = as.Date(paste0(plot_dates, "01"), format = "%Y%m%d"),
    hit_ratio = hit_ratios,
    expected_hit = expected_hit_ratios
  )
  
  df <- df[order(df$date), ]
  Sys.setlocale("LC_TIME", "C")
  
  # Use provided limits or fallback to automatic range
  y_range <- range(c(df$hit_ratio, df$expected_hit), na.rm = TRUE)
  if (is.null(ymin)) ymin <- y_range[1]
  if (is.null(ymax)) ymax <- y_range[2]
  
  plot(df$date, df$hit_ratio, type = "n", lwd = 2, col = "black",
       ylim = c(ymin, ymax),
       xlab = "", ylab = "", main = title,
       axes = FALSE, xaxs = "i", font.main=1, cex.main = 0.95)
  
  abline(h = seq(0, 1, by = 0.05), col = "lightgray")
  
  axis.Date(
    side = 1,
    at = seq(from = min(df$date), to = max(df$date), by = "2 years"),
    format = "%y"
  )
  axis(2)
  
  polygon(
    x = c(df$date[1], df$date, df$date[length(df$date)]),
    y = c(par("usr")[3], df$expected_hit, par("usr")[3]),
    col = "gray90", border = NA
  )
  
  box()
  lines(df$date, df$expected_hit, col = "gray50", lwd = 2, lty = 2)
  lines(df$date, df$hit_ratio, col = "black", lwd = 2)
}

hits_Corr_ratios1 <- plot_hit_data(
  SE_corr_1$plot_forecasts$actuals,
  Corr_10_128_with_macro$plot_forecasts$forecasts,
  60,
  SE_corr_1$plot_forecasts$date[60:length(SE_corr_1$plot_forecasts$date)]
)

hits_Corr_ratios2 <- plot_hit_data(
  SE_corr_1$plot_forecasts$actuals,
  Corr_5_soft_with_macro$plot_forecasts$forecasts,
  60,
  SE_corr_1$plot_forecasts$date[60:length(SE_corr_1$plot_forecasts$date)]
)


all_vals <- c(hits_Corr_ratios1$hit, hits_Corr_ratios1$exp_hit,
              hits_Corr_ratios2$hit, hits_Corr_ratios2$exp_hit)
ymin <- min(all_vals, na.rm = TRUE)
ymax <- max(all_vals, na.rm = TRUE)


par(mfrow = c(2, 1), mar = c(4, 4, 2, 2))


actuals <- SE_corr_1$plot_forecasts$actuals

plot_hits(
  SE_corr_1$plot_forecasts$date[60:length(SE_corr_1$plot_forecasts$date)],
  hits_Corr_ratios1$hit,
  hits_Corr_ratios1$exp_hit,
  title= "Correlation model with 5 financial variables, Threshold = soft",
  ymin = ymin,
  ymax = ymax
)
plot_hits(
  SE_corr_1$plot_forecasts$date[60:length(SE_corr_1$plot_forecasts$date)],
  hits_Corr_ratios2$hit,
  hits_Corr_ratios2$exp_hit,
  title = "Correlation model with 10 financial variables, Threshold = 1.28",
  ymin = ymin,
  ymax = ymax
)
#plot hit rate over time for Corr 5 1.04 as this was the best based on DA stat