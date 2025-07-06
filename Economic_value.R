install.packages("tidyverse")
install.packages("lubridate")
install.packages("PerformanceAnalytics")
install.packages("fredr")
install.packages("tidyquant")
install.packages("zoo")
library(tidyverse)
library(lubridate)
library(PerformanceAnalytics)
library(fredr)
library(tidyquant)
library(zoo)
calculate_realized_variance <- function(daily_returns) {
  n_days <- length(daily_returns)
  mean_return <- mean(daily_returns, na.rm = TRUE)
  
  # Calculate first-order autocorrelation
  phi_hat <- cor(daily_returns[-n_days], daily_returns[-1], use = "complete.obs")
  if(is.na(phi_hat)) phi_hat <- 0
  
  # French, Schwert, and Stambaugh (1987) formula
  sum_squared_deviations <- sum((daily_returns - mean_return)^2, na.rm = TRUE)
  
  # Correction for autocorrelation
  correction_factor <- 1 + (2/n_days) * sum(sapply(1:(n_days-1), function(j) {
    (n_days - j) * phi_hat^j
  }))
  
  realized_var <- sum_squared_deviations * correction_factor
  return(realized_var)
}

# Function to calculate optimal portfolio weights
calculate_optimal_weight <- function(expected_excess_return, variance_excess_return, 
                                     gamma = 6, weight_bounds = c(0, 1)) {
  if(variance_excess_return <= 0) return(0)
  
  optimal_weight <- expected_excess_return / (gamma * variance_excess_return)
  
  # Apply constraints
  optimal_weight <- pmax(weight_bounds[1], pmin(weight_bounds[2], optimal_weight))
  
  return(optimal_weight)
}

# Function to calculate transaction costs
calculate_transaction_costs <- function(current_weight, previous_weight, cost_rate = 0.001) {
  return(2 * cost_rate * abs(current_weight - previous_weight))
}

# Function to calculate utility
calculate_utility <- function(portfolio_returns, gamma = 6, initial_wealth = 1) {
  n <- length(portfolio_returns)
  gross_returns <- 1 + portfolio_returns
  
  utility_sum <- sum(gross_returns - (0.5 * gamma / (1 + gamma)) * gross_returns^2)
  
  return(initial_wealth * utility_sum / n)
}

calculate_performance_fee <- function(alternative_returns, benchmark_returns, gamma = 6) {
  # Ensure vectors are same length and handle NA values
  n <- min(length(alternative_returns), length(benchmark_returns))
  alt_returns <- alternative_returns[1:n]
  bench_returns <- benchmark_returns[1:n]
  
  # Remove any NA values
  valid_indices <- !is.na(alt_returns) & !is.na(bench_returns) & 
    is.finite(alt_returns) & is.finite(bench_returns)
  
  if(sum(valid_indices) < 10) {
    warning("Too few valid observations for performance fee calculation")
    return(NA)
  }
  
  alt_returns <- alt_returns[valid_indices]
  bench_returns <- bench_returns[valid_indices]
  n <- length(alt_returns)
  
  # Calculate utilities for each strategy
  calc_utility <- function(returns, delta = 0) {
    gross_returns <- 1 + returns - delta/12  # Convert annual delta to monthly
    
    # Check for numerical issues
    if(any(!is.finite(gross_returns))) {
      return(-Inf)
    }
    
    utility_sum <- sum(gross_returns - (0.5 * gamma / (1 + gamma)) * gross_returns^2)
    return(utility_sum / n)
  }
  
  # Function to find the delta that equates utilities
  utility_difference <- function(delta) {
    alt_utility <- calc_utility(alt_returns, delta)
    bench_utility <- calc_utility(bench_returns, 0)
    
    if(!is.finite(alt_utility) || !is.finite(bench_utility)) {
      return(1e10)  # Large penalty for invalid utilities
    }
    
    return(abs(alt_utility - bench_utility))
  }
  
  # Try different optimization approaches
  tryCatch({
    # First try with a reasonable range
    result <- optimize(utility_difference, interval = c(-0.2, 0.2))
    
    # Check if the result makes sense
    if(!is.finite(result$minimum) || abs(result$objective) > 1e-3) {
      # If optimization failed, calculate a simple approximation
      alt_mean <- mean(alt_returns, na.rm = TRUE)
      bench_mean <- mean(bench_returns, na.rm = TRUE)
      return((alt_mean - bench_mean) * 1200)  # Simple difference * 1200 (monthly to annual %)
    }
    
    return(result$minimum * 1200)  # Convert to annual basis points
    
  }, error = function(e) {
    # Fallback to simple mean difference
    alt_mean <- mean(alt_returns, na.rm = TRUE)
    bench_mean <- mean(bench_returns, na.rm = TRUE)
    return((alt_mean - bench_mean) * 1200)
  })
}

run_economic_analysis <- function(excess_returns, risk_free_returns, 
                                  forecasts_list, daily_returns_list,
                                  gamma = 6, transaction_costs = c(0, 0.001, 0.003),
                                  weight_bounds_list = list(c(-1, 2), c(0, 1))) {
  
  n_periods <- length(excess_returns)
  results <- list()
  
  for(bound_name in names(weight_bounds_list)) {
    weight_bounds <- weight_bounds_list[[bound_name]]
    
    for(tc_rate in transaction_costs) {
      tc_name <- paste0("tc_", tc_rate * 100, "pct")
      
      # Storage for results
      strategy_results <- list()
      
      # Calculate portfolio performance for each forecasting strategy
      for(strategy_name in names(forecasts_list)) {
        forecasts <- forecasts_list[[strategy_name]]
        
        # Initialize
        weights <- numeric(n_periods)
        portfolio_returns <- numeric(n_periods)
        transaction_cost_series <- numeric(n_periods)
        
        previous_weight <- 0
        
        for(t in 1:n_periods) {
          if(t == 1) {
            # Use first forecast and realized variance from daily returns
            expected_return <- forecasts[t]
            if(!is.null(daily_returns_list) && length(daily_returns_list) >= t) {
              realized_var <- calculate_realized_variance(daily_returns_list[[t]])
            } else {
              # Fallback: use rolling variance of monthly excess returns
              if(t > 12) {
                realized_var <- var(excess_returns[(t-12):(t-1)], na.rm = TRUE)
              } else {
                realized_var <- var(excess_returns[1:t], na.rm = TRUE)
              }
            }
          } else {
            expected_return <- forecasts[t]
            if(!is.null(daily_returns_list) && length(daily_returns_list) >= (t-1)) {
              realized_var <- calculate_realized_variance(daily_returns_list[[t-1]])
            } else {
              realized_var <- var(excess_returns[max(1, t-12):(t-1)], na.rm = TRUE)
            }
          }
          
          weights[t] <- calculate_optimal_weight(expected_return, realized_var, 
                                                 gamma, weight_bounds)
          
          transaction_cost_series[t] <- calculate_transaction_costs(weights[t], 
                                                                    previous_weight, tc_rate)
          
          portfolio_returns[t] <- risk_free_returns[t] + weights[t] * excess_returns[t] - 
            transaction_cost_series[t]
          
          previous_weight <- weights[t]
        }
        
        strategy_results[[strategy_name]] <- list(
          returns = portfolio_returns,
          weights = weights,
          transaction_costs = transaction_cost_series,
          mean_return = mean(portfolio_returns, na.rm = TRUE),
          std_return = sd(portfolio_returns, na.rm = TRUE),
          sharpe_ratio = mean(portfolio_returns, na.rm = TRUE) / sd(portfolio_returns, na.rm = TRUE),
          utility = calculate_utility(portfolio_returns, gamma)
        )
      }
      
      benchmark_strategies <- c()
      passive_strategies <- list(
        "100pct_market" = risk_free_returns + excess_returns,  
        "50pct_market" = risk_free_returns + 0.5 * excess_returns, 
        "0pct_market" = risk_free_returns
      )
      
      for(passive_name in names(passive_strategies)) {
        passive_returns <- passive_strategies[[passive_name]]
        
        if(length(passive_returns) != n_periods) {
          if(length(passive_returns) > n_periods) {
            passive_returns <- passive_returns[1:n_periods]
          } else {
            passive_returns <- c(passive_returns, 
                                 rep(tail(passive_returns, 1), n_periods - length(passive_returns)))
          }
        }
        
        strategy_results[[passive_name]] <- list(
          returns = passive_returns,
          mean_return = mean(passive_returns, na.rm = TRUE),
          std_return = sd(passive_returns, na.rm = TRUE),
          sharpe_ratio = mean(passive_returns, na.rm = TRUE) / sd(passive_returns, na.rm = TRUE),
          utility = calculate_utility(passive_returns, gamma)
        )
      }
      
      for(strategy_name in names(strategy_results)) {
        if(!strategy_name %in% c(names(passive_strategies), benchmark_strategies)) {
          strategy_results[[strategy_name]]$performance_fees <- list()
          
          if(length(strategy_results[[strategy_name]]$returns) > 0 && 
             !all(is.na(strategy_results[[strategy_name]]$returns))) {
            
            for(passive_name in names(passive_strategies)) {
              if(length(strategy_results[[passive_name]]$returns) == 
                 length(strategy_results[[strategy_name]]$returns)) {
                delta <- calculate_performance_fee(
                  strategy_results[[strategy_name]]$returns,
                  strategy_results[[passive_name]]$returns,
                  gamma
                )
                strategy_results[[strategy_name]]$performance_fees[[paste0("delta_", passive_name)]] <- delta
              }
            }
            
            for(bench_name in benchmark_strategies) {
              if(bench_name %in% names(strategy_results) &&
                 length(strategy_results[[bench_name]]$returns) == 
                 length(strategy_results[[strategy_name]]$returns)) {
                delta <- calculate_performance_fee(
                  strategy_results[[strategy_name]]$returns,
                  strategy_results[[bench_name]]$returns,
                  gamma
                )
                strategy_results[[strategy_name]]$performance_fees[[paste0("delta_", bench_name)]] <- delta
              }
            }
          }
        }
      }
      
      results[[paste0(bound_name, "_", tc_name)]] <- strategy_results
    }
  }
  
  return(results)
}

# Function to create summary table like Table 3
create_summary_table <- function(results, scenario_name) {
  scenario_results <- results[[scenario_name]]
  
  summary_df <- data.frame(
    Strategy = names(scenario_results),
    Mean_Return = sapply(scenario_results, function(x) x$mean_return * 100),
    Std_Dev = sapply(scenario_results, function(x) x$std_return * 100),
    Sharpe_Ratio = sapply(scenario_results, function(x) x$sharpe_ratio),
    stringsAsFactors = FALSE
  )
  
  # Add performance fees where available
  for(strategy in names(scenario_results)) {
    if("performance_fees" %in% names(scenario_results[[strategy]])) {
      fees <- scenario_results[[strategy]]$performance_fees
      for(fee_name in names(fees)) {
        col_name <- gsub("delta_", "Δ_", fee_name)
        summary_df[summary_df$Strategy == strategy, col_name] <- fees[[fee_name]]
      }
    }
  }
  
  return(summary_df)
}

get_fred_stock_data <- function(start_date = "1990-01-01", end_date = "2021-12-31") {
  sp500_data <- tq_get("^GSPC", 
                       from = start_date, 
                       to = end_date,
                       get = "stock.prices")
  daily_returns <- sp500_data %>%
    arrange(date) %>%
    mutate(
      daily_return = (adjusted / lag(adjusted)) - 1,
      year_month = floor_date(date, "month")
    ) %>%
    filter(!is.na(daily_return))
  
  daily_returns_by_month <- daily_returns %>%
    group_by(year_month) %>%
    summarise(daily_rets = list(daily_return), .groups = 'drop') %>%
    arrange(year_month)
  
  return(list(
    daily_returns_list = daily_returns_by_month$daily_rets,
    daily_returns_raw = daily_returns
  ))
}

daily_data <- get_fred_stock_data()

# Example usage with simulated data
set.seed(123)
n_periods <- 383  # 40 years of monthly data
dates <- seq(as.Date("1990-01-01"), by = "month", length.out = n_periods)

# Simulate data
excess_returns <- SE_corr_1$plot_forecasts$actuals/100
risk_free_returns <- fin_data %>%
  filter(fin_data$date > 198912 & fin_data$date <= 202112) %>%
  select(Rfree)
risk_free_returns <- risk_free_returns$Rfree
# Create some example forecasting models
forecasts_list <- list(
  "RF-1-1.04" = RF_1_104_with_macro$plot_forecasts$forecasts/100, 
  "RF-1-1.28" = RF_1_128_with_macro$plot_forecasts$forecasts/100,  
  "RF-1-soft" = RF_1_soft_with_macro$plot_forecasts$forecasts/100,   
  "Corr-15-1.04" = Corr_15_104_with_macro$plot_forecasts$forecasts/100,
  "HA" = historical_average$plot_forecasts$forecasts/100
)

# Run analysis
weight_bounds_scenarios <- list(
  "unrestricted" = c(-1, 2),
  "long_only" = c(0, 1)
)

analysis_results <- run_economic_analysis(
  excess_returns = excess_returns,
  risk_free_returns = risk_free_returns,
  forecasts_list = forecasts_list,
  daily_returns_list = daily_data$daily_returns_list, 
  gamma = 6,
  transaction_costs = c(0, 0.001, 0.003),
  weight_bounds_list = weight_bounds_scenarios
)

# Create summary tables
print("Long-only, No Transaction Costs:")
summary_table <- create_summary_table(analysis_results, "long_only_tc_0pct")
print(summary_table)

print("\nUnrestricted, No Transaction Costs:")
summary_table2 <- create_summary_table(analysis_results, "unrestricted_tc_0pct")
print(summary_table2)

print("Long-only, 0.1% Transaction Costs:")
summary_table <- create_summary_table(analysis_results, "long_only_tc_0.1pct")
print(summary_table)

print("\nUnrestricted, 0.1% Transaction Costs:")
summary_table2 <- create_summary_table(analysis_results, "unrestricted_tc_0.1pct")
print(summary_table2)

print("Long-only, 0.3% Transaction Costs:")
summary_table <- create_summary_table(analysis_results, "long_only_tc_0.3pct")
print(summary_table)

print("\nUnrestricted, 0.3% Transaction Costs:")
summary_table2 <- create_summary_table(analysis_results, "unrestricted_tc_0.3pct")
print(summary_table2)






diagnose_data_issues <- function(excess_returns, risk_free_returns, forecasts_list) {
  cat("=== DATA DIAGNOSTICS ===\n")
  
  cat("Data lengths:\n")
  cat("  Excess returns:", length(excess_returns), "\n")
  cat("  Risk-free returns:", length(risk_free_returns), "\n")
  
  cat("\nForecast lengths:\n")
  for(name in names(forecasts_list)) {
    cat("  ", name, ":", length(forecasts_list[[name]]), "\n")
  }
  
  cat("\nData quality checks:\n")
  cat("  Excess returns - NA count:", sum(is.na(excess_returns)), "\n")
  cat("  Excess returns - Infinite count:", sum(!is.finite(excess_returns)), "\n")
  cat("  Risk-free returns - NA count:", sum(is.na(risk_free_returns)), "\n")
  
  cat("\nSummary statistics:\n")
  cat("  Excess returns mean:", round(mean(excess_returns, na.rm = TRUE), 4), "\n")
  cat("  Excess returns std:", round(sd(excess_returns, na.rm = TRUE), 4), "\n")
  cat("  Risk-free mean:", round(mean(risk_free_returns, na.rm = TRUE), 4), "\n")
  
  # Check for extreme values
  extreme_excess <- abs(excess_returns) > 0.5  # > 50% monthly return
  extreme_rf <- abs(risk_free_returns) > 0.1   # > 10% monthly risk-free rate
  
  if(any(extreme_excess, na.rm = TRUE)) {
    cat("  WARNING: Extreme excess returns detected!\n")
  }
  if(any(extreme_rf, na.rm = TRUE)) {
    cat("  WARNING: Extreme risk-free rates detected!\n")
  }
  
  cat("\n")
}
