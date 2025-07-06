install.packages("dplyr")
install.packages("ggplot2")
install.packages("zoo")
library(dplyr)
library(ggplot2)
library(zoo)

selected_df <- purrr::map_dfr(
  .x = names(SE_LASSO_good_1$selected_vars),
  .f = function(time) {
    data.frame(
      Quarter = time,
      Variable = SE_LASSO_good_1$selected_vars[[time]],
      stringsAsFactors = FALSE
    )
  }
)

names_data <- fin_data %>%
  select(-date, -infl, -cay,-'i/k',-pce,-govik
         ,-crdstd,-ogap,-wtexas,-ndrbl,-gpce,-gip,-house,-excess_return,-price)

plot_selected_variables <- function(selected_set, title = "", all_variables = colnames(names_data)) {
  
  # Extract dates and selected variables
  dates <- selected_set$plot_forecasts$date
  selected_vars <- selected_set$selected_vars
  
  # Build full variable list from selected_set if not provided
  if (is.null(all_variables)) {
    all_variables <- sort(unique(unlist(selected_vars)))
  }
  
  # Create full (date, variable) grid with selection indicator
  df <- purrr::map2_dfr(dates, selected_vars, ~ {
    data.frame(
      Date = as.numeric(.x),
      Variable = all_variables,
      Selected = all_variables %in% .y,
      stringsAsFactors = FALSE
    )
  })
  
  # Convert Date to proper format
  df$Date <- as.Date(as.yearmon(as.character(df$Date), "%Y%m"))
  
  # Set axis breaks
  year_range <- format(range(df$Date), "%Y")
  year_breaks <- seq(from = as.numeric(year_range[1]),
                     to   = as.numeric(year_range[2]),
                     by   = 10)
  date_breaks <- as.Date(paste0(year_breaks, "-01-01"))
  x_limits <- range(df$Date)
  
  # Plot
  ggplot(df %>% filter(Selected), aes(x = Date, y = Variable)) +
    geom_point(shape = 16, size = 1.2) +
    scale_x_date(
      breaks = date_breaks,
      labels = scales::date_format("%Y"),
      limits = x_limits,
      expand = c(0, 0)
    ) +
    scale_y_discrete(limits = rev(all_variables)) +  # consistent ordering
    labs(x = "", y = "", title = title) +
    theme_bw(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
      axis.text.y = element_text(size = 7),
      panel.grid.minor = element_blank()
    )
}

plot_selected_variables(SE_RF_1)
plot_selected_variables(mult_period_rf_1)



get_top_k_selected <- function(selected_set, k = 10) {
  # Flatten the list of selected vars per time point
  selected_flat <- unlist(selected_set$selected_vars)
  
  # Total number of time points
  total_periods <- length(selected_set$selected_vars)
  
  # Count frequency
  freq_table <- sort(table(selected_flat) / total_periods, decreasing = TRUE)
  
  # Return as data frame
  df <- data.frame(
    Variable = names(freq_table)[1:k],
    Frequency = as.numeric(freq_table[1:k])
  )
  return(df)
}

get_top_k_selected(SE_RF_1)
get_top_k_selected(SE_corr_1)


plot_factors_facets_custom <- function(set1, set2, set3, names) {
  library(ggplot2)
  library(zoo)
  
  make_df <- function(data, model_name) {
    df <- data$plot_forecasts
    df$Date <- as.Date(as.yearmon(as.character(df$date), "%Y%m"))
    df$model <- model_name
    df
  }
  
  combined_df <- rbind(
    make_df(set1, names[1]),
    make_df(set2, names[2]),
    make_df(set3, names[3])
  )
  
  ggplot(combined_df, aes(x = Date, y = factors, group = model)) +
    geom_line(linewidth = 0.72) +
    facet_wrap(~model, ncol = 1, scales = "fixed", strip.position = "right") +
    # Add model names as text inside the plotting area
    geom_text(data = combined_df %>%
                group_by(model) %>%
                summarise(x = min(Date) + 2300,  # Slightly inward from right end
                          y = max(factors, na.rm = TRUE) - 0.5,
                          label = unique(model)), 
              aes(x = x, y = y, label = label),
              inherit.aes = FALSE,
              fontface = "bold",
              hjust = 1,
              size = 3.5) +
    labs(x = "", y = "") +
    theme_classic(base_size = 9) +
    theme(
      plot.title    = element_text(hjust = 0, face = "bold", size = 9),
      axis.text.x   = element_text(size = 7),
      axis.text.y   = element_text(size = 7),
      panel.border  = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(colour = "gray80", linewidth = 0.3),
      panel.grid.minor.y = element_blank(),
      strip.text    = element_blank()
    )+
    scale_y_continuous(breaks = 0:6, limits = c(0, 6)) +
    scale_x_date(date_breaks = "5 years",
                 date_labels = "%Y",
                 expand = c(0, 0))
}


plot_factors_facets_custom(
  RF_1_104_with_macro,
  RF_1_128_with_macro,
  RF_1_soft_with_macro,
  names = c("Threshold 1.04", "Threshold 1.28", "Threshold soft"))

plot_factors_facets_custom(LASSO_good_15_104_with_macro,
                      LASSO_good_15_128_with_macro,
                      LASSO_good_15_soft_with_macro,
                      names = c("Threshold 1.04", "Threshold 1.28", "Threshold soft"))




compute_macro_selection_fractions <- function(selected_set, top_n = 20) {
  macro_list <- selected_set$plot_forecasts$macro_vars
  
  total_periods <- length(macro_list)

  all_macros <- unlist(macro_list)
  macro_freq <- sort(table(all_macros) / total_periods, decreasing = TRUE)

  freq_df <- data.frame(
    Variable = names(macro_freq),
    Fraction_Selected = as.numeric(macro_freq)
  )
  
  if (!is.null(top_n)) {
    freq_df <- head(freq_df, top_n)
  }
  
  return(freq_df)
}


compute_macro_selection_fractions(RF_1_104_with_macro)
compute_macro_selection_fractions(RF_1_128_with_macro)
compute_macro_selection_fractions(RF_1_soft_with_macro)

compute_macro_selection_fractions(LASSO_good_15_104_with_macro)
compute_macro_selection_fractions(LASSO_good_15_128_with_macro)
compute_macro_selection_fractions(LASSO_good_15_soft_with_macro)


install.packages("purrr")
install.packages("ggplot2")
install.packages("zoo")
install.packages("dplyr")
install.packages("ggrepel")
install.packages("patchwork")
install.packages("gtable", dependencies = TRUE)
library(purrr)
library(ggplot2)
library(zoo)
library(dplyr)
library(ggrepel)
library(gtable)
library(patchwork)

plot_macro_lines <- function(models, labels, title_text) {
  df_list <- lapply(seq_along(models), function(i) {
    df <- models[[i]]$plot_forecasts
    df$Date <- as.Date(zoo::as.yearmon(as.character(df$date), "%Y%m"))
    df$n_macros <- purrr::map_int(df$macro_vars, length)
    df$model <- labels[i]
    df
  })
  
  # Combine all to get consistent axis limits
  all_dates <- do.call(c, lapply(df_list, function(df) df$Date))
  all_counts <- do.call(c, lapply(df_list, function(df) df$n_macros))
  y_ticks <- pretty(c(0, max(all_counts)))
  y_max <- max(y_ticks)
  
  # Define line types
  lty_set <- c("solid", "dashed", "dotted")[1:length(models)]
  
  # Define x ticks
  x_ticks <- seq(min(all_dates), max(all_dates), by = "5 years")
  x_labels <- format(x_ticks, "%Y")
  
  # Set up plot
  plot(df_list[[1]]$Date, df_list[[1]]$n_macros, type = "n",
       ylim = c(0, y_max), xlim = range(all_dates),
       xaxs = "i", yaxs = "i",
       xlab = "", ylab = "",
       xaxt = "n", yaxt = "n")
  
  # Add horizontal grid lines only
  abline(h = y_ticks, col = "gray80", lty = "dotted", lwd = 0.5)
  
  # Add the lines for each model
  for (i in seq_along(df_list)) {
    lines(df_list[[i]]$Date, df_list[[i]]$n_macros,
          lty = lty_set[i], lwd = 2, col = "black")
  }
  
  # Axes
  axis(1, at = x_ticks, labels = x_labels, tck = 0.02)
  axis(2, at = y_ticks, las = 1)
  box()
  
  # Title
  title(main = title_text, font.main = 1, line = 1, cex.main = 1)
  
  # In-plot labels
  for (i in seq_along(df_list)) {
    last_row <- df_list[[i]][nrow(df_list[[i]]), ]
    text(x = last_row$Date + 150, y = last_row$n_macros,
         labels = labels[i], pos = 4, cex = 0.8)
  }
}





par(mfrow = c(2, 1), mar = c(4, 4, 2, 2))

# Top: 2 models
plot_macro_lines(
  models = list(RF_1_104_with_macro, RF_1_128_with_macro),
  labels = c("Threshold 1.04", "Threshold 1.28"),
  title_text = "RF with 1 financial variable"
)

# Bottom: 1 model
plot_macro_lines(
  models = list(LASSO_good_15_104_with_macro,LASSO_good_15_128_with_macro),
  labels = c("Threshold 1.04", "Threshold 1.28"),
  title_text = "LASSO with 15 financial variables"
)

par(mfrow = c(1, 1))  # reset layout

