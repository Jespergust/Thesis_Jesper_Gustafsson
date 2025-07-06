#Load fin_data
install.packages("dplyr")
install.packages("zoo")
library(dplyr)
library(zoo)
library(tidyr)
library(dplyr)
library(ggplot2)

fin_corr_data <- fin_data %>%
  filter(date >= 198001 & date <= 202112) %>%
  mutate(
    excess_return = (ret - Rfree) * 100,
    year = floor(date / 100),  # Extract year from YYYYMM
    period = floor((year - min(year)) / 5) + 1  # Group into 5-year blocks
  ) %>%
  select(-date, -infl, -cay, -`i/k`, -pce, -govik,
         -crdstd, -ogap, -wtexas, -ndrbl, -gpce, -gip, -house, -ret, -Rfree,-retx) %>%
  select(where(~ !any(is.na(.))))

# Group by period and compute correlation
correlation_by_period <- fin_corr_data %>%
  group_by(period) %>%
  summarise(across(
    .cols = -excess_return,
    .fns = ~ cor(., excess_return, use = "complete.obs"),
    .names = "cor_{.col}"
  ))

# Optional: Add period labels
correlation_by_period <- correlation_by_period %>%
  mutate(period_label = paste0(1980 + 5 * (period - 1), "-", 1984 + 5 * (period - 1)))

# Drop 'period' and keep 'period_label' for readability
cor_long <- correlation_by_period %>%
  select(-period) %>%
  pivot_longer(
    cols = starts_with("cor_"),
    names_to = "variable",
    names_prefix = "cor_",
    values_to = "correlation"
  )

ggplot(cor_long, aes(x = period_label, y = variable, fill = correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       name = "Correlation") +
  labs(x = "", y = "", title = "") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )







mac_corr_data <- mac_data %>%
  filter(date >= 198001 & date <= 202112)  %>%
  select(where(~ !any(is.na(.))))

mac_corr_data <- mac_corr_data%>%
  mutate(excess_return = fin_corr_data$excess_return)

model_find_multicoll <- lm(excess_return ~ ., data = mac_corr_data[, !names(mac_corr_data) %in% "date"])
summary(model_find_multicoll)
install.packages("car")
library(car)
vif(model_find_multicoll)

excess_return_corrs <- mac_corr_data %>%
  select(where(is.numeric)) %>%  # Keep only numeric columns
  summarise(across(
    .cols = -excess_return,
    .fns = ~ cor(., excess_return, use = "complete.obs"),
    .names = "cor_{.col}"
  )) %>%
  pivot_longer(
    cols = everything(),
    names_prefix = "cor_",
    names_to = "variable",
    values_to = "correlation"
  )

excess_return_corrs_sorted <- excess_return_corrs %>%
  arrange(desc(correlation))

mac_var_matrix <- mac_corr_data %>%
  select(where(is.numeric)) %>%
  select(-excess_return) %>%
  cor(use = "complete.obs")



install.packages("corrplot")
library(corrplot)
corrplot::corrplot(mac_var_matrix,
                   method = "color",
                   col = colorRampPalette(c("blue", "white", "red"))(200),
                   type = "upper",
                   diag = FALSE,
                   tl.col = "black",
                   tl.cex = 0.4)  # adjust for many variables


fin_corr_matrix <- fin_corr_data %>%
  select(where(is.numeric)) %>%
  select(-excess_return,-year,-period,-price) %>%
  cor(use = "complete.obs")

install.packages("corrplot")
library(corrplot)
corrplot::corrplot(fin_corr_matrix,
                   method = "color",             # fill squares with color
                   col = colorRampPalette(c("blue", "white", "red"))(200),  # black (0) to white (1)
                   tl.cex = 0.7,
                   type = "upper",
                   diag = FALSE,
                   tl.col = "black") 
