install.packages("readxl")
install.packages("zoo")
install.packages("quantmod")
install.packages("tidyquant")
install.packages("dplyr")
install.packages("lubridate")
install.packages("e1071")
install.packages("stringr")
install.packages("forecast")
install.packages("openxlsx")
install.packages("readr")
install.packages("rstudioapi")
library(readxl)
library(zoo)
library(quantmod)
library(tidyquant)
library(dplyr)
library(lubridate)
library(e1071)
library(stringr)
library(forecast)
library(openxlsx)
library(readr)
library(rstudioapi)

summary_variable <- function(dataset) {
  x <- as.numeric(dataset)
  x <- na.omit(x)
  
  mean_x <- mean(x)
  sd_x   <- sd(x)
  skew_x <- skewness(x, type = 1)
  kurt_x <- kurtosis(x, type = 1) + 3
  ar1_x  <- Acf(x, lag.max = 1, plot = FALSE)$acf[2]
  
  return(tibble(
    mean = mean_x,
    stdev = sd_x,
    skew = skew_x,
    kurt = kurt_x,
    ar1 = ar1_x
  ))
}

table_1 <- tibble(
  name = character(),
  mean = numeric(),
  stdev = numeric(),
  skew = numeric(),
  kurt = numeric(),
  ar1 = numeric()
) 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#######################T-bill 3month###################

get_tbill_data <- function(name) {
  data_type <- name
  getSymbols(name, src = "FRED")
  ts_data <- get(name)
  
  data_type <- data.frame(date = index(ts_data), rf_rate = coredata(ts_data)[,1])
  
  data_type <- data_type %>%
    mutate(
      month = floor_date(date, "month"),
      rf_rate = as.numeric(rf_rate)
    ) %>%
    filter(!is.na(rf_rate)) %>%
    filter(month >= as.Date("1990-01-01") & month <= as.Date("2007-12-31"))
}

tbill <- get_tbill_data("TB3MS")

#######################################################

getSymbols("^SP500TR", src = "yahoo", from = as.Date("1990-01-01"), to = as.Date("2007-12-31"))

sp500_monthly <- to.monthly(SP500TR, indexAt = "lastof", OHLC = FALSE)
monthly_closes <- Cl(sp500_monthly) 

monthly_log_returns <- data.frame(
  date = as.Date(index(monthly_closes)),
  close = as.numeric(monthly_closes),
  log_return = c(NA, diff(log(as.numeric(monthly_closes)))) * 100
) 

monthly_log_returns$date <- tbill$date

returns_data <- left_join(monthly_log_returns, tbill, by = "date") %>%
  mutate(
    excess_return = log_return * 12 - rf_rate,
    yyyymm = as.numeric(format(date, "%Y%m"))
  ) %>%
  rename(month_1 = date) %>%
  select(month_1, excess_return)

table_1 <- bind_rows(
  table_1,
  summary_variable(returns_data$excess_return) %>% mutate(name = "S&P500") %>% select(name, everything())
)


#######################################################



#Load dataframe
VRP_dataset <- read_excel("VRP_data_1990_2023.xlsx")

#Select relevant data for plotting
IV <- VRP_dataset$IV
RV <- VRP_dataset$RV
VRP <- VRP_dataset$VRP
dates_VRP <- as.yearmon(paste(VRP_dataset$Year, VRP_dataset$Month), "%Y %m")
IV_RV <- c("IV", "RV")

##########SUMMARIZE THE VRP VARIABLES UNTIL 2007-12##########

table_1 <- bind_rows(
  table_1,
  summary_variable(IV[1:216]) %>% mutate(name = "IV") %>% select(name, everything())
)
table_1 <- bind_rows(
  table_1,
  summary_variable(RV[1:216]) %>% mutate(name = "RV") %>% select(name, everything())
)
table_1 <- bind_rows(
  table_1,
  summary_variable(VRP[1:216]) %>% mutate(name = "VRP") %>% select(name, everything())
)

VRP_IVminRV <- as.numeric(IV) - as.numeric(RV)
summary_variable(VRP_IVminRV[1:216])

###########SUMMARIZE PE RATIOS##########
correct_dates <- seq.Date(from = as.Date("1989-12-01"), to = as.Date("2025-04-01"), by = "month")
correct_dates <- rev(correct_dates) 

pe_ratios <- read_excel("pe_ratios.xlsx", sheet = 1, col_names = FALSE) %>%
  rename(pe = ...2) %>%
  mutate(
    dates = correct_dates,  
    log_pe = log(as.numeric(str_trim(pe)))
  ) %>%
  select(dates, log_pe)

pe_ratios <- pe_ratios[-nrow(pe_ratios), ]
pe_ratios <- pe_ratios[rev(seq_len(nrow(pe_ratios))), ]

table_1 <- bind_rows(
  table_1,
  summary_variable(pe_ratios$log_pe[1:216]) %>% mutate(name = "pe_ratio") %>% select(name, everything())
)


###########SUMMARIZE PD RATIOS##########
pd_ratios <- read_excel("pe_ratios.xlsx", sheet = 4, col_names = FALSE) %>%
  rename(pd = ...2) %>%
  mutate(
    dates = correct_dates,  
    log_pd = -log(as.numeric(str_remove(str_trim(pd), "%")) / 100)
  ) %>%
  select(dates, log_pd)

pd_ratios <- pd_ratios[-nrow(pd_ratios), ]
pd_ratios <- pd_ratios[rev(seq_len(nrow(pd_ratios))), ]

table_1 <- bind_rows(
  table_1,
  summary_variable(pd_ratios$log_pd[1:216]) %>% mutate(name = "pd_ratio") %>% select(name, everything())
)

############SUMMARIZE DFSP until 2007##############
getSymbols(c("BAA", "AAA"), src = "FRED")
dfsp <- merge(BAA, AAA) %>%
  data.frame(date = index(.), BAA = coredata(BAA), AAA = coredata(AAA)) %>%
  mutate(month = floor_date(date, "month"),
         dfsp = BAA - AAA) %>%
  filter(month >= as.Date("1990-01-01") & month <= as.Date("2007-12-31")) %>%
  select(month, dfsp)

table_1 <- bind_rows(
  table_1,
  summary_variable(dfsp$dfsp) %>% mutate(name = "dfsp") %>% select(name, everything())
)

############SUMMARIZE TMSP until 2007##############
getSymbols(c("GS10", "TB3MS"), src = "FRED")

tmsp_xts <- merge(GS10, TB3MS)
tmsp_df <- na.omit(tmsp_xts)  # Remove rows with any NAs

tmsp <- data.frame(
  date = index(tmsp_df),
  GS10 = coredata(tmsp_df)[, "GS10"],
  TB3MS = coredata(tmsp_df)[, "TB3MS"]
) %>%
  mutate(
    month = floor_date(date, "month"),
    tmsp = GS10 - TB3MS
  ) %>%
  filter(month >= as.Date("1990-01-01") & month <= as.Date("2024-12-31")) %>%
  select(month, tmsp)

table_1 <- bind_rows(
  table_1,
  summary_variable(tmsp$tmsp[1:216]) %>% mutate(name = "tmsp") %>% select(name, everything())
)


############SUMMARIZE TMSP until 2024##############
getSymbols(c("GS10", "TB3MS"), src = "FRED")

tmsp_xts <- merge(GS10, TB3MS)
tmsp_df <- na.omit(tmsp_xts)  # Remove rows with any NAs

tmsp <- data.frame(
  date = index(tmsp_df),
  GS10 = coredata(tmsp_df)[, "GS10"],
  TB3MS = coredata(tmsp_df)[, "TB3MS"]
) %>%
  mutate(
    month = floor_date(date, "month"),
    tmsp = GS10 - TB3MS
  ) %>%
  filter(month >= as.Date("1990-01-01") & month <= as.Date("2024-12-31")) %>%
  select(month, tmsp)

summary_variable(tmsp$tmsp)

############SUMMARIZE RREL until 2007##############
# From DTB1 (pre-2001)
getSymbols("DTB1", src = "FRED")

rrel_dtb1 <- data.frame(
  date = index(DTB1),
  rate = coredata(DTB1)
) %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarize(r1m = mean(DTB1, na.rm = TRUE)) %>%
  arrange(month) %>%
  mutate(
    trailing_12m = rollmeanr(r1m, 12, fill = NA),
    rrel = r1m - trailing_12m
  ) %>%
  filter(month >= as.Date("1990-01-01") & month <= as.Date("2007-12-31")) %>%
  filter(!is.na(rrel)) %>%
  select(month, rrel)

# From DGS1MO (post-2001)
getSymbols("DGS1MO", src = "FRED")

rrel_dgs1mo <- data.frame(
  date = index(DGS1MO),
  rate = coredata(DGS1MO)
) %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarize(r1m = mean(DGS1MO, na.rm = TRUE)) %>%
  arrange(month) %>%
  mutate(
    trailing_12m = rollmeanr(r1m, 12, fill = NA),
    rrel = r1m - trailing_12m
  ) %>%
  filter(month >= as.Date("1990-01-01") & month <= as.Date("2007-12-31")) %>%
  filter(!is.na(rrel)) %>%
  select(month, rrel)

getSymbols("TB3MS", src = "FRED")

rrel_tb3ms <- data.frame(
  date = index(TB3MS),
  rate = coredata(TB3MS)
) %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarize(r1m = mean(TB3MS, na.rm = TRUE)) %>%
  arrange(month) %>%
  mutate(
    trailing_12m = zoo::rollmeanr(r1m, 12, fill = NA),
    rrel = r1m - trailing_12m
  ) %>%
  # Restrict only to months missing from the DTB1 and DGS1MO series
  filter(month >= as.Date("2001-09-01") & month <= as.Date("2002-05-01")) %>%
  filter(!is.na(rrel)) %>%
  select(month, rrel)

rrel_combined <- bind_rows(rrel_dtb1, rrel_dgs1mo,rrel_tb3ms) %>%
  arrange(month)





#################################

table_1 <- bind_rows(
  table_1,
  summary_variable(rrel_combined$rrel[1:216]) %>% mutate(name = "rrel") %>% select(name, everything())
)

#######################CAY###################

cay_df <- read_excel("pe_ratios.xlsx", sheet = 5, col_names = FALSE) %>%
  rename(cay = ...1)

rep_times <- c(4, rep(3, nrow(cay_df) - 1))

# 1989Q4 -> 1990Q123
cay_monthly <- cay_df[rep(1:nrow(cay_df), times = rep_times), ,drop = FALSE]

start_date <- as.Date("1989-12-01")
monthly_dates <- seq.Date(from = start_date, by = "month", length.out = nrow(cay_monthly))

cay_monthly$month <- monthly_dates

cay_monthly <- cay_monthly[-1,]

cay_monthly <- cay_monthly %>%
  mutate(cay_std = as.numeric(cay)*170 - 1.95)

table_1 <- bind_rows(
  table_1,
  summary_variable(cay_monthly$cay_std[1:216]) %>% mutate(name = "cay") %>% select(name, everything())
)



regression_data <- data.frame(
  excess_return = returns_data$excess_return[2:216],
  VRP = VRP[1:215]
) 

write.xlsx(regression_data, 'regression_data.xlsx')

full_data <- data.frame(
  Excess = returns_data$excess_return[2:216],
  VRP = VRP[1:215],
  IV = IV[1:215],
  RV = RV[1:215],
  PE = pe_ratios$log_pe[1:215],
  PD = pd_ratios$log_pd[1:215],
  DFSP = dfsp$dfsp[1:215],
  TMSP = tmsp$tmsp[1:215],
  RREL = rrel_combined$rrel[1:215],
  CAY = cay_monthly$cay_std[1:215]
)

full_data[] <- lapply(full_data, function(x) as.numeric(as.character(x)))

write.xlsx(full_data, 'full_data.xlsx')

cor_data <- data.frame(
  Excess = returns_data$excess_return[2:216],
  VRP = VRP[2:216],
  IV = IV[2:216],
  RV = RV[2:216],
  PE = pe_ratios$log_pe[2:216],
  PD = pd_ratios$log_pd[2:216],
  DFSP = dfsp$dfsp[2:216],
  TMSP = tmsp$tmsp[2:216],
  RREL = rrel_combined$rrel[2:216],
  CAY = cay_monthly$cay_std[2:216]
)
cor_data[] <- lapply(cor_data, function(x) as.numeric(as.character(x)))

correlation_matrix <- cor(cor_data)

