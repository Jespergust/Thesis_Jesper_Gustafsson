install.packages("fredr")
install.packages("tidyverse")
install.packages("here")
install.packages("purrr")
install.packages("tidyr")
install.packages("openxlsx")
library(fredr)
library(tidyverse)
library(here)
library(purrr)
library(tidyr)
library(openxlsx)

load(here("data", "fredmd_description.RData")) 
fredr_set_key("437a33b5097338284c7280769850dec2")

tickers <- fredmd_description$fred
failed_tickers <- c() 

start_date <- as.Date("1960-01-01")

macroeconomic_data <- map_dfr(tickers, function(tick) {
  tryCatch({
    fredr(series_id = tick, observation_start = start_date) %>%
      select(date, value) %>%
      mutate(ticker = tick)
  }, error = function(e) {
    message(paste("Skipping", tick, ":", e$message))
    failed_tickers <<- c(failed_tickers,tick)
    return(NULL)
  })
})

#we now remove the columns which contain NA's but we could possibly also use them in periods in which they are known!
macroeconomic_data_full <- macroeconomic_data %>%
  pivot_wider(names_from = ticker, values_from = value) %>%
  mutate(date = format(date, "%Y%m")) %>%
  filter(date != 202505 & date != 202504) %>%
  select(-AMBSL,-MZMSL,-ACOGNO,-TWEXMMTH)

write.xlsx(macroeconomic_data_full, 'macvar_data.xlsx')


