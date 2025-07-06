install.packages("readxl")
install.packages("glmnet")
install.packages("openxlsx")
install.packages("dplyr")
install.packages("tidyr")
library(readxl)
library(glmnet)
library(openxlsx)
library(dplyr)
library(tidyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

extension_data_monthly <- read.xlsx("Data2024.xlsx", sheet = 2)
monthly_dates <- extension_data_monthly$yyyymm
extension_data_monthly$date <- extension_data_monthly$yyyymm
extension_data_monthly <- extension_data_monthly[,colSums(is.na(extension_data_monthly))<nrow(extension_data_monthly)]


extension_data_quarterly <- read.xlsx("Data2024.xlsx", sheet = 3) 

extension_data_quarterly_monthly <- extension_data_quarterly[rep(1:nrow(extension_data_quarterly), each = 3), ]
extension_data_quarterly_monthly$date <- monthly_dates
extension_data_quarterly_monthly <- extension_data_quarterly_monthly[,colSums(is.na(extension_data_quarterly_monthly))<nrow(extension_data_monthly)]


#Select which columns were not already monthly
new_columns <- setdiff(names(extension_data_quarterly_monthly), names(extension_data_monthly))
new_columns <- setdiff(new_columns, "date")  
extension_data_quarterly_monthly <- extension_data_quarterly_monthly %>%
  select(date, all_of(new_columns))

combined_data <- left_join(extension_data_monthly, extension_data_quarterly_monthly, by = "date")


extension_data_yearly <- read.xlsx("Data2024.xlsx", sheet = 4)
extension_data_yearly_monthly <- extension_data_yearly[rep(1:nrow(extension_data_yearly), each = 12), ]
extension_data_yearly_monthly$date <- monthly_dates

#Select which columns were not already monthly or quarterly
new_columns <- setdiff(names(extension_data_yearly_monthly), names(combined_data))
new_columns <- setdiff(new_columns, "date") 
extension_data_yearly_monthly <- extension_data_yearly_monthly %>%
  select(date, all_of(new_columns))


full_extension_data <- left_join(combined_data, extension_data_yearly_monthly, by = "date") %>%
  arrange(date) %>%
  relocate(date) %>%
  select(-yyyymm,-yyyy,-yyyyq)

na_cols_to_add <- apply(full_extension_data,2, function(col) all(is.na(col)))
na_col_names <- names(na_cols_to_add[na_cols_to_add == TRUE])
print(na_col_names)

na_rows <- apply(full_extension_data, 1, function(row) all(is.na(row)))
last_NA <- min(full_extension_data$date[na_rows], na.rm = TRUE)
print(last_NA)

print(na_rows)

write.xlsx(full_extension_data, 'full_extension_data.xlsx')

#Clear data
rm(list = names(which(!unlist(eapply(.GlobalEnv, 
                                     \(x) inherits(x, what = "function"))))))
