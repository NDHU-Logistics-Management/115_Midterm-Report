# load libraries
library(data.table)

# location table
setwd("D:/Github/115_Midterm-Report/區域分析code")
tCityD <- read.csv2("臺東市區公車站點里程.csv", fileEncoding = "Big5", header = TRUE, sep = ",")
hCityD <- read.csv2("花蓮市區公車站點里程.csv", fileEncoding = "Big5", header = TRUE, sep = ",")
countryD <- read.csv2("公路客運站點里程.csv", fileEncoding = "Big5", header = TRUE, sep = ",")

# bus data
setwd("D:/huaTTT_bus_data_23to25")
countrydata1 <- fread("公路客運2023.csv")
countrydata2 <- fread("公路客運2024.csv")
countrydata3 <- fread("公路客運2025.csv")
tCitydata <- fread("臺東縣公車.csv")
hCitydata <- fread("花蓮縣公車.csv")

# merge location data
locD <- rbind(countryD, tCityD, hCityD)

# merge data
countrydata <- rbind(countrydata1, countrydata2, countrydata3)
all_data <- rbind(countrydata, tCitydata, hCitydata)

# check location column class
class(all_data$"上車站牌代碼")
class(locD$StopUID)

# check all_data$"上車站牌代碼" in locD$StopUID
table(all_data$"上車站牌代碼" %in% locD$StopUID)
table(all_data$"上車站牌代碼"[-which(all_data$"上車站牌代碼" == "-99")] %in% locD$StopUID)

#   FALSE    TRUE
#  151236 5530755

# check which "上車站牌代碼" are not in locD$StopUID
table(all_data$"上車站牌名稱"[!all_data$"上車站牌代碼" %in% locD$StopUID])

# check all_data$"下車站牌代碼" in locD$StopUID
table(all_data$"下車站牌代碼" %in% locD$StopUID)
table(all_data$"下車站牌代碼"[-which(all_data$"下車站牌代碼" == "-99")] %in% locD$StopUID)

#   FALSE    TRUE
#  183699 5426456

# check which "下車站牌代碼" are not in locD$StopUID
table(all_data$"下車站牌名稱"[!all_data$"下車站牌代碼" %in% locD$StopUID])

# check date range of data with "上下車站牌代碼" not in locD$StopUID
range(all_data$"資料代表日期(yyyy-MM-dd)"[!all_data$"上車站牌代碼"[-which(all_data$"上車站牌代碼" == "-99")] %in% locD$StopUID])
range(all_data$"資料代表日期(yyyy-MM-dd)"[!all_data$"下車站牌代碼"[-which(all_data$"下車站牌代碼" == "-99")] %in% locD$StopUID])
