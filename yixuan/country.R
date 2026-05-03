# load libraries
library(data.table)
library(dplyr)

# read data
setwd("D:/busData/huaTTT_bus_data_23to25")
countrydata1 <- fread("公路客運2022.csv")
countrydata2 <- fread("公路客運2023.csv")
countrydata3 <- fread("公路客運2024.csv")
countrydata4 <- fread("公路客運2025.csv")

# define analysis time range
anal_time_range <- c("2022-01-01", "2025-6-30")

# src routes list by county
setwd("D:/Github/115_Midterm-Report/yixuan")
source("route_list.R")

# split 2022 THB data by county
# 所屬業管機關
# "THB-VO15-1": 臺北區監理所
# "THB-VO14-2": 高雄區監理所
Hualien_THB_2022 <- countrydata1 %>%
    filter(所屬業管機關 == "THB-VO15-1") %>%
    select("資料代表日期(yyyy-MM-dd)") %>%
    rename(date = "資料代表日期(yyyy-MM-dd)")

Taitung_THB_2022 <- countrydata1 %>%
    filter(所屬業管機關 == "THB-VO14-2") %>%
    select("資料代表日期(yyyy-MM-dd)") %>%
    rename(date = "資料代表日期(yyyy-MM-dd)")

# split 2023-2025 THB data by county
temp <- rbind(countrydata2, countrydata3, countrydata4)
Hualien_THB_23_25 <- temp %>%
    filter(搭乘路線名稱 %in% routes_hualien_THB) %>%
    select("資料代表日期(yyyy-MM-dd)") %>%
    rename(date = "資料代表日期(yyyy-MM-dd)")

Taitung_THB_23_25 <- temp %>%
    filter(搭乘路線名稱 %in% routes_taitung_THB) %>%
    select("資料代表日期(yyyy-MM-dd)") %>%
    rename(date = "資料代表日期(yyyy-MM-dd)")

# combine 2022-2025 THB data by county
Hualien_THB <- rbind(Hualien_THB_2022, Hualien_THB_23_25)
Taitung_THB <- rbind(Taitung_THB_2022, Taitung_THB_23_25)

# extract year and aggregate total volume per year
temp <- Hualien_THB %>%
    filter(date >= anal_time_range[1] & date <= anal_time_range[2])
yearly <- temp[, .(volume = .N), by = .(year = year(as.vector(temp$date)))]
roc_year <- yearly$year - 1911
title <- "111年至114年上半年花蓮縣公路客運運量趨勢折線圖"

# plot
png(
    filename = paste0("D:/Github/115_Midterm-Report/yixuan/imgs/", title, ".png"),
    width = 10,
    height = 6,
    units = "in",
    res = 300
)

windowsFonts(msjh = windowsFont("Microsoft JhengHei"))
par(family = "msjh", mar = c(5, 6, 4, 6))


plot(
    x = roc_year,
    y = yearly$volume,
    type = "o",
    pch = 16,
    lwd = 2,
    col = "gray",
    xlab = "年份",
    ylab = "搭乘次數",
    ylim = c(min(yearly$volume) * 0.9, max(yearly$volume) * 1.3),
    xlim = c(min(roc_year) - 0.1, max(roc_year) + 0.1),
    cex.main = 2,
    cex.lab = 2,
    cex.axis = 1.5,
    xaxt = "n",
    yaxt = "n",
    bty = "n"
)

title(
    main = title,
    cex.main = 2,
    adj = 0
)

axis(1, at = roc_year, labels = c(roc_year[1:3], "114上半"), cex.axis = 1.5)


text(
    x = roc_year,
    y = yearly$volume + max(yearly$volume) * 0.02,
    labels = format(yearly$volume, big.mark = ","),
    pos = 3,
    cex = 1.5
)

grid()


dev.off()
