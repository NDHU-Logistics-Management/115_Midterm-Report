# library
install.packages("openxlsx")
install.packages("pbapply")
library(readr)
library(data.table)
library(dplyr)
library(lubridate)
library(tidyr)
library(purrr)
library(ggplot2)
library(readxl)

library(pbapply)
library(stringr)

library(openxlsx)
library(hms)

## 運量分析

windowsFonts(kai = windowsFont("Microsoft JhengHei"))
# 讀入資料
data1 <- fread("C:/Users/gr704/OneDrive/桌面/運籌計畫/huaTTT_bus_data_23to25/huaTTT_bus_data_23to25/公路客運2025.csv")
data2 <- fread("C:/Users/gr704/OneDrive/桌面/運籌計畫/huaTTT_bus_data_23to25/huaTTT_bus_data_23to25/公路客運2024.csv")
data3 <- fread("C:/Users/gr704/OneDrive/桌面/運籌計畫/huaTTT_bus_data_23to25/huaTTT_bus_data_23to25/公路客運2023.csv")

data1 <- rbind(data1, data2, data3)
unique(data1$搭乘路線名稱)
setwd("C:/Users/gr704/OneDrive/桌面/運籌計畫")

route_type <- list()
route_type$data1$coast <- c(1145, 8101, 8102, 8103, 8105, 8107, 8109, 8119, 8120, 8122, 8125)

route_type$data1$valley <- c(8110, 8117, 8161, 8163, 8165, 8166, 8167, 8168, 8170, 8171, 8172, 8173, 8178)
route_type$data1$cross <- c(8111, 8113, 8181)
route_type$data1$south <- c(8132, 8135, 8136, 8137, 8138, 8150, 8151, 8152)

route_type$data1$zhiben <- c(8115, 8128, 8129, 8130, 8131, 8180)

route_map <- data.frame(
    搭乘路線名稱 = as.character(c(
        route_type$data1$coast,
        route_type$data1$valley,
        route_type$data1$cross,
        route_type$data1$south,
        route_type$data1$zhiben
    )),
    路線類別 = c(
        rep("海岸線", length(route_type$data1$coast)),
        rep("縱谷線", length(route_type$data1$valley)),
        rep("山海線", length(route_type$data1$cross)),
        rep("南迴線", length(route_type$data1$south)),
        rep("知本線", length(route_type$data1$zhiben))
    )
)

data1$搭乘路線名稱 <- as.character(data1$搭乘路線名稱)
data1 <- data1 %>%
    left_join(route_map, by = "搭乘路線名稱")
