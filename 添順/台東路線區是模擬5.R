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

windowsFonts(kai = windowsFont("Microsoft JhengHei"))
# 讀入資料
data1 <- fread("C:/Users/gr704/OneDrive/桌面/運籌計畫/huaTTT_bus_data_23to25/huaTTT_bus_data_23to25/公路客運2025.csv")
data2 <- fread("C:/Users/gr704/OneDrive/桌面/運籌計畫/huaTTT_bus_data_23to25/huaTTT_bus_data_23to25/公路客運2024.csv")
data3 <- fread("C:/Users/gr704/OneDrive/桌面/運籌計畫/huaTTT_bus_data_23to25/huaTTT_bus_data_23to25/公路客運2023.csv")

data1 <- rbind(data1,data2,data3)



route_type = list()
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
    rep("知本線", length(route_type$data1$cross)),
    rep("南迴線", length(route_type$data1$south)),
    rep("山海線", length(route_type$data1$zhiben))
  )
)

data1$搭乘路線名稱 <- as.character(data1$搭乘路線名稱)
data1 <- data1 %>%
  left_join(route_map, by = "搭乘路線名稱")

temp <- data.frame(
  date = data1$`資料代表日期(yyyy-MM-dd)`,
  路線類別 = data1$路線類別)
temp <- temp[!is.na(temp$路線類別), ]

mytable <- table(year(temp$date), temp$路線類別)



roadbus_temp <- addmargins(mytable, margin = 2)
roadbus_table <- roadbus_temp
years <- 2023:2025

volumebyyearfig <- function(df){
  color <- gray.colors(ncol(df))
  idx_sort <- names(sort(df["2024",], decreasing = TRUE))
  n <- df[,idx_sort[1]]
  
  par(family = "Kai", mar = c(5, 6, 4, 10))  # 字體與圖寬
  
  plot(x = years,
       y = n,
       type = "o",
       lwd = 2,
       pch = 16,
       col = color[1],
       xlab = "年",
       ylab = "搭乘次數",
       ylim = c(min(df) * 0.9 , max(df) * 1.1),
       cex.main = 2,
       cex.lab = 2,
       cex.axis = 1.5,
       cex = 1.5,
       xaxt = "n",
       yaxt = "n",
       bty = "n")
  
  lines(x = years,
        y = df[,idx_sort[2]],
        type = "o",
        lwd = 2,
        pch = 16,
        col = color[2],
        cex.main = 2,
        cex.lab = 2,
        cex.axis = 1.5,
        cex = 1.5,
  )
  
  lines(x = years,
        y = df[,idx_sort[3]],
        type = "o",
        lwd = 2,
        pch = 16,
        col = color[3],
        cex.main = 2,
        cex.lab = 2,
        cex.axis = 1.5,
        cex = 1.5,
  )
  
  lines(x = years,
        y = df[,idx_sort[4]],
        type = "o",
        lwd = 2,
        pch = 16,
        col = color[4],
        cex.main = 2,
        cex.lab = 2,
        cex.axis = 1.5,
        cex = 1.5,
  )
  
  lines(x = years,
        y = df[,idx_sort[5]],
        type = "o",
        lwd = 2,
        pch = 16,
        col = color[5],
        cex.main = 2,
        cex.lab = 2,
        cex.axis = 1.5,
        cex = 1.5,
  )
  
  lines(x = years,
        y = df[,idx_sort[6]],
        type = "o",
        lwd = 2,
        pch = 16,
        col = color[6],
        cex.main = 2,
        cex.lab = 2,
        cex.axis = 1.5,
        cex = 1.5,
  )
  
  title(main = paste0(min(years) - 1911, '年', '至', max(years) - 1911,
                      '年臺東縣公路客運年總運量折線圖'),
        cex.main = 2, 
        adj = 0)
  
  # x 軸
  axis(side = 1, at = years, labels = c(years - 1911), cex.axis = 1.5)
  
  # y 軸
  # axis(side = 2, labels = FALSE, las = 1, tcl = -0.3, line = 0.5)
  
  grid()  # 網格線
  
  text(x = years,
       y = n + max(n) * 0.02,
       labels = n,
       pos = 3,            # 文字顯示在點上方
       cex = 1.5,          # 文字大小
       col = "black"
  )
  
  # 圖例
  legend("topright", 
         legend = c("總運量", idx_sort[-1]), 
         col = color, 
         lwd = 2, 
         pch = 16,
         bty = "n",
         inset = c(-0.2, 0),
         xpd = TRUE,
         cex = 1.2
  )
}

# 繪圖 (在 R中呈現)
 volumebyyearfig(roadbus_table)
