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

##運量分析

windowsFonts(kai = windowsFont("Microsoft JhengHei"))
# 讀入資料
data1 <- fread("C:/Users/gr704/OneDrive/桌面/運籌計畫/huaTTT_bus_data_23to25/huaTTT_bus_data_23to25/公路客運2025.csv")
data2 <- fread("C:/Users/gr704/OneDrive/桌面/運籌計畫/huaTTT_bus_data_23to25/huaTTT_bus_data_23to25/公路客運2024.csv")
data3 <- fread("C:/Users/gr704/OneDrive/桌面/運籌計畫/huaTTT_bus_data_23to25/huaTTT_bus_data_23to25/公路客運2023.csv")

data1 <- rbind(data1,data2,data3)

setwd("C:/Users/gr704/OneDrive/桌面/運籌計畫")


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
    rep("山海線", length(route_type$data1$cross)),
    rep("南迴線", length(route_type$data1$south)),
    rep("知本線", length(route_type$data1$zhiben))
  )
)

data1$搭乘路線名稱 <- as.character(data1$搭乘路線名稱)
data1 <- data1 %>%
  left_join(route_map, by = "搭乘路線名稱")

data1 <- data1[!is.na(data1$路線類別), ]

###持卡身分分析(A：普通身分(含 TPASS)；B：學生身分；C：優待身分C01：敬老優待、 C02：愛心優待、C09：其他優待（如軍人卡、警察卡、孩童卡…等）； D：員工身分； X：無法區別身分)

data1 <- data1 %>%
  mutate(
    持卡身分ALL = case_when(
      持卡身分 == "A"   ~ "普通票(含 TPASS)",
      持卡身分 == "B"   ~ "學生票",
      持卡身分 == "C01" ~ "敬老優待",
      持卡身分 == "C02" ~ "愛心優待",
      持卡身分 == "C09" ~ "其他優待",
      持卡身分 == "D"   ~ "員工身分",
      持卡身分 == "X"   ~ "無法區別身分",
      TRUE              ~ "其他"
    )
  )

data1$date <- as.Date(data1$`資料代表日期(yyyy-MM-dd)`)
  

# 篩出2025年1月至6月
temp_114 <- data1 %>%
  filter(year(date) == 2025, month(date) <= 6)

# 按月份統計總人次
mytable <- table(month(temp_114$date))
roadbus_table <- as.data.frame(mytable)
colnames(roadbus_table) <- c("月份", "搭乘次數")
roadbus_table



# 先加月份標籤欄位
roadbus_table$月份標籤 <- paste0("114", sprintf("%02d", as.integer(roadbus_table$月份)))

volumebymonthfig <- function(df) {
  par(family = "kai", mar = c(9, 9, 4, 2), mgp = c(7, 1, 0))
  
  plot(x = 1:nrow(df),              
       y = df$搭乘次數,
       type = "o",                  
       lwd = 2,                      
       pch = 16,                    
       col = "gray30",               
       xlab = "",
       ylab = "搭乘次數",
       ylim = c(min(df$搭乘次數) * 0.9, max(df$搭乘次數) * 1.1),
       cex.main = 2,                
       cex.lab = 2,                 
       cex.axis = 1.5,              
       cex = 1.5,                   
       xaxt = "n",                  
       yaxt = "n",                 
       bty = "n")         
  
  title(main = "114年1月至114年6月臺東縣公路客運總運量折線圖",
        cex.main = 2,
        adj = 0)             
  

  axis(side = 1,at = 1:nrow(df),labels = df$月份標籤,cex.axis = 1.5)
  axis(side = 2,at = axTicks(2),
       labels = format(axTicks(2), big.mark = ","),
       las = 1,
       cex.axis = 1.5)
  mtext("月份", side = 1, line = 5, cex = 2,adj = 0.45)
  grid()
}
# 繪圖 (在 R中呈現)
volumebymonthfig(roadbus_table)

check_path <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

# 儲存折線圖
path <- "img/line_plot/"
check_path(path)
main <-  "114年1月至114年6月臺東縣公路客運總運量折線圖"
png(filename = paste0(path, main, ".png"),
    width = 15, height = 5, units = "in", res = 300, family = "kai")
volumebymonthfig(roadbus_table)
dev.off()




# 統計各持卡身分總人次（直接從temp_114取，不用重新篩）
card_table <- temp_114 %>%
  group_by(持卡身分ALL) %>%
  summarise(搭乘次數 = n()) %>%
  arrange(desc(搭乘次數))

card_table
sum(card_table$搭乘次數)
sum(roadbus_table$搭乘次數)#檢查兩者範圍是否相同

#做圖
cardbyfig <- function(df) {
  color <- gray.colors(nrow(df))
  counts <- setNames(df$搭乘次數, df$持卡身分ALL)
  counts <- sort(counts, decreasing = TRUE)
  counts_no0 <- counts[counts > 0]
  
  pct <- round(counts_no0 / sum(counts_no0) * 100, 1)
  par(family = "kai", mar = c(5, 3, 4, 3)) # 字體與圖寬
  
  bp <- barplot(
    height = counts_no0,
    names.arg = names(counts_no0),
    col = color,
    las = 1,
    cex.main = 2,
    cex.lab = 2,
    cex.axis = 1.5,
    cex = 1.5,
    yaxt = "n",
    ylab = "",
    main = "",
    ylim = c(0, max(counts_no0) * 1.2),
    border = NA
  )
  
  title(main = "114年1月至114年6月臺東縣公路客運持卡身分長條圖", cex.main = 2, adj = 0)
  
  text(
    x = bp,
    y = counts_no0,
    labels = paste0(pct, "%"),
    pos = 3,
    cex = 1.5
  )
}
cardbyfig(card_table)

# 儲存圖片
path <- "images/barplot/"
check_path(path)
img_filename <- "114年1月至114年6月臺東縣公路客運持卡身分長條圖"
png(filename = paste0(path, img_filename, ".png"), width = 13.79, height = 8.25, units = "in", res = 300, family = "kai")
cardbyfig(card_table) 
dev.off()

  