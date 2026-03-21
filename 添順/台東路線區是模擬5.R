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
         inset = c(-0.12, 0),
         xpd = TRUE,
         cex = 1.5
  )
}

# 繪圖 (在 R中呈現)
 volumebyyearfig(roadbus_table)

 check_path <- function(path) {
   dir.create(path, recursive = TRUE, showWarnings = FALSE)
 }
 
 # 儲存折線圖
 path <- "img/line_plot/"
 check_path(path)
 main <- paste0(min(years) - 1911, "年", "至", max(years) - 1911, "年臺東縣公車年總運量")
 png(filename = paste0(path, main, ".png"),
     width = 15, height = 5, units = "in", res = 300, family = "kai")
 volumebyyearfig(roadbus_table)
 dev.off()
 
 ##製作統計表
 # 取出各路線各年運量
 result <- as.data.frame.matrix(mytable)
 result
 
 
 # 計算各路線各年佔比
 prop_table <- prop.table(mytable, margin = 2) * 100
 round(prop_table, 1)
 rowSums(mytable)
 
 ##票種分析(A：普通身分(含 TPASS)；B：學生身分；C：優待身分C01：敬老優待、 C02：愛心優待、C09：其他優待（如軍人卡、警察卡、孩童卡…等）； D：員工身分； X：無法區別身分)
 data1 <- data1[!is.na(data1$路線類別), ] #確認數據只採納台東(含花東)公路客運
 table(data1$票種類型)  #先找出TAPSS的數據
 total_1 <- data1[data1$票種類型 == 1, ]
 table(total_1$持卡身分) # 持卡身分的分布

 data1$年 <- as.integer(substring(data1$`資料代表日期(yyyy-MM-dd)`, 1, 4))
 
 data1 <- data1 %>%
   mutate(
     大票種 = case_when(
       票種類型 == 4     ~ "TPASS月票",
       持卡身分 == "A"   ~ "普通票",
       持卡身分 == "B"   ~ "學生票",
       持卡身分 == "C01" ~ "敬老票",
       持卡身分 == "C02" ~ "愛心票",
       持卡身分 == "C09" ~ "其他優待票",
       持卡身分 == "X"   ~ "無法辨識",
       TRUE              ~ "其他"
     )
   )
 
 # 再建立統計表
 temp <- table(data1$年 - 1911, data1$大票種)
 ticket_table <- addmargins(temp, margin = 2)
 
 #做圖
 tab <- ticket_table[rownames(ticket_table) %in% c("112", "113", "114"), ][, -ncol(ticket_table)]
 tab 
 ticket_hist <- function(tab, yr) {
   color <- gray.colors(ncol(tab))
   row_vec <- as.numeric(unlist(tab[yr, ]))
   names(row_vec) <- colnames(tab)
   counts <- sort(row_vec, decreasing = TRUE)
   counts_no0 <- counts[counts > 0]
   
   par(family = "kai", mar = c(5, 6, 4, 3)) # 字體與圖寬
   
   bp <- barplot(
     height = counts_no0,
     col = color,
     ylab = "數量",
     las = 1,
     cex.main = 2,
     cex.lab = 2,
     cex.axis = 1.5,
     cex = 1.5,
     yaxt = "n",
     main = "",
     ylim = c(0, max(counts_no0) * 1.1)
   )
   
   title(main = paste0(yr, "年臺東縣公路客運搭車票種"), cex.main = 2, adj = 0)
   
   # x 軸
   # axis(side = 1, at = 1:length(df_ticket), labels = c(names(df_ticket)), cex.axis = 1.5)
   
   # y 軸
   axis(side = 2, labels = FALSE, las = 1, cex.axis = 1.5, line = -1)
   
   text(
     x = bp,
     y = counts_no0,
     labels = counts_no0,
     pos = 3,
     cex = 1.5
   )
 }
 
 for (yr in rownames(tab)) {
   # 儲存圖片
   path <- paste0("images/barplot/")
   check_path(path)
   img_filename <- paste0(yr, "年臺東縣公路客運搭車票種")
   png(filename = paste0(path, "/", img_filename, ".png"), width = 13.79, height = 8.25, units = "in", res = 300, family = "Kai")
   ticket_hist(tab, yr)
   dev.off()
 }
 