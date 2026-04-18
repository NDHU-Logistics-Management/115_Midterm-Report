library(dplyr)
library(tidyr)
library(data.table)
library(stringr)
install.packages("openxlsx")
library(openxlsx)

df_112 <- read.csv("C:/Users/Angela/Desktop/運籌/公路客運2023.csv") %>%
  mutate(年份 = 112)
df_113 <- read.csv("C:/Users/Angela/Desktop/運籌/公路客運2024.csv") %>%
  mutate(年份 = 113)

df_114 <- read.csv("C:/Users/Angela/Desktop/運籌/公路客運2025.csv") %>%
  mutate(年份 = 114)


df_all <- bind_rows(df_112, df_113, df_114)
summary(df_all)

# 路線分類 --------

unique_routes <- unique(df_all$搭乘路線名稱)
print(sort(unique_routes))

route_type <- list(
  hualien = list(
    coast = c(
      1129, 1132, 1136, 1140,1145, 8101, 8102, 8105, 8119
      #8103,8107, 8110, 8111, 8113,, 8120  
    ), #井字部分為有出現在資料裡的路線，但不在"花蓮交通e點通"的所列路線中
    valley = c(
      1121, 1122, 1123, 1128, 1130, 1135, 1137, 1139, 1142, 1143,
      8161,8173
      #8109, 8115, 8117, 8128, 8129, 8130, 8131, 8132, 8135, 8136,
      #8137, 8138, 8150, 8151, 8152, 8153, 8156, 8157, 8158, 
      #8163, 8165, 8166, 8167, 8168, 8170, 8171, 8172, 8178,
      #8180,8122
    ),
    cross = c(
      1125, 1126, 1133, 1141, 8181
      #8125
    )
  )
)

route_map <- data.frame(
  搭乘路線名稱 = as.character(c(
    route_type$hualien$coast,
    route_type$hualien$valley,
    route_type$hualien$cross
  )),
  路線類別 = c(
    rep("海岸線", length(route_type$hualien$coast)),
    rep("縱谷線", length(route_type$hualien$valley)),
    rep("山海線", length(route_type$hualien$cross))
  ),stringsAsFactors = FALSE  
)

df_all$搭乘路線名稱 <- as.character(df_all$搭乘路線名稱)

if ("路線類別" %in% names(df_all)) {
  df_all$路線類別 <- NULL
}
df_all$搭乘路線名稱 <- as.character(df_all$搭乘路線名稱)
route_map$搭乘路線名稱 <- as.character(route_map$搭乘路線名稱)

df_all <- left_join(df_all, route_map, by = "搭乘路線名稱")


#------------ 運量分析-----------------
df_hualien <- df_all %>%
  filter(!is.na(路線類別)) %>%
  mutate(
    路線類別 = factor(路線類別, levels = c("海岸線", "縱谷線", "山海線"))
  )

mytable <- table(df_hualien$年份, df_hualien$路線類別)
roadbus_table <- addmargins(mytable, margin = 2)
colnames(roadbus_table)[colnames(roadbus_table) == "Sum"] <- "總運量"



#畫圖
path <- "C:/Users/Angela/Desktop/運籌/112~114年花蓮縣公路客運運量趨勢分析/"
if (!dir.exists(path)) {
  dir.create(path, recursive = TRUE)
}
file_name <- "112年至114年花蓮縣公路客運年總運量折線圖.png"

png(
  filename = paste0(path, file_name),
  width = 15,height = 5,units = "in",res = 300
)

windowsFonts(msjh = windowsFont("Microsoft JhengHei"))
df <- as.data.frame.matrix(roadbus_table)

years <- as.numeric(rownames(df))
n1 <- df[, "總運量"]
n2 <- df[, "海岸線"]
n3 <- df[, "縱谷線"]
n4 <- df[, "山海線"]

par(family = "msjh",mar = c(5, 6, 4, 12))
color <- gray.colors(ncol(df))
plot(
  years, n1,
  type = "o", pch = 16, lwd = 2, col = color[1],
  ylim = c(min(df) * 0.9 , max(df) * 1.1),
  xlim = c(min(years) -0.15 ,max(years) + 0.15),
  xlab = "年",ylab = "搭乘次數",
  main = " ",
  cex.main = 2,
  cex.lab = 2,
  cex.axis = 1.5,
  cex = 1.5,
  xaxt = "n", yaxt = "n", bty = "n")

axis(1, at = years, labels = years,cex.axis = 1.5)
grid() # 網格線

title(main = "112年至114年花蓮縣公路客運年總運量折線圖",
      cex.main = 2,,adj = 0 )
# 其他線
lines(years, n2, type="o", pch=16, lwd=2, col = color[2],
      cex.main = 2,cex.lab = 2,cex.axis = 1.5,cex = 1.5)
lines(years, n3, type="o", pch=16, lwd=2, col = color[3],
      cex.main = 2,cex.lab = 2,cex.axis = 1.5,cex = 1.5)
lines(years, n4, type="o", pch=16, lwd=2, col = color[4],
      cex.main = 2,cex.lab = 2,cex.axis = 1.5,cex = 1.5)

text(years, n1 + max(n1) * 0.02,
     labels = format(n1, big.mark = ",")
     ,pos = 3, col = "black", cex = 1.5, xpd = TRUE)

legend("topright",
       inset = c(-0.18, 0),
       legend = c("總運量","海岸線","縱谷線","山海線"),
       col = color,
       lwd = 2, pch = 16,bty = "n",xpd = TRUE,cex = 1.5)

dev.off()

percent_table <- rbind(
  t(round(prop.table(mytable, 1) * 100, 1)),
  總運量 = 100
)
percent_table


