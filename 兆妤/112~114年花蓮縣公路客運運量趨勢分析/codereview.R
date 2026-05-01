library(dplyr)
library(tidyr)
library(data.table)
library(stringr)
library(openxlsx)

# ---------------- 讀取資料 ----------------
data_dir <- "D:/busData/huaTTT_bus_data_23to25/"
year_files <- list(
    "112" = "公路客運2023.csv",
    "113" = "公路客運2024.csv",
    "114" = "公路客運2025.csv"
)

df_all <- bind_rows(lapply(names(year_files), function(y) {
    read.csv(file.path(data_dir, year_files[[y]])) %>%
        mutate(年份 = as.integer(y))
}))

# ---------------- 路線分類 ----------------
# 8101, 8102, 8105 大部分服務範圍在台東，劃歸台東，故不列入
valley_routes <- c(
    1121, 1122, 1123, 1128, 1130, 1135,
    1137, 1139, 1142, 1143, 8161, 8173
)
route_lines <- list(
    海岸線 = c(1129, 1132, 1136, 1140, 1145, 8119),
    縱谷線 = valley_routes,
    山海線 = c(1125, 1126, 1133, 1141, 8181)
)

route_map <- bind_rows(lapply(names(route_lines), function(cat) {
    data.frame(
        搭乘路線名稱 = as.character(route_lines[[cat]]),
        路線類別 = cat,
        stringsAsFactors = FALSE
    )
}))

df_all$搭乘路線名稱 <- as.character(df_all$搭乘路線名稱)
df_all$路線類別 <- NULL
df_all <- left_join(df_all, route_map, by = "搭乘路線名稱")

# ---------------- 運量分析 ----------------
df_hualien <- df_all %>%
    filter(!is.na(路線類別)) %>%
    mutate(路線類別 = factor(路線類別, levels = c("海岸線", "縱谷線", "山海線")))

mytable <- table(df_hualien$年份, df_hualien$路線類別)
roadbus_table <- addmargins(mytable, margin = 2)
colnames(roadbus_table)[colnames(roadbus_table) == "Sum"] <- "總運量"

# ---------------- 繪圖 ----------------
out_dir <- "C:/Users/Angela/Desktop/運籌/112~114年花蓮縣公路客運運量趨勢分析/"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
out_file <- file.path(out_dir, "112年至114年花蓮縣公路客運年總運量折線圖.png")

png(filename = out_file, width = 15, height = 5, units = "in", res = 300)
windowsFonts(msjh = windowsFont("Microsoft JhengHei"))

df <- as.data.frame.matrix(roadbus_table)
years <- as.numeric(rownames(df))
series <- c("總運量", "海岸線", "縱谷線", "山海線")
colors <- gray.colors(length(series))

par(family = "msjh", mar = c(5, 6, 4, 12))
plot(
    years, df[, series[1]],
    type = "o", pch = 16, lwd = 2, col = colors[1],
    ylim = c(min(df) * 0.9, max(df) * 1.1),
    xlim = c(min(years) - 0.15, max(years) + 0.15),
    xlab = "年", ylab = "搭乘次數", main = " ",
    cex.lab = 2, cex.axis = 1.5, cex = 1.5,
    xaxt = "n", yaxt = "n", bty = "n"
)
axis(1, at = years, labels = years, cex.axis = 1.5)
grid()
title(main = "112年至114年花蓮縣公路客運年總運量折線圖", cex.main = 2, adj = 0)

for (i in seq_along(series)[-1]) {
    lines(years, df[, series[i]],
        type = "o", pch = 16, lwd = 2, col = colors[i], cex = 1.5
    )
}

total <- df[, "總運量"]
text(years, total + max(total) * 0.02,
    labels = format(total, big.mark = ","),
    pos = 3, col = "black", cex = 1.5, xpd = TRUE
)

legend("topright",
    inset = c(-0.18, 0),
    legend = series, col = colors,
    lwd = 2, pch = 16, bty = "n", xpd = TRUE, cex = 1.5
)

dev.off()

# ---------------- 百分比表 ----------------
percent_table <- rbind(
    t(round(prop.table(mytable, 1) * 100, 1)),
    總運量 = 100
)
