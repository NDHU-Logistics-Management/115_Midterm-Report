library(dplyr)
library(lubridate)

df_114 <- read.csv("D:/busData/huaTTT_bus_data_23to25/公路客運2025.csv")

# 臺東公路客運
routes_taitung_THB <- c(
    "1145",
    "309",
    "8101",
    "8102",
    "8103",
    "8105",
    "8107",
    "8109",
    "8110",
    "8111",
    "8113",
    "8115",
    "8117",
    "8119",
    "8120",
    "8122",
    "8125",
    "8128",
    "8129",
    "8130",
    "8131",
    "8132",
    "8135",
    "8136",
    "8137",
    "8138",
    "8150",
    "8151",
    "8152",
    "8153",
    "8156",
    "8157",
    "8158",
    "8161",
    "8163",
    "8165",
    "8166",
    "8167",
    "8168",
    "8170",
    "8171",
    "8172",
    "8173",
    "8178",
    "8180",
    "8181"
)

Taitung_THB <- df_114 %>%
    filter(搭乘路線名稱 %in% routes_taitung_THB)

# 篩出2025年1月至6月
temp_114 <- Taitung_THB %>%
    filter(year(資料代表日期.yyyy.MM.dd.) == 2025, month(資料代表日期.yyyy.MM.dd.) <= 6)

# 按月份統計總人次
mytable <- table(month(temp_114$資料代表日期.yyyy.MM.dd.))
roadbus_table <- as.data.frame(mytable)
colnames(roadbus_table) <- c("月份", "搭乘次數")

# 先加月份標籤欄位
roadbus_table$月份標籤 <- paste0("114", sprintf("%02d", as.integer(roadbus_table$月份)))


windowsFonts(msjh = windowsFont("Microsoft JhengHei"))

volumebymonthfig <- function(df) {
    par(family = "msjh", mar = c(7, 9, 4, 2), mgp = c(7, 1, 0))

    plot(
        x = 1:nrow(df),
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
        bty = "n"
    )

    title(
        main = "114年1月至114年6月臺東縣公路客運總運量折線圖",
        cex.main = 2,
        adj = 0
    )


    axis(side = 1, at = 1:nrow(df), labels = df$月份標籤, cex.axis = 1.5)
    axis(
        side = 2, at = axTicks(2),
        labels = format(axTicks(2), big.mark = ","),
        las = 1,
        cex.axis = 1.5
    )
    mtext("月份", side = 1, line = 3, cex = 2, adj = 0.45)
    grid()
}
# 繪圖 (在 R中呈現)
volumebymonthfig(roadbus_table)

check_path <- function(path) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

# 儲存折線圖
setwd("添順/四月報告")
path <- "img/line_plot/"
check_path(path)
main <- "114年1月至114年6月臺東縣公路客運總運量折線圖"
png(
    filename = paste0(path, main, ".png"),
    width = 15, height = 5, units = "in", res = 300, family = "msjh"
)
volumebymonthfig(roadbus_table)
dev.off()

# 持卡身分分析

temp_114 <- temp_114 %>%
    mutate(
        持卡身分ALL = case_when(
            持卡身分 == "A" ~ "普通(含 TPASS)",
            持卡身分 == "B" ~ "學生",
            持卡身分 == "C01" ~ "敬老優待",
            持卡身分 == "C02" ~ "愛心優待",
            持卡身分 == "C09" ~ "其他優待",
            持卡身分 == "D" ~ "員工",
            持卡身分 == "X" ~ "無法區別",
            TRUE ~ "其他"
        )
    )

# 統計各持卡身分總人次（直接從 temp_114 取，不用重新篩）
card_table <- temp_114 %>%
    group_by(持卡身分ALL) %>%
    summarise(搭乘次數 = n()) %>%
    arrange(desc(搭乘次數))

sum(card_table$搭乘次數)
sum(roadbus_table$搭乘次數) # 檢查兩者範圍是否相同

# 做圖
cardbyfig <- function(df) {
    color <- gray.colors(nrow(df))
    counts <- setNames(df$搭乘次數, df$持卡身分ALL)
    counts <- sort(counts, decreasing = TRUE)
    counts_no0 <- counts[counts > 0]

    pct <- round(counts_no0 / sum(counts_no0) * 100, 1)
    par(family = "msjh", mar = c(5, 3, 4, 3)) # 字體與圖寬

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

# 儲存圖片
path <- "img/barplot/"
check_path(path)
img_filename <- "114年1月至114年6月臺東縣公路客運持卡身分長條圖"
png(filename = paste0(path, img_filename, ".png"), width = 13.79, height = 8.25, units = "in", res = 300, family = "msjh")
cardbyfig(card_table)
dev.off()
