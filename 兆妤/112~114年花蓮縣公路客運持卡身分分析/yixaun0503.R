library(dplyr)
library(tidyr)
library(openxlsx)

# ---------------- 讀取資料 ----------------
setwd("D:/Github/115_Midterm-Report")
load("兆妤/df_hualien.RData")

# ---------------- 定義票種類型 ----------------
ticket_order <- c(
    "普通(含TPASS)", "敬老優待", "學生",
    "愛心優待", "其他優待", "員工", "無法區別"
)

temp1 <- df_hualien %>%
    mutate(
        持卡身分分類 = case_when(
            持卡身分 == "A" ~ "普通(含TPASS)",
            持卡身分 == "B" ~ "學生",
            持卡身分 == "C01" ~ "敬老優待",
            持卡身分 == "C02" ~ "愛心優待",
            持卡身分 == "C09" ~ "其他優待",
            持卡身分 == "D" ~ "員工",
            持卡身分 == "X" ~ "無法區別",
            TRUE ~ NA_character_
        ),
        持卡身分分類 = factor(持卡身分分類, levels = ticket_order)
    ) %>%
    filter(!is.na(持卡身分分類))

# ---------------- 計算人數 ----------------
table1 <- table(temp1$持卡身分分類, temp1$ROCyear)
count_table <- addmargins(table1, margin = 1)
rownames(count_table)[rownames(count_table) == "Sum"] <- "總運量"

# ---------------- 計算比例 ----------------
table2 <- prop.table(table1, 2)
prop_table <- addmargins(table2, margin = 1)

# ---------------- 輸出 Excel 表格 ----------------
path <- "兆妤/112~114年花蓮縣公路客運持卡身分分析"
openxlsx::write.xlsx(
    list(
        "人數表" = count_table,
        "比例表" = prop_table
    ),
    file = file.path(path, "112~114年花蓮縣公路客運持卡身分統計表.xlsx"),
    rowNames = FALSE
)

# ---------------- 長條圖 ----------------
if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
}
title <- "114年花蓮縣公路客運持卡分析長條圖"
file_name <- paste0(title, ".png")
color <- gray.colors(length(ticket_order))

png(
    filename = file.path(path, file_name),
    width = 13.79,
    height = 8.25,
    units = "in",
    res = 300
)

prop_vec <- sort(table2[, "114"], decreasing = TRUE)[-7]
label_vec <- sprintf("%.1f%%", prop_vec * 100)

windowsFonts(msjh = windowsFont("Microsoft JhengHei"))
par(family = "msjh", mar = c(5, 3, 4, 3))

bp <- barplot(
    height = prop_vec,
    col = color,
    las = 1,
    ylim = c(0, max(prop_vec) * 1.1),
    main = title, cex.main = 2, adj = 0,
    cex.lab = 2,
    cex = 1.2,
    cex.axis = 1.5,
    xlab = "",
    ylab = "",
    yaxt = "n"
)

text(
    x = bp,
    y = prop_vec,
    labels = label_vec,
    pos = 3,
    cex = 1.5
)

dev.off()
