library(dplyr)
library(openxlsx)

# ---------------- 讀取資料 ----------------
setwd("D:/Github/115_Midterm-Report")
load("兆妤/df_hualien.RData")

# ---------------- 前處理 ----------------
type_order <- c("非定期票", "花蓮縣199", "花蓮縣399", "台東縣299")

temp1 <- df_hualien %>%
    mutate(
        定期票類型 = case_when(
            is.na(票種次類型) ~ "非定期票",
            票種次類型 == "#HUA-399" ~ "花蓮縣399",
            票種次類型 == "#HUA-199" ~ "花蓮縣199",
            票種次類型 == "#TTT-299" ~ "台東縣299",
            票種次類型 == "" ~ "非定期票",
            TRUE ~ "其他"
        ),
        定期票類型 = factor(定期票類型, levels = type_order)
    ) %>%
    filter(!is.na(定期票類型))

# ---------------- 計算人數 ----------------
table1 <- table(temp1$定期票類型, temp1$ROCyear)
count_table <- addmargins(table1, margin = 1)
rownames(count_table)[rownames(count_table) == "Sum"] <- "總運量"

# ---------------- 計算比例 ----------------
table2 <- prop.table(table1, 2)
prop_table <- addmargins(table2, margin = 1)

# ---------------- 輸出 Excel 表格 ----------------
path <- "兆妤/112~114年花蓮縣公路客運定期票分析"
openxlsx::write.xlsx(
    list(
        "人數表" = count_table,
        "比例表" = prop_table
    ),
    file = file.path(path, "112~114年花蓮縣公路客運定期票.xlsx"),
    rowNames = FALSE
)

# ---------------- 畫圖 ----------------
if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
}
file_name <- "114年花蓮縣公路客運定期票長條圖.png"
color <- gray.colors(length(type_order))

png(
    filename = file.path(path, file_name),
    width = 13.79,
    height = 8.25,
    units = "in",
    res = 300
)

prop_vec <- sort(table2[, "114"], decreasing = TRUE)
label_vec <- sprintf("%.1f%%", prop_vec * 100)

windowsFonts(msjh = windowsFont("Microsoft JhengHei"))
par(family = "msjh", mar = c(5, 6, 4, 3))

bp <- barplot(
    prop_vec,
    names.arg = names(prop_vec),
    col = color,
    border = NA,
    las = 1,
    cex.main = 2,
    cex.lab = 2,
    cex.axis = 1.5,
    cex.names = 1.5,
    yaxt = "n",
    main = "",
    ylim = c(0, max(prop_vec) * 1.15)
)

title(main = "114年花蓮縣公路客運定期票長條圖", cex.main = 2, adj = 0)

text(
    x = bp,
    y = prop_vec + max(prop_vec) * 0.02,
    labels = label_vec,
    pos = 3,
    cex = 1.5
)
dev.off()
