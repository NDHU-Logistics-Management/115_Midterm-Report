library(dplyr)
library(openxlsx)

# ---------------- 讀取資料 ----------------
setwd("D:/Github/115_Midterm-Report")
load("兆妤/df_hualien.RData")

# ---------------- 年運量分析 ----------------
mytable <- table(df_hualien$路線類別, df_hualien$ROCyear)
roadbus_table <- addmargins(mytable, margin = 1)
rownames(roadbus_table)[rownames(roadbus_table) == "Sum"] <- "總運量"

# ---------------- 比例分析 ----------------
prop_table <- addmargins(prop.table(mytable, 2), margin = 1)

# ---------------- 輸出 Excel 表格 ----------------
path <- "兆妤/112~114年花蓮縣公路客運運量趨勢分析"
openxlsx::write.xlsx(
    list(
        "人數表" = roadbus_table,
        "比例表" = prop_table
    ),
    file = file.path(path, "112~114年花蓮縣公路客運運量趨勢分析.xlsx"),
    rowNames = FALSE
)

# ---------------- 繪圖 ----------------
if (!dir.exists(path)) dir.create(path, recursive = TRUE)
out_file <- file.path(path, "112年至114年花蓮縣公路客運年總運量折線圖.png")

series <- c("總運量", "海岸線", "縱谷線", "山海線")
mat <- roadbus_table[, series]
years <- as.numeric(rownames(mat)) - 1911
line_colors <- gray.colors(length(series))

windowsFonts(msjh = windowsFont("Microsoft JhengHei"))
png(filename = out_file, width = 15, height = 5, units = "in", res = 300)
par(family = "msjh", mar = c(5, 6, 4, 12))

matplot(years, mat,
    type = "o", pch = 16, lwd = 2, col = line_colors,
    ylim = c(min(mat) * 0.9, max(mat) * 1.1),
    xlim = c(min(years) - 0.15, max(years) + 0.15),
    xlab = "年", ylab = "搭乘次數",
    cex.lab = 2, cex.axis = 1.5, cex = 1.5, lty = 1,
    xaxt = "n", yaxt = "n", bty = "n"
)
axis(1, at = years, labels = years, cex.axis = 1.5)
grid()
title(
    main = "112年至114年花蓮縣公路客運年總運量折線圖",
    cex.main = 2, adj = 0
)

total <- mat[, "總運量"]
text(years, total + max(total) * 0.02,
    labels = format(total, big.mark = ","),
    pos = 3, cex = 1.5, xpd = TRUE
)
legend("topright",
    inset = c(-0.18, 0),
    legend = series, col = line_colors,
    lwd = 2, pch = 16, bty = "n", xpd = TRUE, cex = 1.5
)

dev.off()
