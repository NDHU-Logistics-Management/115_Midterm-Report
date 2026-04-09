library(dplyr)
library(scales)

df_114 <- read.csv("C:/Users/Angela/Desktop/運籌/公路客運2025.csv") %>%
  mutate(
    日期 = as.Date(`資料代表日期.yyyy.MM.dd.`),
    年月 = format(日期, "%Y%m")
  ) %>%
  filter(年月 %in% c("202501", "202502", "202503", "202504", "202505", "202506")) %>%
  mutate(
    民國年月 = case_when(
      年月 == "202501" ~ "11401",
      年月 == "202502" ~ "11402",
      年月 == "202503" ~ "11403",
      年月 == "202504" ~ "11404",
      年月 == "202505" ~ "11405",
      年月 == "202506" ~ "11406"
    )
  )

# ------ 運量分析 ------

month <- c("11401", "11402", "11403", "11404", "11405", "11406")

df_total <- df_114 %>%
  count(民國年月, name = "搭乘次數")

count_vec <- setNames(df_total$搭乘次數, df_total$民國年月)
count_vec <- count_vec[month]
count_vec[is.na(count_vec)] <- 0

windowsFonts(msjh = windowsFont("Microsoft JhengHei"))
par(family = "msjh", mar = c(5, 7, 4, 2))

plot(
  x = 1:6,
  y = count_vec,
  type = "o",
  pch = 16,
  lwd = 2,
  col = "black",
  xaxt = "n",
  las = 1,
  xlab = "月份",
  ylab = "",
  ylim = c(min(count_vec )* 0.9, max(count_vec) * 1.1),
  cex.lab = 1.5,
  cex = 1.5,
  bty = "n",
  main = "")
title("114年1月至6月花蓮縣公路客運總運量折線圖", cex.main =2, adj =0)
axis(1, at = 1:6, labels = month)
mtext("搭乘次數",side = 2,line = 4.5,cex = 1.5)


# ------ 持卡身分 -------

ticket_order <- c("普通身分(含TPASS)", "敬老優待", "學生身分",
                  "愛心優待", "員工身分", "其他優待", "無法區別")

df_summary <- df_114 %>%
  mutate(
    持卡身分分類 = case_when(
      as.character(票種類型) == "4" ~ "普通身分(含TPASS)",
      持卡身分 == "A"   ~ "普通身分(含TPASS)",
      持卡身分 == "B"   ~ "學生身分",
      持卡身分 == "C01" ~ "敬老優待",
      持卡身分 == "C02" ~ "愛心優待",
      持卡身分 == "D"   ~ "員工身分",
      持卡身分 == "C09" ~ "其他優待",
      持卡身分 == "X"   ~ "無法區別",
      TRUE              ~ NA_character_
    )
  )%>%
  filter(!is.na(持卡身分分類)) %>%
  count(持卡身分分類, name = "人數") %>%
  mutate(
    百分比 = round(人數 / sum(人數) * 100, 1)
  ) %>%
  arrange(match(持卡身分分類, ticket_order))

total_row <- df_summary %>%
  summarise(
    持卡身分分類 = "總計",
    `人數` = sum(`人數`, na.rm = TRUE),
    `百分比` = 100 )

df_summary <- bind_rows(df_summary, total_row)
df_summary <- df_summary %>%
  mutate(
    百分比 = paste0(sprintf("%.1f", 百分比), "%"),
    人數 = format(人數, big.mark = ",", scientific = FALSE)
  )
df_summary

write.xlsx(df_summary,
           "C:/Users/Angela/Desktop/運籌/114上半年花蓮公路客運分析/
           114年1月至6月花蓮縣公路客運持卡身分統計表.xlsx",
           rowNames = FALSE)



df_identity <- df_114 %>%
  mutate(
    持卡身分名稱 = case_when(
      as.character(票種類型) == "4" ~ "普通身分(含TPASS)",
      持卡身分 == "A"   ~ "普通身分(含TPASS)",
      持卡身分 == "B"   ~ "學生身分",
      持卡身分 == "C01" ~ "敬老優待",
      持卡身分 == "C02" ~ "愛心優待",
      持卡身分 == "C09" ~ "其他優待",
      持卡身分 == "D"   ~ "員工身分",
      持卡身分 == "X"   ~ "無法區別",
      TRUE              ~ NA_character_
    )
  ) %>%
  filter(!is.na(持卡身分名稱)) %>%
  count(持卡身分名稱, name = "搭乘次數") %>%
  mutate(
    比例 = 搭乘次數 / sum(搭乘次數),
    比例標籤 = paste0(sprintf("%.1f", 比例 * 100), "%")
  ) %>%
  arrange(desc(搭乘次數))


count_vec <- df_identity$搭乘次數
names(count_vec) <- df_identity$持卡身分名稱
label_vec <- df_identity$比例標籤

par(family = "msjh", mar = c(5, 4, 4, 2))

bp <- barplot(
  height = count_vec,
  col = "gray60",
  border = NA,
  ylim = c(0, max(count_vec) * 1.15),
  main = " ",
  xlab = "",
  ylab = "",
  cex.names = 1.2,
  yaxt = "n"
)
title("114年1月至6月花蓮縣持卡身分長條圖",
      cex.main =2,adj = 0) 

text(
  x = bp,
  y = count_vec,
  labels = label_vec,
  pos = 3,
  cex = 1.1
)






