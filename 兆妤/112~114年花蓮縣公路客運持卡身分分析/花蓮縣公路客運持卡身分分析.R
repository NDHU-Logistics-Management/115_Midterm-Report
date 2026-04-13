library(dplyr)
library(tidyr)
install.packages("openxlsx")
install.packages("tidyverse")
library(openxlsx)
library(tidyverse)


df_112 <- read.csv("C:/Users/Angela/Desktop/運籌/參考資料/公路客運2023.csv") %>%  mutate(年份 = 112)
df_113 <- read.csv("C:/Users/Angela/Desktop/運籌/參考資料/公路客運2024.csv") %>%  mutate(年份 = 113)
df_114 <- read.csv("C:/Users/Angela/Desktop/運籌/參考資料/公路客運2025.csv") %>%  mutate(年份 = 114)
df_all <- bind_rows(df_112, df_113, df_114)

# 持卡身分分析

#統計表

ticket_order <- c("普通身分(含TPASS)", "學生身分", "敬老優待",
                  "愛心優待", "員工身分", "其他優待", "無法區別")

df_summary <- df_all %>%
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
    ),
    持卡身分分類 = factor(持卡身分分類, levels = ticket_order)
  ) %>%
  filter(!is.na(持卡身分分類)) %>%
  count(年份, 持卡身分分類, name = "人數") %>%
  group_by(年份) %>%
  mutate(
    百分比 = round(人數 / sum(人數) * 100, 1)
  ) %>%
  ungroup()


df_table <- df_summary %>%
  mutate(百分比 = sprintf("%.1f%%", 百分比)) %>%
  pivot_wider(
    names_from = 年份,
    values_from = c(人數, 百分比),
    names_glue = "{年份}{.value}"
  ) %>%
  mutate(
    `113成長率` = ifelse(`112人數` == 0, NA, sprintf("%.1f%%", (`113人數` - `112人數`) / `112人數` * 100)),
    `114成長率` = ifelse(`113人數` == 0, NA, sprintf("%.1f%%", (`114人數` - `113人數`) / `113人數` * 100))
  ) %>%
  select(
    持卡身分分類,
    `112人數`, `112百分比`,
    `113人數`, `113百分比`,
    `114人數`, `114百分比`,
    `113成長率`, `114成長率`,
  )


total_row <- df_table %>%
  summarise(
    持卡身分分類 = "總計",
    `112人數` = sum(`112人數`, na.rm = TRUE),
    `112百分比` = "100%",
    `113人數` = sum(`113人數`, na.rm = TRUE),
    `113百分比` = "100%",
    `114人數` = sum(`114人數`, na.rm = TRUE),
    `114百分比` = "100%",
    `113成長率` = sprintf("%.1f%%",
                       (`113人數` - `112人數`) / `112人數` * 100
    ),
    `114成長率` = sprintf("%.1f%%",
                       (`114人數` - `113人數`) / `113人數` * 100
    )
  )

df_table <- bind_rows(df_table, total_row)

write.xlsx(df_table,
           "C:/Users/Angela/Desktop/運籌/112~114年花蓮縣公路客運持卡身分統計表.xlsx",
           rowNames = FALSE)

#長條圖

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

windowsFonts(msjh = windowsFont("Microsoft JhengHei"))
par(family = "msjh", mar = c(5, 4, 4, 2))

bp <- barplot(
  height = count_vec,
  col = "gray60",
  border = NA,
  ylim = c(0, max(count_vec) * 1.15),
  main = "114年花蓮縣公路客運持卡身分長條圖", cex.main = 2, adj = 0,
  cex.lab = 2,
  cex = 1.2,
  cex.axis = 1.5,
  xlab = "",
  ylab = "",
  yaxt = "n"
)

text(
  x = bp,
  y = count_vec,
  labels = label_vec,
  pos = 3,
  cex = 1.1
)

