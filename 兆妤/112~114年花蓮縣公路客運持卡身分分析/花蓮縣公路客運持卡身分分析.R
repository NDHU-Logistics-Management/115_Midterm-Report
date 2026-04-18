library(dplyr)
library(tidyr)
install.packages("openxlsx")
install.packages("tidyverse")
library(openxlsx)
library(tidyverse)


df_112 <- read.csv("C:/Users/Angela/Desktop/運籌/公路客運2023.csv") %>%  mutate(年份 = 112)
df_113 <- read.csv("C:/Users/Angela/Desktop/運籌/公路客運2024.csv") %>%  mutate(年份 = 113)
df_114 <- read.csv("C:/Users/Angela/Desktop/運籌/公路客運2025.csv") %>%  mutate(年份 = 114)
df_all <- bind_rows(df_112, df_113, df_114)

# 持卡身分分析
unique_routes <- unique(df_all$搭乘路線名稱)
print(sort(unique_routes))

route_type <- list(
  hualien = list(
    coast = c(
      1129, 1132, 1136, 1140, 1145, 8101, 8102, 8105, 8119
    ),
    valley = c(
      1121, 1122, 1123, 1128, 1130, 1135, 1137, 1139, 1142, 1143,
      8161, 8173
    ),
    cross = c(
      1125, 1126, 1133, 1141, 8181
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
  ),
  stringsAsFactors = FALSE
)

df_all$搭乘路線名稱 <- as.character(df_all$搭乘路線名稱)
route_map$搭乘路線名稱 <- as.character(route_map$搭乘路線名稱)

if ("路線類別" %in% names(df_all)) {
  df_all$路線類別 <- NULL
}

df_all <- left_join(df_all, route_map, by = "搭乘路線名稱")

df_hualien <- df_all %>%
  filter(!is.na(路線類別)) %>%
  mutate(
    路線類別 = factor(路線類別, levels = c("海岸線", "縱谷線", "山海線"))
  )

#統計表

ticket_order <- c("普通身分(含TPASS)", "學生身分", "敬老優待",
                  "愛心優待", "員工身分", "其他優待", "無法區別")

df_summary <- df_hualien %>%
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
  complete(
    年份 = c(112, 113, 114),
    持卡身分分類 = factor(ticket_order, levels = ticket_order),
    fill = list(人數 = 0)
  ) %>%
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
path <- "C:/Users/Angela/Desktop/運籌/112~114年花蓮縣公路客運運量趨勢分析/"

if (!dir.exists(path)) {
  dir.create(path, recursive = TRUE)
}
file_name <- "114年花蓮縣公路客運持卡分析長條圖.png"
color <- gray.colors(nrow(tab))

png(
  filename = paste0(path, file_name),
  width = 13.79,
  height = 8.25,
  units = "in",
  res = 300
)

df_identity <- df_hualien%>%
  filter(年份 == 114) %>%
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
  arrange(desc(比例))

count_vec <- df_identity$搭乘次數
names(count_vec) <- df_identity$持卡身分名稱
label_vec <- df_identity$比例標籤

windowsFonts(msjh = windowsFont("Microsoft JhengHei"))
pct <- round(count_vec / sum(count_vec) * 100, 1)
par(family = "msjh", mar = c(5, 3, 4, 3))

bp <- barplot(
  height = count_vec,
  col = "gray60",
  border = NA,
  las = 1,
  ylim = c(0, max(count_vec) * 1.1),
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
  cex = 1.5
)

dev.off()
