library(dplyr)
library(tidyr)
library(openxlsx)
library(tidyverse)


df_112 <- read.csv("C:/Users/Angela/Desktop/運籌/公路客運2023.csv") %>%  mutate(年份 = 112)
df_113 <- read.csv("C:/Users/Angela/Desktop/運籌/公路客運2024.csv") %>%  mutate(年份 = 113)
df_114 <- read.csv("C:/Users/Angela/Desktop/運籌/公路客運2025.csv") %>%  mutate(年份 = 114)
df_all <- bind_rows(df_112, df_113, df_114)

#資料篩選
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


#定期票分析
df_pass <- df_hualien %>%
  mutate(
    定期票類型 = case_when(
      is.na(票種次類型) ~ "非定期票",
      票種次類型 == "#HUA-399" ~ "花蓮縣399",
      票種次類型 == "#HUA-199" ~ "花蓮縣199",
      票種次類型 == "#TTT-299" ~ "台東縣299",
      票種次類型 == "" ~ "非定期票",
      TRUE ~ "其他"
    )
  ) %>%
  filter(!is.na(定期票類型))

df_pass$定期票類型 <- factor(
  df_pass$定期票類型,
  levels = c("非定期票","花蓮縣199", "花蓮縣399", "台東縣299")
)

df_pass_summary <- df_pass %>%
  group_by(年份, 定期票類型) %>%
  summarise(人數 = n(), .groups = "drop") %>%
  complete(
    年份 = c(112, 113, 114),
    定期票類型 = c("非定期票","花蓮縣199", "花蓮縣399", "台東縣299"),
    fill = list(人數 = 0)
  ) %>%
  arrange(年份, 定期票類型)
print(df_pass_summary)

#統計表
df_pass_table <- df_pass_summary %>%
  pivot_wider(
    names_from = 年份,
    values_from = 人數,
    values_fill = 0
  )
colnames(df_pass_table) <- c("定期票類型", "112年", "113年", "114年")
print(df_pass_table)

df_pass_percent <- df_pass_summary %>%
  group_by(年份) %>%
  mutate(百分比 = round(人數 / sum(人數) * 100, 1)) %>%
  ungroup()

df_pass_percent_table <- df_pass_percent %>%
  mutate(百分比 = paste0(百分比, "%")) %>%
  select(年份, 定期票類型, 百分比) %>%
  pivot_wider(
    names_from = 年份,
    values_from = 百分比,
    values_fill = "0%"
  ) %>%
  rename(
    `112年` = `112`,
    `113年` = `113`,
    `114年` = `114`
  )
print(df_pass_percent_table)

df_count <- df_pass_table %>%
  mutate(統計類型 = "人數",
         across(`112年`:`114年`, as.character)
  )

df_percent <- df_pass_percent_table %>%
  mutate(統計類型 = "百分比")

df_table <- bind_rows(df_count, df_percent) %>%
  select(統計類型, 定期票類型, `112年`, `113年`, `114年`)

print(df_table)

write.xlsx(df_table,
           "C:/Users/Angela/Desktop/運籌/112~114年花蓮縣公路客運定期票統計表.xlsx",
           rowNames = FALSE)


#畫圖
path <- "C:/Users/Angela/Desktop/運籌/112~114年花蓮縣公路客運運量趨勢分析/"

if (!dir.exists(path)) {
  dir.create(path, recursive = TRUE)
}
file_name <- "114年花蓮縣公路客運定期票長條圖.png"
color <- gray.colors(nrow(tab))

png(
  filename = paste0(path, file_name),
  width = 13.79,
  height = 8.25,
  units = "in",
  res = 300
)

par(family = "msjh", mar = c(5, 6, 4, 3))
tab <- df_pass_summary %>%
  filter(年份 == 114) %>%
  mutate(百分比 = 人數 / sum(人數)) %>%
  arrange(desc(百分比))

bp <- barplot(
  tab$人數,
  names.arg = tab$定期票類型,
  col = color,
  border = NA,
  las = 1,
  cex.main = 2,
  cex.lab = 2,
  cex.axis = 1.5,
  cex.names = 1.5,
  yaxt = "n",
  main = "",
  ylim = c(0, max(tab$人數) * 1.15)
)

title(main = "114年花蓮縣公路客運定期票長條圖", cex.main = 2, adj = 0)

text(
  x = bp,
  y = tab$人數 + max(tab$人數) * 0.02,
  labels = paste0(formatC(tab$百分比 * 100, format = "f", digits = 1), "%"),
  pos = 3,
  cex = 1.5
)
dev.off()