library(dplyr)

# ---------------- 讀取資料 ----------------
data_dir <- "D:/busData/huaTTT_bus_data_23to25/"
year_files <- c(
    "112" = "公路客運2023.csv",
    "113" = "公路客運2024.csv",
    "114" = "公路客運2025.csv"
)

df_all <- bind_rows(lapply(names(year_files), function(y) {
    read.csv(file.path(data_dir, year_files[[y]]))
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
route_map <- data.frame(
    搭乘路線名稱 = as.character(unlist(route_lines)),
    路線類別 = rep(names(route_lines), lengths(route_lines))
)

df_all <- df_all %>%
    mutate(搭乘路線名稱 = as.character(搭乘路線名稱)) %>%
    left_join(route_map, by = "搭乘路線名稱")

df_hualien <- df_all %>%
    filter(!is.na(路線類別)) %>%
    rename(date = 資料代表日期.yyyy.MM.dd.) %>%
    mutate(
        路線類別 = factor(路線類別, levels = names(route_lines)),
        ROCyear = lubridate::year(date) - 1911
    )

save(df_hualien, file = "D:/Github/115_Midterm-Report/兆妤/df_hualien.RData")
