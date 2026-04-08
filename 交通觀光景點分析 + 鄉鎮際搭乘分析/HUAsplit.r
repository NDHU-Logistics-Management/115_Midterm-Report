library(dplyr)

df_2023 <- read.csv("C:/Users/Angela/Desktop/運籌/公路客運2023.csv") %>% mutate(年份 = 112)
df_2024 <- read.csv("C:/Users/Angela/Desktop/運籌/公路客運2024.csv") %>% mutate(年份 = 113)
df_2025 <- read.csv("C:/Users/Angela/Desktop/運籌/公路客運2025.csv") %>% mutate(年份 = 114)

df_all <- bind_rows(df_2023, df_2024, df_2025)

# 路線分類 --------
unique_routes <- unique(df_all$搭乘路線名稱)
print(sort(unique_routes))

route_type <- list(
    hualien = list(
        coast = c(
            1129, 1132, 1136, 1140, 1145, 8101, 8102, 8105, 8119
            # 8103,8107, 8110, 8111, 8113,, 8120  #井字部分為有出現在資料裡的路線，但不在"花蓮交通e點通"的所列路線中
        ),
        valley = c(
            1121, 1122, 1123, 1128, 1130, 1135, 1137, 1139, 1142, 1143,
            8161, 8173
            # 8109, 8115, 8117, 8128, 8129, 8130, 8131, 8132, 8135, 8136,
            # 8137, 8138, 8150, 8151, 8152, 8153, 8156, 8157, 8158, 8163,
            # 8165, 8166, 8167, 8168, 8170, 8171, 8172, 8178, 8180, 8122
        ),
        cross = c(
            1125, 1126, 1133, 1141, 8181
            # 8125
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
    ), stringsAsFactors = FALSE
)

df_all$搭乘路線名稱 <- as.character(df_all$搭乘路線名稱)

if ("路線類別" %in% names(df_all)) {
    df_all$路線類別 <- NULL
}
df_all <- left_join(
    df_all, route_map,
    by = "搭乘路線名稱"
)
