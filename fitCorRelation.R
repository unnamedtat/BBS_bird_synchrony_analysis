library(sf)
source("NUSABird/2023Release_Nor/Script/global/global.R")

# 对于每个时间段的相关性系数，算出两两之间的距离
combos<-routes_info_with_id[,"RouteID"] %>%
      combn(., 2, simplify = FALSE)

log10_distance <- sapply(combos, function(RouteID) {
  p1 <- routes_info_with_id %>%
    filter(RouteID ==RouteID[1] )%>%
    with(st_point(c(Longitude, Latitude)))
  p2 <- routes_info_with_id %>%
    filter(RouteID ==RouteID[2] )%>%
    with(st_point(c(Longitude, Latitude)))
  st_distance(p1, p2) %>% log10()
  })

# 将结果合并为数据框
corr_stats <- data.frame(
  t = cors_test[,1],
  p_value = cors_test[,2],
  conf_intervalup = cors_test[,3],
  conf_intervaldown = cors_test[,4],
  cor_value = cors_test[,5],
  routeID1 = unlist(lapply(combos, `[`, 1)),
  routeID2 = unlist(lapply(combos, `[`, 2))
  )
# 对于数据中存在显著相关关系的，参与线性拟合计算。

