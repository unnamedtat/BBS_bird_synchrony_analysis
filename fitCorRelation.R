library(sf)
source("NUSABird/2023Release_Nor/Script/global/globalPath.R")

# 读取数据

routes_info_with_id <-read.csv(routes_info_with_id_path)
log10_distance <- routes_info_with_id%>%
  st_as_sf(coords = c("Longitude","Latitude"),crs = 4326)%>%
  st_distance()%>%
  log10()

# 对于数据中存在显著相关关系的，参与线性拟合计算。

