source("NUSABird/2023Release_Nor/Script/global/globalPath.R")

# 读取数据
routes_info_with_id <-read.csv(routes_info_with_id_path)

logn_distance <-routes_info_with_id%>%
  sf::st_as_sf(coords = c("Longitude","Latitude"),crs = 4326) %>%
  sf::st_distance()%>%
  log(base = 10)

# 一切都是按顺序来的
rownames(logn_distance) <- colnames(logn_distance) <- 1:nrow(logn_distance)