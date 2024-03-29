source("NUSABird/2023Release_Nor/Script/global/loadFIData.R")

routes_info_with_id <-read.csv(routes_info_with_id_path)
# #############在空间上划分网格计算相关性，由于数据不均匀效果不好##########################
# grid_resolution <- 20
# # 获取数据的经纬度边界范围
# lat_range <- range(routes_info_with_id$Latitude)
# lon_range <- range(routes_info_with_id$Longitude)
# # 创建等距经纬度网格
# lon_grid <- seq(lon_range[1], lon_range[2], by = grid_resolution)
# lat_grid <- seq(lat_range[1], lat_range[2], by = grid_resolution)
# # 为每个坐标点分配网格索引
# routes_info_with_id$lon_idx <- findInterval(routes_info_with_id$Longitude, lon_grid)
# routes_info_with_id$lat_idx <- findInterval(routes_info_with_id$Latitude, lat_grid)
# # 将网格索引合并为单一索引
# routes_info_with_id$grid_idx <- paste(routes_info_with_id$lon_idx,
#                                       routes_info_with_id$lat_idx,
#                                       sep = "_")
# routes_grid_groups <- split(routes_info_with_id, routes_info_with_id$grid_idx)

# #计算相关性
# grid_corr_list <- prepared_data_list %>%
#   purrr::map2(names(.), function(filtered_itp_list, name) {
#     pairwise_grid_corr_pearson <- filtered_itp_list %>%
#       purrr::map(., function(every_AOU) {
#         # 依次分组找到每个网格的数据
#         purrr::map(routes_grid_groups, function(grid_data) {
#           Hmisc::rcorr(every_AOU[[grid_data$RouteID ]],type = "pearson")
#           pause(0.1)
#         })
#         # 分组后的计算相关性
#
#         return(grid_corr)
#       })
#     # 保存网格相关性数据
#     save(pairwise_grid_corr_pearson, file = file.path(paste(workflow_dir, name, sep = "/"), "grid_corr_p.RData"))
#   })
#根据bcr计算相关性
routes_BCR_groups <- split(routes_info_with_id, routes_info_with_id$BCR)
prepared_data_list %>%
  purrr::map2(names(.), function(filtered_itp_list, name) {
    BCR_inner_cor_p<-filtered_itp_list %>%
      purrr::map(., function(every_AOU) {
        # 依次分组找到每个网格的数据
        purrr::map(routes_BCR_groups, function(grid_data) {
          matched_list<-every_AOU[grid_data$RouteID]%>%
            compact()
          if(length(matched_list)<2)return(NULL)
            purrr::map_dfr(matched_list, ~ .x)%>%
            sapply(function(x) round(x, 0))%>%
            Hmisc::rcorr(type = "pearson")
        }) %>%
        compact()
      }
      )
    # 保存BCR相关性数据
    save(BCR_inner_cor_p, file = file.path(paste(workflow_dir, name, sep = "/"), "BCR_inner_cor_p.RData"))
  }
  )