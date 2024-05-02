suppressMessages(library(Hmisc))
suppressMessages(library(tseries))

source("NUSABird/2023Release_Nor/Script/global/loadFIData.R")

########################皮尔逊相关指数#############################
prepared_data_list %>%
  purrr::map2(names(.), function(filtered_itp_list, name) {
    pairwise_AOU_corr_pearson <- filtered_itp_list%>%
        purrr::map(., function(every_AOU) {
          # 用Hmisc包中的rcorr函数计算相关系数：皮尔逊指数[线性]
          every_AOU %>%
            purrr::map_dfr(., ~ .x)%>%
            sapply(function(x) round(x, 0))%>%
            Hmisc::rcorr(type = "pearson")
        })
      if(is_diff) save_filename=file.path(workflow_dir, name,diff_overall_stats_p_basename)
      else save_filename=file.path(workflow_dir, name,overall_stats_p_basename)
        # 保存整体统计数据
        save(pairwise_AOU_corr_pearson, file = save_filename)
  })
##################################滞后相关性#############################
    #两两计算滞后相关性
    # pairwise_AOU_corr_ccf <- filtered_itp_list%>%
    # purrr::map(., function(x) {
    #   ts_names <- names(x)
    #   # 获取所有时间序列对的组合
    #   combos <- combn(ts_names, 2, simplify = FALSE)
    #   ccf_1 <- t(sapply(combos, function(y) {
    #     ccf(x[[y[1]]], x[[y[2]]],type = "correlation", lag.max = 1, plot = FALSE)$acf[3]
    #     }))
    #   # 将结果合并为数据框
    #   corr_stats <- data.frame(
    #     ccf_1=ccf_1,
    #     routeID1 = unlist(lapply(combos, `[`, 1)),
    #     routeID2 = unlist(lapply(combos, `[`, 2))
    #     )})
    # save(pairwise_AOU_corr_ccf, file = file.path(paste(workflow_dir, name, sep = "/"),
    #                                             "overall_stats_ccf.RData"))
########################计算总体及种群波动指标#######################
########################但是时间序列非平稳，意义不大了#######################
    # # 计算每个时间序列的平均值和标准差
    # all_stats <- filtered_itp_list %>%
    #   purrr::map2(.,names(.), function(x,AOU) {
    #       sum_ts <- Reduce(`+`, x) # 子列表时间序列求和
    #       list(mean = mean(sum_ts), sd = sd(sum_ts),AOU=AOU) # 计算平均值和标准差
    #   })%>%
    #   do.call(rbind, .)
    # # 保存整体统计数据
    # write.csv(all_stats, file = file.path(paste(workflow_dir, name, sep = "/"), "all_stats.csv"))


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
        purrr::map(routes_BCR_groups, function(bcr_every) {
          indx_list<-which(names(every_AOU) %in% bcr_every$RouteID)
          matched_list<-every_AOU[indx_list]%>%
            compact()
          if(length(matched_list)<2)return(NULL)
            purrr::map_dfr(matched_list, ~ .x)%>%
            sapply(function(x) round(x, 0))%>%
            Hmisc::rcorr(type = "pearson")
        }) %>%
        compact()
      }
      )
    if(is_diff) save_filename=file.path(workflow_dir, name,diff_BCR_inner_cor_p_basename)
    else save_filename=file.path(workflow_dir, name,BCR_inner_cor_p_basename)
    # 保存BCR相关性数据
    save(BCR_inner_cor_p, file = save_filename)
  }
  )