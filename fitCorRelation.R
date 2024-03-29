suppressMessages(library(sf))
suppressMessages(library(dplyr))
suppressMessages(library(purrr))
suppressMessages(library(ggplot2))
suppressMessages(library(units))
source("NUSABird/2023Release_Nor/Script/global/loadRoutesDistance.R")

count<-0
#######探究皮尔逊系数的两两相关性################
sub_dirs <- list.dirs(workflow_dir, recursive = TRUE, full.names = TRUE)
for(sub_dir in sub_dirs){
  if (!file.exists(file.path(sub_dir, "overall_stats_p.RData"))) next
  load(file = file.path(sub_dir,"overall_stats_p.RData"))
  pairwise_AOU_corr_pearson%>%
    purrr::map(function (AOU_corr_list){
      #获取在AOU_corr_list中选取的索引和数据，筛数据就需要这一步
      sig_indices <- which(upper.tri(AOU_corr_list$P), arr.ind = TRUE)
      sig_r_values <- AOU_corr_list$r[sig_indices]
      #而对应的routeID就是列名和行名
      rownames_aou <- rownames(AOU_corr_list$P)
      colnames_aou <- colnames(AOU_corr_list$P)
      #两个端点的routeID索引和对应距离
      routeID_indices <- cbind(rownames_aou[sig_indices[,1]],
                              colnames_aou[sig_indices[,2]])
      distance <- logn_distance[routeID_indices]
      ###加入两个点的坐标，由于id按顺序来的，所以可以直接用索引
      xy1_data <- routes_info_with_id[routeID_indices[,1], c("Longitude", "Latitude")]
      xy2_data <- routes_info_with_id[routeID_indices[,2], c("Longitude", "Latitude")]

      fit_data <- data.frame(sig_r_values = sig_r_values, distance = distance,
                            start_x = xy1_data$Longitude, start_y = xy1_data$Latitude,
                            end_x = xy2_data$Longitude, end_y = xy2_data$Latitude)
      #######线性拟合############
      fit_lm <- lm(sig_r_values ~ distance, data = fit_data)
      # 绘制散点图
      p <- ggplot(fit_data) +
        geom_point(aes(x = distance, y = sig_r_values)) +
        labs(x = "Distance", y = "r_values", title = "Scatterplot of r_values vs Distance") +
        geom_abline(aes(intercept = coef(fit_lm)[1], slope = coef(fit_lm)[2]), color = "red")
      ggsave(paste0(workflow_dir, "/pics/all_fit/", count, ".png"), p,
              width = 10, height = 10, units = "in", dpi = 300)
      # print(summary(fit_lm))
      ###########两两绘制空间相关性####################
      draw_data <- fit_data %>%
        dplyr::select(start_x, start_y, end_x, end_y, sig_r_values)%>%
        filter(abs(sig_r_values) >0.5)
      draw_data$sig_r_values<-abs(draw_data$sig_r_values)

      sp_pic<-ggplot2::ggplot(draw_data) +
        # ggplot2::geom_sf() +  # 绘制基础图层
        ggplot2::geom_segment(aes(x = start_x, y = start_y, xend = end_x, yend = end_y,
                          color = sig_r_values),
                      size = 1) + # 绘制连线,根据 sig_r_values 着色
        ggplot2::scale_color_gradient(low = "blue", high = "red") + # 设置色带范围
        ggplot2::labs(color = "sig_r_values") # 设置颜色图例标题
        # print(sp_pic)
        ggsave(paste0(workflow_dir,"/pics/all_fit_sp/",count,".png"),sp_pic,
                width = 10, height = 10, units = "in", dpi = 300)
        count<<-count+1
        print(count)
        print(fit_lm$coefficients)
        return(fit_lm)
    })
}

