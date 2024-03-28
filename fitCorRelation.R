suppressMessages(library(sf))
suppressMessages(library(dplyr))
suppressMessages(library(purrr))
source("NUSABird/2023Release_Nor/Script/global/globalPath.R")

# 读取数据
routes_info_with_id <-read.csv(routes_info_with_id_path)
log10_distance <- routes_info_with_id%>%
  sf::st_as_sf(coords = c("Longitude","Latitude"),crs = 4326)%>%
  sf::st_distance()%>%
  log(base = 25)

rownames(log10_distance) <- colnames(log10_distance) <- 1:nrow(log10_distance)

# 对于数据中存在显著相关关系的，参与线性拟合计算。
sub_dirs <- list.dirs(workflow_dir, recursive = TRUE, full.names = TRUE)

for(sub_dir in sub_dirs){
  if (!file.exists(file.path(sub_dir, "overall_stats_p.RData"))) next
  load(file = file.path(sub_dir,"overall_stats_p.RData"))
  pairwise_AOU_corr_pearson%>%
    purrr::map(function (AOU_corr_list){
      # # 选择显著相关的数据
      # sig_indices <- which(AOU_corr_list$P < 0.05
      #                       & upper.tri(AOU_corr_list$P), arr.ind = TRUE)
      sig_indices <- which(upper.tri(AOU_corr_list$P), arr.ind = TRUE)

      sig_r_values <- AOU_corr_list$r[sig_indices]

      rownames_aou <- rownames(AOU_corr_list$P)
      colnames_aou <- colnames(AOU_corr_list$P)
      routeID_indices <- cbind(rownames_aou[sig_indices[,1]],
                              colnames_aou[sig_indices[,2]])
      distance <- log10_distance[routeID_indices]

      fit_data <- data.frame(sig_r_values = sig_r_values, distance = distance)

      fit_lm <- lm(sig_r_values ~ distance, data = fit_data)
      # 首先检查fit_data的结构
      # str(fit_data)
      # 绘制散点图
      plot(x = fit_data$distance,
          y = fit_data$sig_r_values,
          xlab = "Distance",
          ylab = "Sig_r_values",
          main = "Scatterplot of Sig_r_values vs Distance")

      # 可选:添加回归线
      abline(lm(sig_r_values ~ distance, data = fit_data), col = "red")
      print(summary(fit_lm))
    })
}
