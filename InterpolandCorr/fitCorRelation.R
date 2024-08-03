suppressMessages(library(sf))
suppressMessages(library(dplyr))
suppressMessages(library(purrr))
suppressMessages(library(ggplot2))
suppressMessages(library(units))
suppressMessages(library(scales))

source("NUSABird/2023Release_Nor/Script/global/globalPath.R")
source("NUSABird/2023Release_Nor/Script/global/globalPlots.R")

# 读取数据
routes_info_with_id <-read.csv(routes_info_with_id_path)

logn_distance <-routes_info_with_id%>%
  sf::st_as_sf(coords = c("Longitude","Latitude"),crs = 4326) %>%
  sf::st_distance()%>%
  units::drop_units()%>%
  log(base = 10)
  # %>%rescale(to = c(0, 1))
# 一切都是按顺序来的
rownames(logn_distance) <- colnames(logn_distance) <- 1:nrow(logn_distance)

#######探究皮尔逊系数的两两相关性################
sub_dirs <- list.dirs(workflow_dir, recursive = TRUE, full.names = TRUE)

getFlmNetwork <- function(corr_list, dir, sp_dir,years, AOU,BCR=0 ) {
  if (!dir.exists(file.path(dir,years))) dir.create(file.path(dir,years))
    if (!dir.exists(file.path(sp_dir,years))) dir.create(file.path(sp_dir,years))
  #获取在AOU_corr_list中选取的索引和数据，筛数据就需要这一步
  sig_indices <- which(upper.tri(corr_list$P), arr.ind = TRUE)
  sig_r_values <- corr_list$r[sig_indices]
  #而对应的routeID就是列名和行名
  rownames_aou <- rownames(corr_list$P)
  colnames_aou <- colnames(corr_list$P)
  #两个端点的routeID索引和对应距离
  routeID_indices <- cbind(rownames_aou[sig_indices[, 1]],
                          colnames_aou[sig_indices[, 2]])
  distance <- logn_distance[routeID_indices]
  ###加入两个点的坐标，由于id按顺序来的，所以可以直接用索引
  xy1_data <- routes_info_with_id[routeID_indices[, 1], c("Longitude", "Latitude")]
  xy2_data <- routes_info_with_id[routeID_indices[, 2], c("Longitude", "Latitude")]

  fit_data <- data.frame(sig_r_values = sig_r_values, distance = distance,
                        start_x = xy1_data$Longitude, start_y = xy1_data$Latitude,
                        end_x = xy2_data$Longitude, end_y = xy2_data$Latitude)
  fit_lm <- lm(sig_r_values ~ distance, data = fit_data)

##########绘制拟合图####################
# # 计算注释文本绝对位置
#   x_range <- range(fit_data$distance, na.rm = TRUE)
#   y_range <- range(fit_data$sig_r_values, na.rm = TRUE)
#   x_pos1 <- (x_range[2]-x_range[1]) * 0.15 + x_range[1]
#   y_pos1 <- (y_range[2]-y_range[1]) * 0.15 + y_range[1]
#   x_pos2 <- (x_range[2]-x_range[1]) * 0.15 + x_range[1]
#   y_pos2 <- (y_range[2]-y_range[1]) * 0.25 + y_range[1]
#   # 绘制散点图
#   p <- ggplot2::ggplot(fit_data) +
#     ggplot2::geom_point(aes(x = distance, y = sig_r_values)) +
#     ggplot2::labs(x = "distance(log10)", y = "coefficients", title = "") +
#     ggplot2::geom_abline(aes(intercept = coef(fit_lm)[1], slope = coef(fit_lm)[2]),
#                         color = "red")+
#     annotate("text", x = x_pos1, y = y_pos1,
#             label = paste("y =", round(coef(fit_lm)[1], 4),
#                           "+", round(coef(fit_lm)[2], 4), "* x"),
#             family="Times New Roman", size = 5) +
#     annotate("text", x = x_pos2, y = y_pos2,
#             label = paste("R^2 =", round(summary(fit_lm)$r.squared, 5),
#                           ", P =", round(summary(fit_lm)$coefficients[2, 4], 8)),
#             family="Times New Roman", size = 5)+
#     theme_bar+
#     theme(plot.background = element_rect(fill = "white",colour = "transparent"))
#
#   fit_lm_path<-file.path(dir,years, paste0(AOU,if(BCR!=0) paste0("_",BCR) else "",".png"))
#
#   ggplot2::ggsave(fit_lm_path, p, width = plots_corr_width, height = plots_corr_height,
#                   units = "in", dpi = 300)
  # print(summary(fit_lm))
  ###########两两绘制空间相关性####################

  # draw_data <- fit_data %>%
  #   dplyr::select(start_x, start_y, end_x, end_y, sig_r_values) %>%
  #   filter(abs(sig_r_values) > 0.3)
  # draw_data$sig_r_values <- abs(draw_data$sig_r_values)
  #
  # sp_pic <- ggplot2::ggplot(draw_data) +
  #   # ggplot2::geom_sf() +  # 绘制基础图层
  #   ggplot2::geom_segment(aes(x = start_x, y = start_y, xend = end_x, yend = end_y,
  #                             color = sig_r_values),
  #                         size = 1) + # 绘制连线,根据 sig_r_values 着色
  #   ggplot2::scale_color_gradient(low = "blue", high = "red") + # 设置色带范围
  #   ggplot2::labs(color = "sig_r_values") # 设置颜色图例标题
  # # print(sp_pic)
  # sp_fit_pic_path<-file.path(sp_dir,years, paste0(AOU,if(BCR!=0) paste0("_",BCR) else "",".png"))
  # ggplot2::ggsave(sp_fit_pic_path, sp_pic, width = 10, height = 10, units = "in", dpi = 300)
  #

  # print(fit_lm$coefficients)

  print(paste("Finish",years,AOU,paste0(if(BCR!=0) BCR else "")))

  fit_row <- data.frame(AOU = AOU, BCR = BCR,
                      intercept = coef(fit_lm)[1], slope = coef(fit_lm)[2], years = years,
                      r2 = summary(fit_lm)$r.squared, p_value = summary(fit_lm)$coefficients[2, 4])

  return(fit_row)
}

result_rows <- data.frame(AOU = character(), BCR = integer(), years = character(),
                          intercept = numeric(), slope = numeric(),
                          r2 = numeric(), p_value = numeric())

###################### 按分组变量进行拟合#####################################
for(sub_dir in sub_dirs){
  if(is_diff) the_process_data_path<- file.path(sub_dir, diff_BCR_inner_cor_p_basename)
  else  the_process_data_path<-file.path(sub_dir, BCR_inner_cor_p_basename)
  if (!file.exists(the_process_data_path)) next
  years<-basename(sub_dir)
  dir<-file.path(bcr_fit_dir,paste0(if(is_diff) "diff_" else ""))
  sp_dir<-file.path(bcr_fit_sp_dir,paste0(if(is_diff) "diff_" else ""))
  dir.create(dir, showWarnings = FALSE)
  dir.create(sp_dir, showWarnings = FALSE)
  load(file = the_process_data_path)
  BCR_Cor_rows_inner_rows<- BCR_inner_cor_p %>%
    purrr::map2(.,names(.),function (AOU_corr_list, AOU){
      purrr::map2(AOU_corr_list, names(AOU_corr_list), function (AOU_corr_BCR_list, BCR){
        if (nrow(AOU_corr_BCR_list$P) < 50) return(NULL)
        getFlmNetwork(AOU_corr_BCR_list,
                      dir = dir,
                      sp_dir=sp_dir,
                      years=years,
                      AOU=AOU,
                      BCR=as.integer(BCR))
      })%>%  dplyr::bind_rows()
    })%>%  dplyr::bind_rows()

    result_rows <- bind_rows(result_rows, BCR_Cor_rows_inner_rows)
}

########################不按分组变量进行拟合#####################################
for(sub_dir in sub_dirs){
  if(is_diff) the_process_data_path<- file.path(sub_dir, diff_overall_stats_p_basename)
  else  the_process_data_path<-file.path(sub_dir, overall_stats_p_basename)
  if (!file.exists(the_process_data_path)) next
  years<-basename(sub_dir)
  dir <- file.path(fit_dir, paste0(if (is_diff) "diff_" else ""))
  sp_dir <- file.path(fit_sp_dir, paste0(if (is_diff) "diff_" else ""))
  dir.create(dir, showWarnings = FALSE)
  dir.create(sp_dir, showWarnings = FALSE)
  load(file = the_process_data_path)
  Cor_rows<-pairwise_AOU_corr_pearson%>%
    purrr::map2(.,names(.),function (AOU_corr_list, AOU){
      getFlmNetwork(AOU_corr_list,
                dir = dir,
                sp_dir=sp_dir,
                years=years,
                AOU=AOU
      )
    })%>%  dplyr::bind_rows()
    result_rows <- bind_rows(result_rows, Cor_rows)
}
  write.table(result_rows,
            file = file.path(workflow_window_dir,'fit.csv'),
            sep = ",",
            row.names = FALSE,
            col.names = TRUE,
            append = FALSE,
            quote = FALSE)


