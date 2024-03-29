suppressMessages(library(sf))
suppressMessages(library(dplyr))
suppressMessages(library(purrr))
suppressMessages(library(ggplot2))
suppressMessages(library(units))

source("NUSABird/2023Release_Nor/Script/global/globalPath.R")

# 读取数据
routes_info_with_id <-read.csv(routes_info_with_id_path)

logn_distance <-routes_info_with_id%>%
  sf::st_as_sf(coords = c("Longitude","Latitude"),crs = 4326) %>%
  sf::st_distance()%>%
  log(base = 10)

# 一切都是按顺序来的
rownames(logn_distance) <- colnames(logn_distance) <- 1:nrow(logn_distance)

#######探究皮尔逊系数的两两相关性################
sub_dirs <- list.dirs(workflow_dir, recursive = TRUE, full.names = TRUE)

getFlmNetwork <- function(corr_list, fit_lm_path, sp_fit_pic_path) {
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
  #######线性拟合############
  fit_lm <- lm(sig_r_values ~ distance, data = fit_data)
  # 绘制散点图
  p <- ggplot2::ggplot(fit_data) +
    ggplot2::geom_point(aes(x = distance, y = sig_r_values)) +
    ggplot2::labs(x = "Distance", y = "r_values", title = "r_values vs Distance") +
    ggplot2::geom_abline(aes(intercept = coef(fit_lm)[1], slope = coef(fit_lm)[2]),
                        color = "red")+
    annotate("text", x = max(fit_data$distance) * 0.85, y = max(fit_data$sig_r_values) * 0.9,
            label = paste("y =", round(coef(fit_lm)[1], 4),
                          "+", round(coef(fit_lm)[2], 4), "* x"),
            parse = TRUE) +
    annotate("text", x = max(fit_data$distance) * 0.85, y = max(fit_data$sig_r_values) * 0.8,
            label = paste("R^2 =", round(summary(fit_lm)$r.squared, 4),
                          ", P =", round(summary(fit_lm)$coefficients[2, 4], 4)))

  ggplot2::ggsave(fit_lm_path, p, width = 10, height = 10, units = "in", dpi = 300)
  # print(summary(fit_lm))
  ###########两两绘制空间相关性####################

  draw_data <- fit_data %>%
    dplyr::select(start_x, start_y, end_x, end_y, sig_r_values) %>%
    filter(abs(sig_r_values) > 0.3)
  draw_data$sig_r_values <- abs(draw_data$sig_r_values)

  sp_pic <- ggplot2::ggplot(draw_data) +
    # ggplot2::geom_sf() +  # 绘制基础图层
    ggplot2::geom_segment(aes(x = start_x, y = start_y, xend = end_x, yend = end_y,
                              color = sig_r_values),
                          size = 1) + # 绘制连线,根据 sig_r_values 着色
    ggplot2::scale_color_gradient(low = "blue", high = "red") + # 设置色带范围
    ggplot2::labs(color = "sig_r_values") # 设置颜色图例标题
  # print(sp_pic)
  ggplot2::ggsave(sp_fit_pic_path, sp_pic, width = 10, height = 10, units = "in", dpi = 300)
  # print(fit_lm$coefficients)
  path_parts <- strsplit(fit_lm_path, "/")[[1]]
  pics_idx <- which(path_parts == "pics")
  desired_part <- paste(path_parts[pics_idx:length(path_parts)], collapse = " ")
  print(desired_part)
  print(paste("Finish",desired_part,sep = ":"))
  return(fit_lm)
}
# dir.create(bcr_fit_dir, showWarnings = FALSE)
# dir.create(bcr_fit_sp_dir, showWarnings = FALSE)
#按分组变量进行拟合
for(sub_dir in sub_dirs){
  if(is_diff) the_process_data_path<- file.path(sub_dir, diff_BCR_inner_cor_p_basename)
  else  the_process_data_dir<-file.path(sub_dir, BCR_inner_cor_p_basename)
  if (!file.exists(the_process_data_path)) next
  years<-basename(sub_dir)
  dir1<-file.path(bcr_fit_dir,paste0(if(is_diff) "diff_" else "",years))
  dir2<-file.path(bcr_fit_sp_dir,paste0(if(is_diff) "diff_" else "",years))
  dir.create(dir1, showWarnings = FALSE)
  dir.create(dir2, showWarnings = FALSE)
  load(file = file.path(sub_dir,BCR_inner_cor_p_basename))
  BCR_inner_cor_p %>%
    purrr::map2(.,names(.),function (AOU_corr_list, AOU){
      purrr::map2(AOU_corr_list, names(AOU_corr_list), function (AOU_corr_BCR_list, BCR){
        if (nrow(AOU_corr_BCR_list$P) < 50) return(NULL)
        getFlmNetwork(AOU_corr_BCR_list,
                  fit_lm_path = file.path(dir1, paste0(AOU,"_", BCR,".png")),
                  sp_fit_pic_path = file.path(dir2,paste0(AOU,"_", BCR,".png")))
      })})
}
# dir.create(fit_dir, showWarnings = FALSE)
# dir.create(fit_sp_dir, showWarnings = FALSE)
for(sub_dir in sub_dirs){
  if(is_diff) the_process_data_path<- file.path(sub_dir, diff_overall_stats_p_basename)
  else  the_process_data_dir<-file.path(sub_dir, overall_stats_p_basename)
  if (!file.exists(the_process_data_path)) next
  years<-basename(sub_dir)
  dir1 <- file.path(fit_dir, paste0(if (is_diff) "diff_" else "", years))
  dir2 <- file.path(fit_sp_dir, paste0(if (is_diff) "diff_" else "", years))
  dir.create(dir1, showWarnings = FALSE)
  dir.create(dir2, showWarnings = FALSE)
  load(file = the_process_data_path)
  pairwise_AOU_corr_pearson%>%
    purrr::map2(.,names(.),function (AOU_corr_list, AOU){
      getFlmNetwork(AOU_corr_list,
                fit_lm_path = file.path(fit_dir,years, paste0(AOU,".png")),
                sp_fit_pic_path = file.path(fit_sp_dir,years, paste0(AOU,".png")))
    })
}