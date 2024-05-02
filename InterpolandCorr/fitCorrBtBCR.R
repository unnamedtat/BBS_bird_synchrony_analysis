suppressMessages(library(purrr))
suppressMessages(library(dplyr))
suppressMessages(library(scatterplot3d))

source("NUSABird/2023Release_Nor/Script/global/globalPath.R")
source("NUSABird/2023Release_Nor/Script/global/globalPlots.R")

# 使用各bcr质心计算
distance_bwt_BCR <- read.csv(distance_bwt_BCR_path)

sub_dirs <- list.dirs(workflow_dir, recursive = TRUE, full.names = TRUE)
for (sub_dir in sub_dirs){
  if(!file.exists(file.path(sub_dir,BCR_bwt_cor_p_basename))) next
  load(file.path(sub_dir,BCR_bwt_cor_p_basename))
}

purrr::map2(BCR_bwt_cor_p,names(BCR_bwt_cor_p),function(every_AOU, AOU) {
  # 对每个物种的相关性结果进行拟合
  every_AOU$BCR_1 <- as.integer(every_AOU$BCR_1)
  every_AOU$BCR_2 <- as.integer(every_AOU$BCR_2)
  dist_cor <- every_AOU %>%
  dplyr::left_join(distance_bwt_BCR,
            by = c("BCR_1" = "ORIG_FID", "BCR_2" = "DEST_FID"))%>%
            select(cor,LINK_DIST,BCR_sum_1,BCR_sum_2)
  # 拟合线性模型
  dist_cor$BCR_sum_1<- as.integer(dist_cor$BCR_sum_1)
  dist_cor$BCR_sum_2<- as.integer(dist_cor$BCR_sum_2)
  dist_cor$BCR_sum_dif<-abs(dist_cor$BCR_sum_1-dist_cor$BCR_sum_2)
  dist_cor$cor<- as.numeric(dist_cor$cor)
  dist_cor$LINK_DIST<-log(dist_cor$LINK_DIST,10)
  model<-lm(cor ~ LINK_DIST,data = dist_cor)
  # model<-lm(cor ~ poly(LINK_DIST, 2, raw = TRUE) * poly(BCR_sum_1, 2, raw=TRUE),data = dist_cor)
  # 检查拟合模型的效果
  # print(summary(model))
  # 保存拟合结果
  # save(model, file = file.path(workflow_dir,name,"fit",paste0("fit_",name,".RData")))
  # 绘制三维散点图
  # colorvec <- rgb(dist_cor$cor/max(dist_cor$cor), 0, 0)
  # scatterplot3d(dist_cor$BCR_sum_1,dist_cor$BCR_sum_1,dist_cor$LINK_DIST,
  #                pch = 19,
  #                color = colorvec,
  #                main = "3D Scatterplot",
  #                xlab = "BCR_sum_1",
  #                ylab = "BCR_sum_2",
  #                zlab = "LINK_DIST")
  # 绘制散点图并保存
  x_range <- range(dist_cor$LINK_DIST, na.rm = TRUE)
  y_range <- range(dist_cor$cor, na.rm = TRUE)
  x_pos1 <- (x_range[2]-x_range[1]) * 0.15 + x_range[1]
  y_pos1 <- (y_range[2]-y_range[1]) * 0.15 + y_range[1]
  x_pos2 <- (x_range[2]-x_range[1]) * 0.15 + x_range[1]
  y_pos2 <- (y_range[2]-y_range[1]) * 0.25 + y_range[1]
  p <- ggplot2::ggplot(dist_cor) +
    ggplot2::geom_point(aes(x = LINK_DIST, y = cor)) +
    ggplot2::labs(x = "距离(log10)", y = "相关性系数", title = "") +
    ggplot2::geom_abline(aes(intercept = coef(model)[1], slope = coef(model)[2]),
                        color = "red")+
    annotate("text", x = x_pos1, y = y_pos1,
            label = paste("y =", round(coef(model)[1], 4),
                          "+", round(coef(model)[2], 4), "* x"),
            family="Times New Roman", size = 5) +
    annotate("text", x = x_pos2, y = y_pos2,
            label = paste("R^2 =", round(summary(model)$r.squared, 5),
                          ", P =", round(summary(model)$coefficients[2, 4], 8)),
            family="Times New Roman", size = 5)+
    theme_bar+
    theme(plot.background = element_rect(fill = "white",colour = "transparent"))
  ggplot2::ggsave(file.path(bcr_btw_fit_dir,paste0(AOU, ".png")), p, width = plots_corr_width, height = plots_corr_height,
                  units = "in", dpi = 300)
})