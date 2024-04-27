suppressMessages(library(purrr))
suppressMessages(library(dplyr))
suppressMessages(library(scatterplot3d))

source("NUSABird/2023Release_Nor/Script/global/globalPath.R")

distance_bwt_BCR <- read.csv(distance_bwt_BCR_path)

sub_dirs <- list.dirs(workflow_dir, recursive = TRUE, full.names = TRUE)
for (sub_dir in sub_dirs){
  if(!file.exists(file.path(sub_dir,"BCR_bwt_cor_p.RData"))) next
  load(file.path(sub_dir,"BCR_bwt_cor_p.RData"))
}

purrr::map(BCR_bwt_cor_p,function(every_AOU) {
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
  model<-lm(cor ~ LINK_DIST,data = dist_cor)
  # model<-lm(cor ~ poly(LINK_DIST, 2, raw = TRUE) * poly(BCR_sum_1, 2, raw=TRUE),data = dist_cor)
  # 检查拟合模型的效果
  print(summary(model))
  # 保存拟合结果
  # save(model, file = file.path(workflow_dir,name,"fit",paste0("fit_",name,".RData")))
  # 画图
  #   plot(dist_cor$LINK_DIST,dist_cor$cor)
  # 绘制三维散点图
  # colorvec <- rgb(dist_cor$cor/max(dist_cor$cor), 0, 0)
  # scatterplot3d(dist_cor$BCR_sum_1,dist_cor$BCR_sum_1,dist_cor$LINK_DIST,
  #                pch = 19,
  #                color = colorvec,
  #                main = "3D Scatterplot",
  #                xlab = "BCR_sum_1",
  #                ylab = "BCR_sum_2",
  #                zlab = "LINK_DIST")
})