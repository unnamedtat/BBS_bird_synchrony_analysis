suppressMessages(library(purrr))
suppressMessages(library(dplyr))

source("NUSABird/2023Release_Nor/Script/global/globalPath.R")

distance_bwt_BCR <- read.csv(distance_bwt_BCR_path)


load("NUSABird/2023Release_Nor/Workflow_total/1966-2022/BCR_bwt_cor_p.RData")

purrr::map(BCR_bwt_cor_p,function(every_AOU) {
  # 对每个物种的相关性结果进行拟合
  every_AOU$BCR_1 <- as.integer(every_AOU$BCR_1)
  every_AOU$BCR_2 <- as.integer(every_AOU$BCR_2)
dist_cor <- every_AOU %>%
  dplyr::left_join(distance_bwt_BCR,
            by = c("BCR_1" = "ORIG_FID", "BCR_2" = "DEST_FID"))%>%
            select(cor,LINK_DIST)
  # 拟合线性模型
  model<-lm(cor~LINK_DIST,data = dist_cor)
  # 检查拟合模型的效果
  summary(model)
  # 保存拟合结果
  # save(model, file = file.path(workflow_dir,name,"fit",paste0("fit_",name,".RData")))
  # 画图
    plot(dist_cor$LINK_DIST,dist_cor$cor)
})