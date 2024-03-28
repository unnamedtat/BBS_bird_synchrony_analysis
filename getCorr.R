suppressMessages(library(Hmisc))
suppressMessages(library(tseries))
suppressMessages(library(purrr))
source("NUSABird/2023Release_Nor/Script/global/globalPath.R")

sub_dirs <- list.dirs(workflow_dir, recursive = TRUE, full.names = TRUE)
loaded_data_list<- sub_dirs %>%
  rlang::set_names(basename(.)) %>%
    purrr::map(., function(dir) {
        file_path <- file.path(dir, "filtered_itp_list.RData")
        if (file.exists(file_path)) {
        load(file_path)
        filtered_itp_list
        }
    })%>%
  Filter(length, .)

########################皮尔逊相关指数#############################
    pairwise_AOU_corr_pearson <- filtered_itp_list%>%
        purrr::map(., function(every_AOU) {
          # 用Hmisc包中的rcorr函数计算相关系数：皮尔逊指数[线性]
          every_AOU %>%
            purrr::map_dfr(., ~ .x)%>%
            sapply(function(x) round(x, 0))%>%
            Hmisc::rcorr(type = "pearson")
          # 后面再算一下非线性的时间序列各种性质
        })
        # 保存整体统计数据
        save(pairwise_AOU_corr_pearson, file = file.path(paste(workflow_dir, name, sep = "/"),
                                                        "overall_stats_p.RData"))
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