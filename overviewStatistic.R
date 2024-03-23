library(stats)
library(purrr)
library(zoo)
library(imputeTS)
# source("NUSABird/2023Release_Nor/Script/globalPlots.R")
source("NUSABird/2023Release_Nor/Script/global/global.R")

##### 对于每个种群的时间序列，计算出每个种群的平均值和标准差#########
routes_list %>%
  purrr::map2(names(.), function(df, name) {
    df<-df%>%
      dplyr::inner_join(selected_AOU_number,by="AOU")%>%
      # dplyr::select(AOU,RouteID, Year, SpeciesTotal)%>% # 下面的计算只需要这几列
      # ！注意！由于(2000年左右之后)每年同一路线、ID的数据不一定唯一
      # 因此需要将相同ID的数据合并
      dplyr::group_by(AOU, RouteID, Year) %>%
      dplyr::summarise(SpeciesTotal = sum(SpeciesTotal, na.rm = TRUE))
      # 从名称中提取开始年份和结束年份
    start_year <- as.numeric(substr(name, 1, 4))
    ###在这里中断
    if(start_year<1989 || start_year>1997){
      return(NULL)
    }
    end_year <- as.numeric(substr(name, 6, 9))
    all_years <- start_year:end_year
    # 获取唯一的AOU和RouteID组合,生成完整的Year序列
    complete_df <- df %>%
      tidyr::expand(tidyr::nesting("AOU", "RouteID"),Year = all_years) %>%
      dplyr::left_join(df, by = c("AOU", "RouteID", "Year")) %>%
      dplyr::mutate(SpeciesTotal = dplyr::coalesce(SpeciesTotal, NA))
    # 将数据按照AOU和RouteID分组，生成时间序列
    ts_list <- split(complete_df, f = complete_df$AOU) %>%
      lapply(., function(AOU_df) {
        split(AOU_df, f = AOU_df$RouteID)%>%
          lapply(., function(per_AOU_df) {
          zoo::zoo(per_AOU_df$SpeciesTotal, order.by = per_AOU_df$Year)
          #   withCallingHandlers(
          #   zoo::zoo(per_AOU_df$SpeciesTotal, order.by = per_AOU_df$Year),
          #   warning = function(e)
          #     print(per_AOU_df)
          #   )
    })
    })
    # 对于每个时间序列，进行缺失值插值，并过滤调质量过差的时间序列
    filtered_list <- lapply(ts_list, function(every_AOU) {
      # 计算每个时间序列的缺失值比例
      na_ratio <- sapply(every_AOU, function(y) mean(is.na(y)))
      # 过滤掉缺失值比例超过80%的时间序列
      every_AOU[na_ratio <= 0.2]%>%
        lapply(., function(y) {
        # 对缺失值进行插值
        imputeTS::na_random(y)###需要确定插值方法
      })
    })
########################计算总体及种群波动指标#######################
    # 计算每个时间序列的平均值和标准差
    all_stats <- filtered_list %>%
      purrr::map2(.,names(.), function(x,AOU) {
          sum_ts <- Reduce(`+`, x) # 子列表时间序列求和
          list(mean = mean(sum_ts), sd = sd(sum_ts),AOU=AOU) # 计算平均值和标准差
      })%>%
      do.call(rbind, .)
    #两两计算相关性
    pairwise_AOU_corr <- filtered_list%>%
    purrr::map(., function(x) {
      ts_names <- names(x)
      # 获取所有时间序列对的组合
      combos <- combn(ts_names, 2, simplify = FALSE)
      cors_test <- t(sapply(combos, function(y) {
        cor_test_result <- stats::cor.test(x[[y[1]]], x[[y[2]]])
        c(unname(cor_test_result$statistic),
          cor_test_result$p.value,
          cor_test_result$conf.int[2],
          cor_test_result$conf.int[1],
          unname(cor_test_result$estimate))
        }))
      # 将结果合并为数据框
      corr_stats <- data.frame(
        t = cors_test[,1],
        p_value = cors_test[,2],
        conf_intervalup = cors_test[,3],
        conf_intervaldown = cors_test[,4],
        cor_value = cors_test[,5],
        routeID1 = unlist(lapply(combos, `[`, 1)),
        routeID2 = unlist(lapply(combos, `[`, 2))
        )
    })
    save(all_stats, file = file.path(paste(workflow_dir, name, sep = "/"), "all_stats.RData"))
    # 保存整体统计数据
    save(pairwise_AOU_corr, file = file.path(paste(workflow_dir, name, sep = "/"), "overall_stats.RData"))
    print(paste("Finish", name))
  })
