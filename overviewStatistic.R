suppressMessages(library(Hmisc))
suppressMessages(library(purrr))
suppressMessages(library(zoo))
suppressMessages(library(imputeTS))
suppressMessages(library(purrr))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
source("NUSABird/2023Release_Nor/Script/global/globalPath.R")

# 读取数据
routes_info_with_id <-read.csv(routes_info_with_id_path)
load(file = routes_list_path)
selected_AOU_number<-read.csv(sum_species_path)%>%
  dplyr::filter(SpeciesTotal>0)%>%
  dplyr::top_n(top_species,SpeciesTotal)%>%
  dplyr::select(AOU)

##### 对于每个种群的时间序列，计算出每个种群的平均值和标准差#########
routes_list %>%
  purrr::map2(names(.), function(df, name) {
    df<-df%>%
      dplyr::inner_join(selected_AOU_number,by="AOU")%>%
      # ！注意！由于(2000年左右之后)每年同一路线、ID的数据不一定唯一
      # 因此需要将相同ID的数据合并
      dplyr::group_by(AOU, RouteID, Year) %>%
      dplyr::summarise(SpeciesTotal = sum(SpeciesTotal, na.rm = TRUE))
      # 从名称中提取开始年份和结束年份
    start_year <- as.numeric(substr(name, 1, 4))
    ###在这里中断
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
            # 错误处理
          #   withCallingHandlers(
          #   zoo::zoo(per_AOU_df$SpeciesTotal, order.by = per_AOU_df$Year),
          #   warning = function(e)
          #     print(per_AOU_df)
          #   )
    })
    })
    # 对于每个时间序列，进行缺失值插值，并过滤调质量过差的时间序列
    filtered_itp_list <- lapply(ts_list, function(every_AOU) {
      # 计算每个时间序列的缺失值比例
      na_ratio <- sapply(every_AOU, function(y) mean(is.na(y)))
      # 过滤掉缺失值比例超过80%的时间序列
      every_AOU[na_ratio <= 0.2]%>%
        lapply(., function(y) {
        # 对缺失值进行插值
        imputeTS::na_ma(y)###用移动加权平均进行插值
      })
    })
    save(filtered_itp_list, file = filtered_itp_list_path)
########################计算总体及种群波动指标#######################
    # 计算每个时间序列的平均值和标准差
    all_stats <- filtered_itp_list %>%
      purrr::map2(.,names(.), function(x,AOU) {
          sum_ts <- Reduce(`+`, x) # 子列表时间序列求和
          list(mean = mean(sum_ts), sd = sd(sum_ts),AOU=AOU) # 计算平均值和标准差
      })%>%
      do.call(rbind, .)
    #两两计算相关性
    pairwise_AOU_corr_pearson <- filtered_itp_list%>%
    purrr::map(., function(every_AOU) {
      # 用Hmisc包中的rcorr函数计算相关系数：皮尔逊指数[线性]
      every_AOU %>%
        purrr::map_dfr(., ~ .x)%>%
        sapply(function(x) round(x, 0))%>%
        Hmisc::rcorr(type = "pearson")
      # 后面再算一下非线性的时间序列各种性质
    })
    write.csv(all_stats, file = file.path(paste(workflow_dir, name, sep = "/"), "all_stats.csv"))
    # 保存整体统计数据
    save(pairwise_AOU_corr_pearson, file = file.path(paste(workflow_dir, name, sep = "/"),
                                                    "overall_stats_p.RData"))
    print(paste("Finish", name))
  })
