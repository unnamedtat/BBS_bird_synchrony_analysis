library(gridExtra)
library(readr)
library(stats)
library(purrr)
library(Hmisc)
library(zoo)
library(imputeTS)
# source("NUSABird/2023Release_Nor/Script/globalPlots.R")
source("NUSABird/2023Release_Nor/Script/global/global.R")

overall_stats<-list()
overall_corr_stats<-list()
##### 对于每个种群的时间序列，计算出每个种群的平均值和标准差#########
routes_list %>%
  map2(names(.), function(df, name) {
    df<-df%>%
      inner_join(selected_AOU_number,by="AOU")%>%
        select(AOU,RouteID, Year, SpeciesTotal) # 下面的计算只需要这几列
    # 从名称中提取开始年份和结束年份
    start_year <- as.numeric(substr(name, 1, 4))
    end_year <- as.numeric(substr(name, 6, 9))
    all_years <- start_year:end_year
    # 获取唯一的AOU和RouteID组合,生成完整的Year序列
    complete_df <- df %>%
      expand(nesting(AOU, RouteID),Year = all_years) %>%
      left_join(df, by = c("AOU", "RouteID", "Year")) %>%
      mutate(SpeciesTotal = coalesce(SpeciesTotal, NA))
    # 将数据按照AOU和RouteID分组，生成时间序列
    ts_list <- split(complete_df, f = complete_df$AOU) %>%
      lapply(., function(AOU_df) {
        split(AOU_df, f = AOU_df$RouteID)%>%
          lapply(., function(per_AOU_df) {
          zoo(per_AOU_df$SpeciesTotal, order.by = per_AOU_df$Year)
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
        na_interpolation(y)
      })
    })
########################计算总体及种群波动指标#######################
    # 计算每个时间序列的平均值和标准差
    all_stats <- filtered_list %>%
      map2(.,names(.), function(x,AOU) {
          sum_ts <- Reduce(`+`, x) # 子列表时间序列求和
          list(mean = mean(sum_ts), sd = sd(sum_ts),AOU=AOU) # 计算平均值和标准差
      })%>%
      do.call(rbind, .)
    #两两计算相关性
    overall_corr_stats[[name]]<-filtered_list%>%
    map(., function(x) {
      ts_names <- names(x)
      # 获取所有时间序列对的组合
      combos <- combn(ts_names, 2, simplify = FALSE)
    cors_test <- t(sapply(combos, function(y) {
      cor_test_result <- cor.test(x[[y[1]]], x[[y[2]]])
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
        overall_corr_stats[[name]]<-corr_stats
    })
  })
# 保存整体统计数据
walk2(overall_corr_stats, names(overall_corr_stats),
      ~write.csv(.x, file = file.path(paste(workflow_dir, .y, sep = "/"),
                                      "AOU_Stastic.csv"), row.names = FALSE))


# 对于每个时间段的相关性系数，算出两两之间的距离
# log10_distance<-routes_info_with_id %>%
#   select(RouteID,Longitude,Latitude)
# 对于矩阵中显著性大于【】的，参与线性拟合计算。



#############已废弃！对于每个表，统计每年最多的物种数目，并将数量前十五的物种按顺序存入每一列################
# routes_AOU_list <- sub_dirs %>%
#   set_names(basename(sub_dirs)) %>%  # 使用文件夹名称作为列表名称
#   map(~list.files(., pattern = "Routes_AOU_count.csv", full.names = TRUE)) %>%
#   map(~map(.x, read.csv))
#
# top_15_species  <- routes_AOU_list %>%
#   map2(names(routes_AOU_list), function(x, name) {
#     if (length(x) == 0) {
#       return(NULL)
#     }
#
#     map_dfr(x, function(df) {
#       stats <- df %>%
#         group_by(Year, AOU) %>%
#         summarise(
#           sum_TotalSpecies = sum(TotalSpecies),
#           .groups = "drop"
#         ) %>%
#         group_by(Year) %>%
#           top_n(15, sum_TotalSpecies)
#
#       return(stats)
#     })
#   })
#
# # 存储每个物种的代码和数量
# top_15_species <- top_15_species[!sapply(top_15_species, is.null)]
# walk2(top_15_species, names(top_15_species),
#       ~write.csv(.x, file = file.path(paste(workflow_dir, .y, sep = "/"),
#                                       "top_15_species_count.csv"), row.names = FALSE))
#
# # 存储每个物种的排名表
# top_15_species_ranked <- top_15_species %>%
#   map(function(df) {
#     # 根据年份和最大物种总数给AOU排序
# top_15_species_ranked <- df %>%
#   arrange(Year, desc(sum_TotalSpecies)) %>%
#   group_by(Year) %>%
#   mutate(rn = row_number()) %>%
#   pivot_wider(names_from = rn, values_from = AOU, names_prefix = "AOU_") %>%
#   select(-sum_TotalSpecies)
#
# merged_df <- top_15_species_ranked %>%
#   group_by(Year) %>%
#   summarise(across(where(is.numeric), ~ coalesce(.[!is.na(.)], NA)))
#   })
#
# # 将每个表存入文件
# walk2(top_15_species_ranked, names(top_15_species_ranked), ~write.csv(.x, file = file.path(paste(workflow_dir, .y, sep = "/"), "top_15_species_AOU.csv"), row.names = FALSE))
#
# # # 创建txt文件并打开用于写入
# # output_file <- file(file.path(workflow_dir,"top_15_species.txt"), "w")
# result_list <- list()
# for (tbl_name in names(top_15_species_ranked)) {
#   # writeLines(paste(tbl_name,"年"), output_file)
#   # writeLines("", output_file)
#   tbl <- top_15_species_ranked[[tbl_name]]
#   name_seq = names(tbl)[names(tbl) != "Year"]
#   curr_result <- tibble(Column = name_seq,
#                         Top_Value = NA_character_,
#                         Count = NA_integer_)
#   for (i in seq_along(name_seq)) {
#     col <- name_seq[i]
#     if (col == "Year") {
#       next
#     }
#     max_val <- tbl %>%
#       group_by_at(col) %>%
#       summarise(count = n()) %>%
#       top_n(1, count) %>%
#       pull(!!sym(col))
#
#     max_count <- tbl %>%
#       group_by_at(col) %>%
#       summarise(count = n()) %>%
#       top_n(1, count) %>%
#       pull(count)
#
#       # 将结果存入curr_result
#       curr_result$Top_Value[i] <- max_val
#       curr_result$Count[i] <- max_count
#
#   # writeLines(paste("字段", col, "最多值为", max_val, ",出现次数为", max_count), output_file)
#   }
#     # 将当前表的结果存入result_list
#     result_list[[tbl_name]] <- curr_result
#     # writeLines("-------------------", output_file)
# }
# # 将result_list转换为一个数据框
# result_df <- dplyr::bind_rows(result_list, .id = "Table")
# # # 关闭文件
# # close(output_file)
#
# write.csv(result_df, file = file.path(workflow_dir, "top_15_species_count.csv"), row.names = FALSE)
