library(gridExtra)
library(readr)
library(stats)
library(purrr)
library(Hmisc)
# source("NUSABird/2023Release_Nor/Script/globalPlots.R")
source("NUSABird/2023Release_Nor/Script/global/global.R")

all_stats<-list()
corr_stats<-list()
##### 对于每个种群的时间序列，计算出每个种群的平均值和标准差#########
routes_list %>%
  map2(names(.), function(df, name) {
    df<-left_join(selected_AOU_number,df,by="AOU")%>%
        select(AOU,RouteID, Year, SpeciesTotal) # 下面的计算只需要这几列
    # 从名称中提取开始年份和结束年份
    start_year <- as.numeric(substr(name, 1, 4))
    end_year <- as.numeric(substr(name, 6, 9))
    all_years <- start_year:end_year
      # 获取唯一的AOU和RouteID组合
      # 首先，建立一个去重的AOU和RouteID组合数据框
   unique_aou_routeid_df <- df %>%
    select(AOU, RouteID) %>%
    distinct()
  # 然后，生成完整的Year序列
    #############这里需要更改，应该是插值？
  complete_df <- unique_aou_routeid_df %>%
    crossing(Year = all_years) %>%
    left_join(df, by = c("AOU", "RouteID", "Year"), suffix = c("", ".y")) %>%
    mutate(SpeciesTotal = coalesce(SpeciesTotal, 0)) %>%
    select(-ends_with(".y"))
  #算出所有选中物种在不同的采样点的统计学性质
  stats <- complete_df %>%
  group_by(AOU, RouteID) %>%
    summarise(
      mean_SpeciesTotal = mean(SpeciesTotal, na.rm = TRUE),
      sd_SpeciesTotal = sd(SpeciesTotal, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
  all_stats[[name]]<-stats

   #按AOU字段分割数据框，分别进行两两皮尔逊指数的计算
  split_AOU_df <- split(complete_df, complete_df$AOU)
  AOU_Pearson <-  map(split_AOU_df, function(every_AOU){
   # 首先将数据展宽,每个RouteID对应10列时间序列
    cor_mat <- every_AOU %>%
      pivot_wider(names_from = RouteID, values_from = SpeciesTotal,names_prefix = "RouteID")%>%
      select(-AOU,-Year)%>%
      as.matrix()%>%
      rcorr()
      return(cor_mat)}
  )
    corr_stats[[name]]<-AOU_Pearson
      })

# 将每个表存入文件
walk2(all_stats, names(all_stats), ~write.csv(.x, file = file.path(paste(workflow_dir, .y, sep = "/"), "AOU_Stastic.csv"), row.names = FALSE))
# 对于每个时间段的相关性系数，算出两两之间的距离
log10_distance<-routes_info_with_id %>%
  select(RouteID,Longitude,Latitude)
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
