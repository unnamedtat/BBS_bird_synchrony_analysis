suppressMessages(library(purrr))
suppressMessages(library(zoo))
suppressMessages(library(imputeTS))
suppressMessages(library(purrr))
suppressMessages(library(tidyr))
suppressMessages(library(dplyr))

source("NUSABird/2023Release_Nor/Script/global/globalPath.R")

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
    })
    })
    # 对于每个时间序列，进行缺失值插值，并过滤调质量过差的时间序列
    filtered_itp_list<-
      lapply(ts_list, function(every_AOU) {
      # 计算每个时间序列的缺失值比例
      na_ratio <- sapply(every_AOU, function(y) mean(is.na(y)))
      # 过滤掉缺失值比例超过80%的时间序列
      every_AOU[na_ratio <= 0.2]%>%
        lapply(., function(y) {
        # 对缺失值进行插值
        itp<-imputeTS::na_ma(y)###用移动加权平均进行插值
        if(is_diff){
          itp<-diff(itp)
        }
        return(itp)
        # tr<-tseries::adf.test(t, alternative="stationary") #检验是否平稳序列
        # return(tr$p.value)
      })
    })
    if(is_diff) save_filename<-file.path(workflow_dir,name,filtered_itp_list_diff_basename)
    else save_filename<-file.path(workflow_dir,name,filtered_itp_list_basename)
    save(filtered_itp_list, file = save_filename)
    print(paste("Finish interpolation", name))
  })
