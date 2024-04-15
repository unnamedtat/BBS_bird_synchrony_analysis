suppressMessages(library(zoo))
suppressMessages(library(purrr))
source("NUSABird/2023Release_Nor/Script/global/loadFIData.R")

routes_info_with_id <-read.csv(routes_info_with_id_path)

# 根据BCR分组计算总和，求总和的相关性
routes_BCR_groups <- split(routes_info_with_id, routes_info_with_id$BCR)
prepared_data_list %>%
  purrr::map2(names(.), function(filtered_itp_list, name) {
    BCR_bwt_cor_p<-filtered_itp_list %>%
      purrr::map(., function(every_AOU) {
        # 获取BCR内鸟的总数
        BCR_total<-purrr::map(routes_BCR_groups, function(grid_data) {
          matched_list<-every_AOU[grid_data$RouteID]%>%
            compact()
          if(length(matched_list)<2)return(NULL)
          z_combined_vec <- do.call(cbind, matched_list)
          rowSums(z_combined_vec)
        }) %>%
        compact()
        BCR_bwt_cor<-purrr::map_dfr(BCR_total, ~ .x)%>%
          sapply(function(x) round(x, 0))%>%
          Hmisc::rcorr(type = "pearson")
      }
      )
    save(BCR_bwt_cor_p, file = BCR_bwt_cor_p_basename)
  }
  )
