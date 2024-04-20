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
            purrr::compact()

          if(length(matched_list)<2)return(NULL)
          z_combined_vec <- do.call(cbind, matched_list)
          rowSums(z_combined_vec)
        }) %>%
        purrr::compact()

        # 获取皮尔逊指数
        BCR_round_df<-purrr::map_dfr(BCR_total, ~ .x)%>%
          sapply(function(x) round(x, 0))

        combins <- combn(colnames(BCR_round_df), 2)

        ccf_corr <- apply(combins, 2, FUN =
          function(x){
            ccf(BCR_round_df[, x[1]], BCR_round_df[, x[2]], plot = FALSE,
                lag.max = 1)$acf[2]
          })
      # 还要计算相关的两个区域的总数
        BCR_sum_1 <- apply(combins, 2, FUN =
          function(x){
            sum(BCR_round_df[, x[1]])
          })
        BCR_sum_2 <- apply(combins, 2, FUN =
          function(x){
            sum(BCR_round_df[, x[2]])
          })

        ccf_corr_with_cb <- t(rbind(combins, ccf_corr,BCR_sum_1,BCR_sum_2))
        colnames(ccf_corr_with_cb) <- c("BCR_1", "BCR_2", "cor","BCR_sum_1","BCR_sum_2")
        as.data.frame(ccf_corr_with_cb)
      }
      )
    save(BCR_bwt_cor_p, file = file.path(workflow_dir,name,BCR_bwt_cor_p_basename))
  }
  )
