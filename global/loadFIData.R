suppressMessages(library(purrr))
source("NUSABird/2023Release_Nor/Script/global/globalPath.R")
#############读取过滤插值好的数据，并组合##########################
sub_dirs <- list.dirs(workflow_dir, recursive = TRUE, full.names = TRUE)
prepared_data_list<- sub_dirs %>%
  rlang::set_names(basename(.)) %>%
    purrr::map(., function(dir) {
        if(is_diff) file_path <- file.path(dir, filtered_itp_list_diff_basename)
        else file_path <- file.path(dir, filtered_itp_list_basename)
        if (file.exists(file_path)) {
        load(file_path)
        filtered_itp_list
        }
    })%>%
  Filter(length, .)