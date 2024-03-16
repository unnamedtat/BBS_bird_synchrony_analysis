library(purrr)
library(dplyr)
library(tidyr)
source("NUSABird/2023Release_Nor/Script/global/globalPath.R")
#########路径等############

sub_dirs <- list.dirs(workflow_dir, recursive = TRUE, full.names = TRUE)

#带有routeID的routes
routes_info_with_id <-read.csv(routes_info_with_id_path)

#每个window的数据
routes_list <- sub_dirs %>%
  set_names(basename(sub_dirs)) %>%  # 使用文件夹名称作为列表名称
  map(~list.files(., pattern = "selected_total_data.csv", full.names = TRUE)) %>%
  Filter(length, .) %>%
  map(~map(.x, read.csv)) %>%
  flatten() %>%
    map(~left_join(.x, routes_info_with_id, by = c("CountryNum","StateNum", "Route")))

selected_AOU_number <- read.csv(sum_species_path)["AOU"]%>%
  top_n(top_species)
