library(purrr)
library(dplyr)
library(tidyr)
#########路径等############
workflow_dir <- "NUSABird/2023Release_Nor/Workflow"
sub_dirs <- list.dirs(workflow_dir, recursive = TRUE, full.names = TRUE)

routes_info_with_id <-read.csv(paste("NUSABird/2023Release_Nor/Workflow/","route_with_id.csv",sep="/"))

routes_list <- sub_dirs %>%
  set_names(basename(sub_dirs)) %>%  # 使用文件夹名称作为列表名称
  map(~list.files(., pattern = "selected_total_data.csv", full.names = TRUE)) %>%
  Filter(length, .) %>%
  map(~map(.x, read.csv)) %>%
  flatten() %>%
    map(~left_join(.x, routes_info_with_id, by = c("CountryNum","StateNum", "Route")))

selected_AOU_number <- read.csv(paste(workflow_dir,"selected_species.csv",sep="/"))["AOU"]
