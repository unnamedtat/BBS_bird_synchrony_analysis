# Description: 参数调整
use_Workflow<-"total"
group_by<-"None"

workflow_total_dir<-"NUSABird/2023Release_Nor/Workflow_total"
workflow_window_dir<-"NUSABird/2023Release_Nor/Workflow_window"
if(use_Workflow=="total"){
  workflow_dir<-workflow_total_dir
  begin<-1966
  end<-2022
  length<-57
  step<-0
  cover_percent<-0.7
}else {
  workflow_dir<-workflow_window_dir
  begin<-1971
  end<-2022
  length<-18
  step<-2
  cover_percent<-0.8
}
# 选取的物种数
top_species<-16
every_top_path <- paste0(workflow_dir, "/every_",top_species,"_top.xlsx")

states_path<-"NUSABird/2023Release_Nor/States"
routes_path<-"NUSABird/2023Release_Nor/routes.csv"
weather_path<-"NUSABird/2023Release_Nor/weather.csv"
# 保存文件的路径
routes_info_with_id_path<-file.path(workflow_dir,"route_with_id.csv")
sum_species_path<-file.path(workflow_dir,"sum_species.csv")
AOU_name_path<-file.path(workflow_dir,"name.csv")
process_data_dir<-file.path(workflow_dir,"Rdata")
routes_list_path<-file.path(process_data_dir,"routes_list.RData")

# 出图路径
picure_dir<-file.path(workflow_dir,"pics")
timeline_pic_path<-file.path(picure_dir,"时间窗口.png")
# 按bcr分组的出图路径
bcr_fit_dir <- file.path(picure_dir,"all_bcr_fit")
bcr_fit_sp_dir <- file.path(picure_dir, "all_bcr_sp_fit")
# 仅按物种分组的出图路径
fit_dir <- file.path(picure_dir, "all_fit")
fit_sp_dir <- file.path(picure_dir, "all_sp_fit")