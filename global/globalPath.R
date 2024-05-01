# Description: 参数调整
use_Workflow<-"window" #使用的工作流，total为全年，window为时间窗口
is_diff<-FALSE #在插值之后是否进行差分，已放弃，不再差分

# 这里直接写死了读取所有子文件，所以需要留意
workflow_total_dir<-"NUSABird/2023Release_Nor/Workflow_total"
workflow_window_dir<-"NUSABird/2023Release_Nor/Workflow_window"
if(use_Workflow=="total"){
  workflow_dir<-workflow_total_dir
  begin<-1971
  end<-2022
  length<-52 # 窗口长度，全年为
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
distance_bwt_BCR_path<-"NUSABird/2023Release_Nor/BCR_dist.csv"
# 保存文件的路径
routes_info_with_id_path<-file.path(workflow_dir,"route_with_id.csv")
sum_species_path<-file.path(workflow_dir,"sum_species.csv")
AOU_name_path<-file.path("NUSABird/2023Release_Nor","name.csv")
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
filtered_itp_list_diff_basename<-"filtered_itp_list_diff.RData"
filtered_itp_list_basename<-"filtered_itp_list.RData"

# 保存的文件名
overall_stats_p_basename<-"overall_stats_p.RData"
diff_overall_stats_p_basename<-"diff_overall_stats_p.RData"
BCR_inner_cor_p_basename<-"BCR_inner_cor_p.RData"
diff_BCR_inner_cor_p_basename<-"diff_BCR_inner_cor_p.RData"

BCR_bwt_cor_p_basename<-"BCR_bwt_cor_p.RData"


for(dir in list(workflow_dir, process_data_dir, picure_dir,
                bcr_fit_dir, bcr_fit_sp_dir, fit_dir, fit_sp_dir)){
    if (!dir.exists(dir)) dir.create(dir)
}