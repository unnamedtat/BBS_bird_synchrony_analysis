states_path<-"NUSABird/2023Release_Nor/States"
workflow_dir<-"NUSABird/2023Release_Nor/Workflow"
routes_path<-"NUSABird/2023Release_Nor/routes.csv"
weather_path<-"NUSABird/2023Release_Nor/weather.csv"
routes_info_with_id_path<-paste(workflow_dir,"route_with_id.csv",sep="/")
sum_species_path<-paste(workflow_dir,"sum_species.csv",sep="/")
AOU_name_path<-"NUSABird/2023Release_Nor/Workflow/name.csv"
process_data_dir<-paste(workflow_dir,"Rdata", sep = "/")
routes_list_path<-file.path(process_data_dir,"routes_list.RData")

# 出图路径
timeline_pic_path<-paste0(workflow_dir,"/pics/","时间窗口.png")
top_species<-16
every_top_path <- paste0(workflow_dir, "/every_",top_species,"_top.xlsx")