states_path<-"NUSABird/2023Release_Nor/States"
workflow_dir<-"NUSABird/2023Release_Nor/Workflow"
routes_path<-"NUSABird/2023Release_Nor/routes.csv"
weather_path<-"NUSABird/2023Release_Nor/weather.csv"
routes_info_with_id_path<-paste(workflow_dir,"route_with_id.csv",sep="/")
sum_species_path<-paste(workflow_dir,"sum_species.csv",sep="/")
AOU_name<-"NUSABird/2023Release_Nor/Workflow/name.csv"
# time moving window
begin<-1967
end<-2019
length<-12
step<-2

top_species<-15
timeline_pic_path<-paste0(workflow_dir,"/pics/","时间窗口.png")