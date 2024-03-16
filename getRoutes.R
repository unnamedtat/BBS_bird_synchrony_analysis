library(dplyr)
source("NUSABird/2023Release_Nor/Script/global/globalPath.R")
# 为所有路线生成唯一ID
routes_info <- read.csv(routes_path)
routes_info_with_id <- routes_info %>%
  mutate(RouteID = row_number())
write.csv(routes_info_with_id, file = routes_info_with_id_path,
                                 row.names = FALSE)
name.file <- list.files(states_path, full.names = T, pattern = ".csv")
# 合并所有data
combined_data <- data.frame()
for(file in name.file){
  this_csv <- read.csv(file, encoding = "UTF-8")
  this_data <- this_csv[, -(8:12)]
  combined_data <- rbind(this_data, combined_data)
}
# 初次筛选
weather_data<-read.csv(weather_path)

# 遍历并生成每个窗口的data
for (i in seq(begin, end-9, by = step)){
  Start_Years <- i
  End_Years <- i+length-1
  save_path=paste(workflow_dir,paste(as.character(Start_Years),as.character(End_Years),sep="-"),sep="/")
  dir.create(save_path)
  # 进一步对每个窗口进行筛选
  this_qualified_data<- weather_data%>%
  filter(RunType == "1",RPID=="101")
  # 为了匹配2017年之前的情况

########### convert the long data into population matrix ###############################
#check for the number of years for each route in every state
#and only keep the route which lasts for 31 years (1997-2022 except2020)
  combined_data_in_this<-combined_data%>%
    filter(Year>=Start_Years & Year <= End_Years)
  # 保证剩下的路线质量较高
  # 取至少覆盖80%的年份
  selected_routes <- this_qualified_data %>%
    semi_join(combined_data_in_this,by = "RouteDataID")%>%
    # 避免重命名重复列
    group_by(CountryNum,StateNum, Route) %>%
    # 保留覆盖年份80%的路线
    summarise(n.year = n_distinct(Year)) %>%
    filter(n.year >= (End_Years-Start_Years + 1)*0.8)%>%
    arrange(StateNum)

  write.csv(selected_routes,paste(save_path,"selected_routes.csv",sep="/"))
  selected_records <- semi_join(combined_data_in_this, selected_routes, by = c("CountryNum","StateNum", "Route"))
  write.csv(selected_records, paste(save_path,"selected_total_data.csv",sep="/"))
}
###########取全年的优势种，直接把质量达标的统计起来求和，差别不大##########################
sum_species <- weather_data %>%
  filter(RunType == "1",RPID==101)%>%
  left_join(combined_data)%>%
  filter(Year>=begin & Year <= end)%>%
  group_by(AOU)%>%
  summarise(SpeciesTotal = sum(SpeciesTotal))%>%
  arrange(desc(SpeciesTotal))
write.csv(sum_species,sum_species_path)
