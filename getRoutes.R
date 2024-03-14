library(dplyr)
states_path="NUSABird/2023Release_Nor/States"
for (i in seq(1988, 2010, by = 2)){
  Start_Years <- i
  End_Years <- i+9
  save_path=paste0("NUSABird/2023Release_Nor/Workflow/",as.character(Start_Years),"-",as.character(End_Years),"/")
dir.create(save_path)

name.file <- list.files(states_path, full.names = T, pattern = ".csv")

combined_data <- data.frame()

for(file in name.file){
  this_csv <- read.csv(file, encoding = "UTF-8")
  this_data <- this_csv[, -(8:12)]
  combined_data <- rbind(this_data, combined_data)
}

saveRDS(combined_data, paste0(save_path,"Data_States_long.RDS"),)
########### convert the long data into population matrix ###############################
read_data <- readRDS(paste0(save_path,"Data_States_long.RDS") ) %>%
  filter(RPID == "101", Year>=Start_Years & Year <= End_Years)
#check for the number of years for each route in every state
#and only keep the route which lasts for 31 years (1997-2022 except2020)
selected_routes <- read_data %>%
  group_by(CountryNum,StateNum, Route) %>%
  summarise(n.year = n_distinct(Year)) %>%
  filter(n.year >= (End_Years-Start_Years + 1)*0.8) %>% ##取至少覆盖80%的年份
  arrange(StateNum)
  write.csv(selected_routes,paste0(save_path,"selected_routes.csv"))

  selected_records <- semi_join(read_data, selected_routes, by = c("CountryNum","StateNum", "Route"))
  write.csv(selected_records, paste0(save_path,"selected_total_data.csv"))
 # 已废弃
  # #find records for the selected routes with states number
# routes_info <- read.csv("NUSABird/2023Release_Nor/routes.csv")
#
# selected_routes_with_info <- left_join(selected_routes, routes_info, by = c("CountryNum","StateNum", "Route"))
#   write.csv(selected_routes_with_info,paste0(save_path,"selected_routes.csv"))
#
# selected_records <- semi_join(read_data, selected_routes, by = c("CountryNum","StateNum", "Route"))
#
# #and combine them with the their information
# merged_data <- left_join(selected_records, routes_info, by = c("CountryNum","StateNum", "Route"))

#group by distinct routes(and species), year, and calculate the total population for each species in each year
# Routes_count <- merged_data %>%
#   group_by(StateNum, CountryNum, Route,Year) %>%
#   summarise(TotalStops = sum(StopTotal, na.rm = TRUE),
#             TotalSpecies = sum(SpeciesTotal, na.rm = TRUE)) %>%
#   left_join(routes_info, by = c("CountryNum","StateNum", "Route"))

# routes_info_with_id <- routes_info %>%
# mutate(RouteID = row_number())
# Routes_AOU_count <- merged_data %>%
#   group_by(StateNum, CountryNum, Route, AOU,Year) %>%
#   summarise(TotalStops = sum(StopTotal, na.rm = TRUE),
#             TotalSpecies = sum(SpeciesTotal, na.rm = TRUE))%>%
#   left_join(routes_info_with_id, by = c("CountryNum","StateNum", "Route"))

# routes_info_with_id <- routes_info %>%
# mutate(RouteID = row_number())
# merged_data <- left_join(merged_data, routes_info_with_id, by = c("CountryNum","StateNum", "Route"))


# write.csv(merged_data, paste0(save_path,"selected_total_data.csv"))
# write.csv(Routes_AOU_count, paste0(save_path,"Routes_AOU_count.csv"))
# write.csv(Routes_count, paste0(save_path,"Routes_count.csv"))
}


