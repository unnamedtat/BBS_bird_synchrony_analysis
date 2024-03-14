library(stats)
library(purrr)
library(gridExtra)
source("NUSABird/2023Release_Nor/Script/globalPlots.R")
source("NUSABird/2023Release_Nor/Script/global.R")

# ####################### 种群整体动态 #######################
# 对每个数据框执行分析和绘图操作
plot1s_list <- routes_list %>%
  map2(names(.), function(x, name) {
      total_species <- x %>%
        group_by(Year) %>%
        summarise(Total = sum(SpeciesTotal))
      min_year <- min(total_species$Year)
      max_year <- max(total_species$Year)

      total_plot <- ggplot(total_species, aes(x = Year, y = Total)) +
        geom_line() +
        geom_point(colour = "gray", size = 2, alpha = 0.5, shape = 19) +
        # geom_text(aes(label = Total), vjust = -1)+
        ggtitle(paste0(name,"年"))+  # 使用文件名作为图形标题
        scale_x_continuous(breaks = seq(min_year,max_year,4))+
        # scale_y_continuous(labels = scales::label_scientific(digits = 2))+
        scale_y_continuous(labels = fancy_scientific)+
        labs(x = "年份", y = "观测总数量") +
        theme_custom+
        theme(axis.title.y = element_text(angle =90))
      return(total_plot)
  })
# 将所有图形排列在一张图上
plot1_grid <- do.call(grid.arrange, c(plot1s_list, ncol = 2)) %>%
  ggsave(paste0(workflow_dir,"/pics/","时间窗口.png"),., width = 12, height = 15, units = "in", dpi = 300)
####################### 查看每个时间窗的路线分布 #######################
selected_routes_list <- sub_dirs %>%
  set_names(basename(sub_dirs)) %>%
  map(~list.files(., pattern = "selected_routes.csv", full.names = TRUE)) %>%
  Filter(length, .) %>%
  map(~map(.x, read.csv)) %>%
  flatten() %>%
  map(~left_join(.x, routes_info, by = c("CountryNum","StateNum", "Route")))

all_routes <- read.csv("NUSABird/2023Release_Nor/routes.csv")
all_routes_sf <- st_as_sf(all_routes, coords = c("Longitude", "Latitude"), crs = 4326)
df_bbox <- st_bbox(all_routes_sf)
#图形边界框，避免图形边界被遮挡
buffer<-1
bbox<-df_bbox
bbox["xmin"] <- bbox["xmin"]-1
bbox["ymin"] <- bbox["ymin"]-1
bbox["xmax"] <- bbox["xmax"]+1
bbox["ymax"] <- bbox["ymax"]+1
# cropped_map <- st_crop(world_map, df_bbox)
bbox_sf <- st_as_sfc(bbox)

# 创建一个绘图函数,用于绘制单个数据框中的经纬度数据
plot_latlng <- function(df,name) {
  # df<-data.frame(df[1])
   df<- st_as_sf(df,coords = c("Longitude", "Latitude"), crs = 4326)

  plot<-ggplot(df) +
    theme_geo+
    # coord_equal() +# 保持x和y轴比例相同
    geom_sf(data = world_map, fill = "white")+
    geom_sf(data = all_routes_sf, color = "lightgray",size=0.1) +
    geom_sf(data = df, color = "black",size=0.1)+
    geom_sf(data = bbox_sf, fill = "transparent",linewidth=0.7,color="black")+
    coord_sf(xlim = c(df_bbox["xmin"]-buffer, df_bbox["xmax"]+buffer),
              ylim = c(df_bbox["ymin"]-buffer, df_bbox["ymax"]+buffer),expand = FALSE)+
    # theme(axis.text = element_blank(), axis.ticks = element_blank()) +
      ggtitle(paste0(name,"年：",nrow(df),"条路线"))  # 使用文件夹名作为图形标题
    # scale_x_continuous(name = "Longitude") +
    # scale_y_continuous(name = "Latitude")
    # labs(title = "路线分布", x = "Longitude", y = "Latitude")
  return(plot)
}

# # 对selected_routes_list中的每一个二维列表应用绘图函数
# plot_2 <- selected_routes_list %>%
#      imap(~ {
#     df <- .x
#     names(.y) <- .y # 使用文件夹名作为图形标题
#     plot_latlng(df, .y)
#   })
plot_2 <- selected_routes_list %>%
     map2(names(.), ~plot_latlng(.x, .y))

# 将所有图形排列在一张图上
plot_2 <- do.call(grid.arrange, c(plot_2, ncol = 3))

# 显示最终图形
plot_2  %>%
  ggsave(paste0(workflow_dir,"/pics/","路线分布.png"),., width = 12, height = 15, units = "in", dpi = 300)
####################### 查看每个时间窗的物种分布 #######################
library(openxlsx)
top_16_path <- paste0(workflow_dir,"/top_16.xlsx")
top_16_workbook <- createWorkbook()
AOU_name <- read.csv("NUSABird/2023Release_Nor/Workflow/name.csv")
# 对每个数据框执行分析和绘图操作
plot3s_list <- routes_list %>%
  map2(names(.), function(x, name) {
      AOU_count <- x %>%
        group_by(AOU) %>%
        summarise(Total = sum(SpeciesTotal))%>%
        top_n(16, Total)%>%
        left_join(AOU_name, by = c("AOU" = "AOU"))%>%
        arrange(desc(Total))

      addWorksheet(top_16_workbook, sheetName = name)
      writeData(top_16_workbook, sheet = name, AOU_count[,c("AOU","English_Common_Name","Total")])
      AOU_count_plot <- ggplot(AOU_count, aes(x = English_Common_Name, y = Total, fill = English_Common_Name )) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_y_continuous(labels = fancy_scientific)+
        ggtitle(paste0(name, " 年数量前16的物种分布")) +
        labs(x = "", y = "",fill="物种名称") +
        theme_bar
    
      return(AOU_count_plot)
  })
# 保存工作簿
saveWorkbook(top_16_workbook, top_16_path, overwrite = TRUE)
# 将plots列表分成多个子列表,每个子列表包含8个图形
sub_plots <- split(plot3s_list, rep(1:ceiling(length(plot3s_list)/6), each=6, length.out=length(plot3s_list)))

# 遍历每个子列表,并将子列表中的图形排列在一张图上并保存
for (i in seq_along(sub_plots)) {
  plot3_grid <- do.call(grid.arrange, c(sub_plots[[i]], ncol = 2)) %>%
  ggsave(paste0(workflow_dir,"/pics/","物种数", i, ".png"),.,width=15, height=20, units = "in", dpi = 300)
}

# 获取工作簿中所有工作表名称
sheet_names <- getSheetNames(top_16_path)

# 读取每个工作表的数据
all_data <- lapply(sheet_names, function(sheet_name) {
  read.xlsx(top_16_workbook, sheet = sheet_name)
})
# 合并所有数据并计算和
merged_data <- bind_rows(all_data, .id = "sheet_name") %>%
  group_by(English_Common_Name) %>%
  summarise(total = sum(Total))

# 打印结果
print(merged_data)

####################### （已废弃）查看每年的物种分布，只对10-19执行 #######################
#
# AOU_name <- read.csv("NUSABird/2023Release_Nor/Workflow/name.csv")
# AOU_Count <- routes_list[2010-2019]%>%
#   left_join(AOU_name, by = c("AOU" = "AOU"))
# ## 需要更改
# df_by_year <- split(AOU_Count, AOU_Count$Year)
# plot_3 <- lapply(df_by_year, function(aou_df) {
#   total_species <- aou_df %>%
#     group_by(English_Common_Name) %>%
#     summarise(Total = sum(SpeciesTotal))%>%
#     top_n(15, Total)
#   total_species <- data.frame(AOU_name = total_species$English_Common_Name, Total = total_species$Total)
#   total_plot <- ggplot(total_species, aes(x = AOU_name, y = Total, fill = AOU_name )) +
#     geom_bar(stat = "identity", position = "dodge") +
#     ggtitle(paste0(as.character(aou_df$Year[1]), " 年数量前15的物种分布")) +
#     labs(x = "", y = "",fill="物种名称") +
#     theme_bar
#   return(total_plot)
# })
#
# # 将plots列表分成多个子列表,每个子列表包含6个图形
# sub_plots <- split(plot_3, rep(1:ceiling(length(plot_3)/6), each=6, length.out=length(plot_3)))
#
# # 遍历每个子列表,并将子列表中的图形排列在一张图上并保存
# for (i in seq_along(sub_plots)) {
#   plot_grid <- do.call(grid.arrange, c(sub_plots[[i]], ncol=2, nrow=3))
#   ggsave(filename=paste0("NUSABird/2023Release_Nor/Workflow/pics/AOU(10-19)/
#   ","物种数", i, ".png"), plot=plot_grid, width=15, height=30)
# }

####################### 已废弃查看AOU每年分布 #######################
# # 读取并存储所有Routes_AOU_count.csv文件
# routes_list <- sub_dirs %>%
#   set_names(basename(sub_dirs)) %>%  # 使用文件夹名称作为列表名称
#   map(~list.files(., pattern = "Routes_AOU_count.csv", full.names = TRUE)) %>%
#   map(~map(.x, read.csv))
#
# # 对每个数据框执行分析和绘图操作
# plots_list <- routes_list %>%
#   map2(names(routes_list), function(x, name) {
#     map(x, function(df) {
#       # 确保数据框包含Year列
#       if ("Year" %in% names(df)) {
#         df_by_aou <- split(df, df$AOU) # 按AOU字段分割数据框
#         map(df_by_aou, function(aou_df) {
#           total_species <- aou_df %>%
#             group_by(Year) %>%
#             summarise(Total = sum(TotalSpecies))
#           total_species <- data.frame(Year = total_species$Year, Total = total_species$Total)
#           min_year <- min(total_species$Year)
#           max_year <- max(total_species$Year)
#           aou_name <- unique(aou_df$AOU) # 获取当前AOU的名称
#
#           total_plot <- ggplot(total_species, aes(x = Year, y = Total)) +
#             geom_line() +
#             geom_point(colour = "gray", size = 2, alpha = 0.5, shape = 19) +
#             ggtitle(paste(name, aou_name)) + # 使用文件名和AOU名称作为图形标题
#             scale_x_continuous(breaks = seq(min_year, max_year, 4)) +
#             scale_y_continuous(labels = scales::scientific) +
#             labs(x = "年份", y = "观测总数量") +
#             theme_custom
#           return(total_plot)
#         })
#       } else {
#         return(NULL) # 如果数据框不包含Year列,则返回NULL
#       }
#     })
#   })
#

####################### 查看AOU每年分布 #######################
# # 读取并存储所有Routes_AOU_count.csv文件
# routes_list <- sub_dirs %>%
#   set_names(basenamesub_dirs()) %>%  # 使用文件夹名称作为列表名称
#   map(~list.files(., pattern = "Routes_AOU_count.csv", full.names = TRUE)) %>%
#   map(~map(.x, read.csv))
#
# # 对每个数据框执行分析和绘图操作
# plots_list <- routes_list %>%
#   map2(names(routes_list), function(x, name) {
#     map(x, function(df) {
#       # 确保数据框包含Year列
#       if ("Year" %in% names(df)) {
#         df_by_aou <- split(df, df$AOU) # 按AOU字段分割数据框
#         map(df_by_aou, function(aou_df) {
#           total_species <- aou_df %>%
#             group_by(Year) %>%
#             summarise(Total = sum(TotalSpecies))
#           total_species <- data.frame(Year = total_species$Year, Total = total_species$Total)
#           min_year <- min(total_species$Year)
#           max_year <- max(total_species$Year)
#           aou_name <- unique(aou_df$AOU) # 获取当前AOU的名称
#
#           total_plot <- ggplot(total_species, aes(x = Year, y = Total)) +
#             geom_line() +
#             geom_point(colour = "gray", size = 2, alpha = 0.5, shape = 19) +
#             ggtitle(paste(name, aou_name)) + # 使用文件名和AOU名称作为图形标题
#             scale_x_continuous(breaks = seq(min_year, max_year, 4)) +
#             scale_y_continuous(labels = scales::scientific) +
#             labs(x = "年份", y = "观测总数量") +
#             theme_custom
#           return(total_plot)
#         })
#       } else {
#         return(NULL) # 如果数据框不包含Year列,则返回NULL
#       }
#     })
#   })
#

# # 展平result_list中的所有列表元素
# all_AOU_plot <- plots_list %>% flatten() %>% compact() # 使用compact()函数移除NULL值
#
# # 将所有图形排列在一张图上
# plot_grid <- do.call(grid.arrange, c(all_AOU_plot, ncol = 2))
#
# # 显示最终图形
# plot_grid %>%
#   ggsave(paste0(workflow_dir, "/pics/", "时间窗口.png"), ., dpi = 300, width = 12, height = 8, units = "in")
# # 读取并存储所有Routes_AOU_count.csv文件
# routes_list <- sub_dirs %>%
#   set_names(basename(sub_dirs)) %>%  # 使用文件夹名称作为列表名称
#   map(~list.files(., pattern = "Routes_AOU_count.csv", full.names = TRUE)) %>%
#   map(~map(.x, read.csv))
#
# # 对每个数据框执行分析和绘图操作
# plots_list <- routes_list %>%
#   map2(names(routes_list), function(x, name) {
#     map(x, function(df) {
#       total_species <- df %>%
#         group_by(Year) %>%
#         summarise(Total = sum(TotalSpecies))
#       total_species <- data.frame(Year = total_species$Year, Total = total_species$Total)
#       min_year <- min(total_species$Year)
#       max_year <- max(total_species$Year)
#
#       total_plot <- ggplot(total_species, aes(x = Year, y = Total)) +
#         geom_line() +
#         geom_point(colour = "gray", size = 2, alpha = 0.5, shape = 19) +
#         # geom_text(aes(label = Total), vjust = -1)+
#         ggtitle(name)+  # 使用文件名作为图形标题
#         scale_x_continuous(breaks = seq(min_year,max_year,4))+
#         scale_y_continuous(labels = scales::scientific)+
#         labs(x = "年份", y = "观测总数量") +
#         theme_custom
#       return(total_plot)
#     })
#   })
# # # 展平result_list中的所有列表元素
# all_AOU_plot <- plots_list %>% flatten()
# # # 将所有图形排列在一张图上
# # plot_grid <- do.call(grid.arrange, c(plot_1, ncol = 2))
# #
# # # 显示最终图形
# # plot_grid  %>%
# #   ggsave(paste0(workflow_dir,"/pics/AOU/","物种.png"),., dpi = 300, width = 12, height = 8, units = "in")
# #
