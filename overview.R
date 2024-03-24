suppressMessages(library(gridExtra))
suppressMessages(library(purrr))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
source("NUSABird/2023Release_Nor/Script/global/globalPlots.R")
source("NUSABird/2023Release_Nor/Script/global/globalPath.R")

# 读取数据
routes_info_with_id <-read.csv(routes_info_with_id_path)
load(file = routes_list_path)
sub_dirs <- list.dirs(workflow_dir, recursive = TRUE, full.names = TRUE)
# ####################### 种群整体动态 #######################
# 对每个数据框执行分析和绘图操作

plot1s_list <- routes_list %>%
  purrr::map2(names(.), function(x, name) {
      total_species <- x %>%
        dplyr::group_by(Year) %>%
        dplyr::summarise(SpeciesTotal = sum(SpeciesTotal))
      min_year <- min(total_species$Year)
      max_year <- max(total_species$Year)

      total_plot <- ggplot2::ggplot(total_species, aes(x = Year, y = SpeciesTotal)) +
        ggplot2::geom_line() +
        ggplot2::geom_point(colour = "gray", size = 2, alpha = 0.5, shape = 19) +
        # geom_text(aes(label = Total), vjust = -1)+
        ggplot2::ggtitle(paste0(name,"年"))+  # 使用文件名作为图形标题
        ggplot2::scale_x_continuous(breaks = seq(min_year,max_year,4))+
        # scale_y_continuous(labels = scales::label_scientific(digits = 2))+
        ggplot2::scale_y_continuous(labels = fancy_scientific)+
        ggplot2::labs(x = "年份", y = "观测总数量") +
        theme_custom+
        ggplot2::theme(axis.title.y = element_text(angle =90))
      return(total_plot)
  })
# 将所有图形排列在一张图上
# plot1_grid <- do.call(grid.arrange, c(plot1s_list, ncol = 2)) %>%
#   ggsave(timeline_pic_path,., width = 12, height = 15, units = "in", dpi = 300)
sub_plots1 <- split(plot1s_list, rep(1:ceiling(length(plot1s_list)/plots1_per_img),
                                  each=plots1_per_img, length.out=length(plot1s_list)))

# 遍历每个子列表,并将子列表中的图形排列在一张图上并保存
for (i in seq_along(sub_plots1)) {
  plot1_grid <- do.call(grid.arrange, c(sub_plots1[[i]], ncol = plots1_ncol)) %>%
  ggplot2::ggsave(paste0(workflow_dir,"/pics/","timewindows", i, ".png"),.,
          width=plots1_width, height=plots1_height, units = "in", dpi = 300)
}
####################### 查看每个时间窗的路线分布 #######################
selected_routes_list <- sub_dirs %>%
  rlang::set_names(basename(sub_dirs)) %>%
  purrr::map(~list.files(., pattern = "selected_routes.csv", full.names = TRUE)) %>%
  purrr::map(~purrr::map(.x, read.csv)) %>%
  purrr::flatten() %>%
  purrr::map(~dplyr::left_join(.x, routes_info_with_id, by = c("CountryNum","StateNum", "Route")))

all_routes_sf <- sf::st_as_sf(routes_info_with_id,
                              coords = c("Longitude", "Latitude"), crs = 4326)
df_bbox <- sf::st_bbox(all_routes_sf)
#图形边界框，避免图形边界被遮挡
buffer<-1
bbox<-df_bbox
bbox["xmin"] <- bbox["xmin"]-1
bbox["ymin"] <- bbox["ymin"]-1
bbox["xmax"] <- bbox["xmax"]+1
bbox["ymax"] <- bbox["ymax"]+1
# cropped_map <- st_crop(world_map, df_bbox)
bbox_sf <- sf::st_as_sfc(bbox)

# 创建一个绘图函数,用于绘制单个数据框中的经纬度数据
plot_latlng <- function(df,name) {
  df<- sf::st_as_sf(df,coords = c("Longitude", "Latitude"), crs = 4326)

  plot<-ggplot2::ggplot(df) +
    theme_geo+
    # coord_equal() +# 保持x和y轴比例相同
    ggplot2::geom_sf(data = world_map, fill = "white")+
    ggplot2::geom_sf(data = all_routes_sf, color = "lightgray",size=0.1) +
    ggplot2::geom_sf(data = df, color = "black",size=0.1)+
    ggplot2::geom_sf(data = bbox_sf, fill = "transparent",linewidth=0.7,color="black")+
    ggplot2::coord_sf(xlim = c(df_bbox["xmin"]-buffer, df_bbox["xmax"]+buffer),
              ylim = c(df_bbox["ymin"]-buffer, df_bbox["ymax"]+buffer),expand = FALSE)+
    # theme(axis.text = element_blank(), axis.ticks = element_blank()) +
      ggplot2::ggtitle(paste0(name,"年：",nrow(df),"条路线"))  # 使用文件夹名作为图形标题
    # scale_x_continuous(name = "Longitude") +
    # scale_y_continuous(name = "Latitude")
    # labs(title = "路线分布", x = "Longitude", y = "Latitude")
  return(plot)
}

plot_2 <- selected_routes_list %>%
    purrr::map2(names(.), ~plot_latlng(.x, .y))

# # 将所有图形排列在一张图上
# plot_2 <- do.call(grid.arrange, c(plot_2, ncol = 3))
# # 显示最终图形
# plot_2  %>%
#   ggsave(paste0(workflow_dir,"/pics/","路线分布.png"),., width = 12, height = 15, units = "in", dpi = 300)
# 将plots列表分成多个子列表,每个子列表包含8个图形
sub_plots2 <- split(plot_2, rep(1:ceiling(length(plot_2)/plots2_per_img),
                                    each=plots2_per_img, length.out=length(plot_2)))

# 遍历每个子列表,并将子列表中的图形排列在一张图上并保存
for (i in seq_along(sub_plots2)) {
  plot2_grid <- do.call(grid.arrange, c(sub_plots2[[i]], ncol = plots2_ncol)) %>%
  ggplot2::ggsave(paste0(workflow_dir,"/pics/","routes", i, ".png"),.,
        width=plots2_width, height=plots2_height, units = "in", dpi = 300)
}

####################### 查看每个时间窗的物种分布 #######################
suppressMessages(library(openxlsx))
top_16_workbook <- openxlsx::createWorkbook()
AOU_name <- read.csv(AOU_name_path) %>%
  dplyr::select(AOU, English_Common_Name)
# 对每个数据框执行分析和绘图操作
plot3s_list <- routes_list %>%
  purrr::map2(names(.), function(x, name) {
      AOU_count <- x %>%
        dplyr::group_by(AOU) %>%
        dplyr::summarise(Total = sum(SpeciesTotal))%>%
        dplyr::top_n(16, Total)%>%
        dplyr::left_join(AOU_name, by = c("AOU" = "AOU"))%>%
        dplyr::arrange(dplyr::desc(Total))

      openxlsx::addWorksheet(top_16_workbook, sheetName = name)
      openxlsx::writeData(top_16_workbook, sheet = name,
                          AOU_count[,c("AOU","English_Common_Name","Total")])
      AOU_count_plot <- ggplot2::ggplot(AOU_count, aes(x = English_Common_Name,
                                                      y = Total, fill = English_Common_Name )) +
        ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ggplot2::scale_y_continuous(labels = fancy_scientific)+
        ggplot2::ggtitle(paste0(name, "年数量前16的物种")) +
        ggplot2::labs(x = "", y = "",fill="物种") +
        theme_bar

      return(AOU_count_plot)
  })
# 保存工作簿
openxlsx::saveWorkbook(top_16_workbook, every_top_path, overwrite = TRUE)
# 将plots列表分成多个子列表,每个子列表包含8个图形
sub_plots3 <- split(plot3s_list, rep(1:ceiling(length(plot3s_list)/plots3_per_img),
                                      each=plots3_per_img, length.out=length(plot3s_list)))

# 遍历每个子列表,并将子列表中的图形排列在一张图上并保存
for (i in seq_along(sub_plots3)) {
  plot3_grid <- do.call(grid.arrange, c(sub_plots3[[i]], ncol = plots3_ncol)) %>%
  ggplot2::ggsave(paste0(workflow_dir,"/pics/","species_rank", i, ".png"),.,
          width=plots3_width, height=plots3_height, units = "in", dpi = 300)
}

# 获取工作簿中所有工作表名称
sheet_names <- openxlsx::getSheetNames(every_top_path)

# 读取每个工作表的数据
all_data <- lapply(sheet_names, function(sheet_name) {
  openxlsx::read.xlsx(top_16_workbook, sheet = sheet_name)
})
# 合并所有数据并计算和
merged_data <- dplyr::bind_rows(all_data, .id = "sheet_name") %>%
  dplyr::group_by(English_Common_Name) %>%
  dplyr::summarise(total = sum(Total))

# 打印结果
print(merged_data)