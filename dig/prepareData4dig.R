library(openxlsx)
library(purrr)

workflow_dir <- "NUSABird/2023Release_Nor/Workflow_window"
sub_dirs <- list.dirs(workflow_dir, recursive = TRUE, full.names = TRUE)

for(data_path in sub_dirs){
  if (file.exists(file.path(data_path, "filtered_itp_list.RData"))){
    load(file.path(data_path, "filtered_itp_list.RData"))
    prepared_data_path <- file.path(data_path, "filtered_itp_list")
    # 没有则创建目录
    if (!dir.exists(prepared_data_path) ){
      dir.create(prepared_data_path)
    }
    purrr::map2(names(filtered_itp_list),filtered_itp_list,function(AOU,x){
      x_names<-as.data.frame(names(x))

      y<-t(as.data.frame(x))%>%
        round(0)%>%
        cbind(x_names)

      y$`names(x)`<-as.integer( y$`names(x)`)

        openxlsx::write.xlsx(y, file = file.path(prepared_data_path,paste0(AOU,".xlsx")))
    })
  }
}

