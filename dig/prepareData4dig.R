data_path <- "NUSABird/2023Release_Nor/Workflow_window/1971-1988"

load(file.path(data_path, "filtered_itp_list.RData"))

library(openxlsx)
library(purrr)

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