load("NUSABird/2023Release_Nor/Workflow_total/1971-2022/filtered_itp_list.RData")

library(openxlsx)
library(purrr)

# 没有则创建目录
if (!dir.exists("NUSABird/2023Release_Nor/Workflow_total/1971-2022/filtered_itp_list")) {
  dir.create("NUSABird/2023Release_Nor/Workflow_total/1971-2022/filtered_itp_list")
}
purrr::map2(names(filtered_itp_list),filtered_itp_list,function(AOU,x){
  x_names<-as.data.frame(names(x))

  y<-t(as.data.frame(x))%>%
    round(0)%>%
    cbind(x_names)

  y$`names(x)`<-as.integer( y$`names(x)`)

    openxlsx::write.xlsx(y, file = paste0("NUSABird/2023Release_Nor/Workflow_total/1971-2022/filtered_itp_list/",AOU,".xlsx"))
})