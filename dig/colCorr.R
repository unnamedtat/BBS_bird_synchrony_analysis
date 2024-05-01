library(openxlsx)
library(dplyr)
library(stringr)


dir_path <- "NUSABird/2023Release_Nor/Workflow_total/1971-2022/cube_analysis/出图/聚类"
file_path <- file.path(dir_path,"聚类平均值.xlsx")

workbook <- openxlsx::loadWorkbook(file_path)

# 创建一个空的数据框用于存储结果
results <- data.frame(Sheet = character(), Cluster1 = character(), Cluster2 = character(), Corr = numeric(), Pvalue = numeric(), stringsAsFactors = FALSE)

for (sheet in names(workbook)) {
  data <- read.xlsx(workbook, sheet = sheet)

  cluster_cols <- names(data)

  if (length(cluster_cols) > 1) {
    combns<- combn(cluster_cols, 2)

    for(col in 1:ncol(combns)){
        col1<-combns[1,col]
        col2<-combns[2,col]
        c1 <- data[[col1]]
        c2 <- data[[col2]]
        cor_test <- cor.test(c1,c2, method = "pearson")
        results <- rbind(results, data.frame(Sheet = sheet, Cluster1 = col1, Cluster2 = col2,
                                             Corr = cor_test$estimate, Pvalue = cor_test$p.value,
                                             stringsAsFactors = FALSE))
    }
  } else {
    cat(paste0("工作表 ", sheet, " 不包含至少两个以'聚类'开头并以数字结尾的列\n\n"))
  }
}

# 输出结果数据框
print(results)

# 将结果保存到文件
write.xlsx(results, file = file.path(dir_path,"聚类相关性.xlsx"))