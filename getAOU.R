# 首先，使用 readLines 读取txt文件的内容
lines <- readLines("NUSABird/2023Release_Nor/AOU.txt")

# 提取出所有字段，忽略空行
lines <- lines[nzchar(lines)]

# 定义一个分割数据的函数
split_data <- function(x) {
  # 去除行首的空格
  x <- trimws(x, "left")

  # 使用单个空格分割前两个字段
  fields <- strsplit(x, " ", fixed = TRUE)[[1]]
  first_two <- fields[1:2]

  # 从原始字符串中删除前两个字段
  x <- gsub(paste0(first_two[1], " ", first_two[2]), "", x, fixed = TRUE)
  x <- trimws(x, "left") # 去掉前导空格

  # 将剩余部分按三个或更多空格进行分割
  remaining_fields <- unlist(strsplit(x, "\\s{4,}", perl = TRUE))
  remaining_fields <- remaining_fields[1]

  # if (length(remaining_fields) !=7) {
  #   print(first_two)
  #   print(length(remaining_fields) )
  #   # print(remaining_fields)
  # }
  return(c(first_two, remaining_fields))
}

data_list <- lapply(lines, split_data)

# 现在我们需要构造一个数据框来保存这些数据
df <- do.call(rbind.data.frame, data_list)

# 添加每一列的名称


colnames(df) <- c("Seq", "AOU", "English_Common_Name")
                  # "French_Common_Name", "Spanish_Common_Name",
                  # "ORDER", "Family", "Genus", "Species")

# 最后，我们将这个数据框保存为csv文件
write.csv(df, "NUSABird/2023Release_Nor/Workflow/NAME.csv", row.names = FALSE)
