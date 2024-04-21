# BBS 同步性分析脚本

Some data analysis R script

# preparation
- getAOUName.R 用于获取AOU对应的物种名称
- getRoutes.R 对每个时间窗的数据进行清洗
- global文件夹 存放全局路径及绘图设置、读取数据
- overview.R 对每个时间窗的整体数据进行分析、绘图

## 点数据分析

- filterInterpol.R 对每个时间窗的数据进行插值
- getCorr.R 对每个时间窗的点数据进行相关性分析
- getCorrBtBRC.R 获取每个BCR总体之间的相关性
- fitCorRelation.R 对每个时间窗的相关性结果数据进行拟合
- fitCorRelationBtBRC.R 对每个BCR总体之间的相关性结果数据进行拟合
 
# dig

- prepareData 准备时空立方体的数据
- buildCube 构建时空立方体
