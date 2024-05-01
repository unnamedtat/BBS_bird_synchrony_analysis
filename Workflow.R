# source("NUSABird/2023Release_Nor/Script/prepare/getAOUName.R")
source("NUSABird/2023Release_Nor/Script/prepare/getRoutes.R")
###########发现以下脚本逻辑会对文件夹总所有指定的时间跨度执行##########
source("NUSABird/2023Release_Nor/Script/prepare/overview.R")
#插值
source("NUSABird/2023Release_Nor/Script/InterpolandCorr/filterInterpol.R")
#计算相关性指数
source("NUSABird/2023Release_Nor/Script/InterpolandCorr/getCorr.R")
source("NUSABird/2023Release_Nor/Script/InterpolandCorr/getCorrBtBCR.R")
#拟合相关性指数
source("NUSABird/2023Release_Nor/Script/InterpolandCorr/fitCorRelation.R")
source("NUSABird/2023Release_Nor/Script/InterpolandCorr/fitCorrBtBCR.R")