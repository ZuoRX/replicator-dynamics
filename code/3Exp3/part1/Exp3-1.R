suppressPackageStartupMessages(suppressWarnings({
  library(rstudioapi)
  library(tidyverse)
  library(data.table)
  library(progress)
  library(parallel)
  library(foreach)
  library(doParallel)
  library(EvolutionaryGames)
  library(ggthemes)
  library(latex2exp)
  library(ggtext)
  library(progress)
  library(plotly)
  library(RColorBrewer)
  library(BB)
}))

#setwd("/home/zuo_r/involution/code/1Exp1")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#---------------------------------------------------------------------#
#-----------------------------3.1实验一--------------------------------#
#---------------------------------------------------------------------#
source("udf_Exp3-1.R")
n_core<-detectCores(logical = F)

t1<-Sys.time()
system.time({
  cl<- makeCluster(n_core)
  registerDoParallel(cl)       #进行进程注册
  clusterEvalQ(cl,{
    result<-data.frame()
    library(tidyverse)
    source("udf_Exp3-1.R")
  })
  result <- foreach(
    d=seq(1.2,4,0.2),
    .combine=rbind   #返回结果的整合
  ) %dopar% get_star_by_d(d)
  stopCluster(cl)
})

t2<-Sys.time()
t2-t1

write.csv(result,"result_Exp3-1.csv",row.names = F)

#------------------------------------------------------------------------------#
#-----------------------------3.1实验二（beta）--------------------------------#
#------------------------------------------------------------------------------#
source("udf_Exp3-1.R")

n_core<-detectCores(logical = F)

t1<-Sys.time()
system.time({
  cl<- makeCluster(n_core)
  registerDoParallel(cl)       #进行进程注册
  clusterEvalQ(cl,{
    result<-data.frame()
    library(tidyverse)
    source("udf_Exp3-1.R")
  })
  result <- foreach(
    d=seq(1.2,4,0.2),
    .combine=rbind   #返回结果的整合
  ) %dopar% get_star_by_d_beta(d)
  stopCluster(cl)
})

t2<-Sys.time()
t2-t1
#3.575636 hours

write.csv(result,"result_Exp3-1_beta.csv",row.names = F)












