suppressPackageStartupMessages(suppressWarnings({
  library(tidyverse)
  library(data.table)
  library(progress)
  library(parallel)
  library(foreach)
  library(doParallel)
}))

setwd("/home/zuo_r/involution")
source("replicator_dynamics3.R")
n_core<-detectCores(logical = F)

parameters1<-fread("parameters1.csv") %>% 
  .[10001:30000,]

t1<-Sys.time()
system.time({
  cl<- makeCluster(n_core)      
  registerDoParallel(cl)       #进行进程注册
  clusterEvalQ(cl,{
    N<-50
    result<-data.frame()
  }) 
  result <- foreach(
    i = 1:100000,
    .combine=rbind   #返回结果的整合
  ) %dopar% get_data_by_parameters1(i)
  stopCluster(cl)
})

t2<-Sys.time()
t2-t1 

write.csv(result,"result1-10.csv",row.names = F)

#每次运行完，restart R,来抽出内存






