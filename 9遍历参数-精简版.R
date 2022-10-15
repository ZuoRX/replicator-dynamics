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

parameters1<-fread("data/parameters1.csv") %>% 
  .[1:1000000,]

gc()

t1<-Sys.time()
system.time({
  cl<- makeCluster(n_core)      
  registerDoParallel(cl)       #进行进程注册
  clusterEvalQ(cl,{
    N<-50
    result<-data.frame()
  }) 
  result <- foreach(
    i = 1:1000000,
    .combine=rbind   #返回结果的整合
  ) %dopar% get_data_by_parameters1(i)
  stopCluster(cl)
})

t2<-Sys.time()
t2-t1 

fwrite(result,"result1-100.csv",row.names = F)



#每次运行完，restart R,来抽出内存

####################################
############数据合并################
####################################

# result1<-fread("result1-10.csv")
# result2<-fread("result11-80.csv")
# result3<-fread("result81-180.csv")
# result4<-fread("result181-250.csv")
# result5<-fread("result251-300.csv")
# result6<-fread("result301-400.csv")
# result7<-fread("result401-504.csv")
# 
# result<-rbind(result1,result2,result3,result4,result5,result6,result7)
# 
# fwrite(result,"result-all.csv",row.names = F)
















