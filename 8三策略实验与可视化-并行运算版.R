suppressPackageStartupMessages(suppressWarnings({
  library(tidyverse)
  library(data.table)
  library(EvolutionaryGames)
  library(ggthemes)
  library(latex2exp)
  library(ggtext)
  library(progress)
  library(plotly)
  library(RColorBrewer)
  library(parallel)
  library(foreach)
  library(doParallel)
}))


setwd("/home/zuo_r/involution")

source("replicator_dynamics3.R")

#查看本地核数
n_core<-detectCores(logical = F)



#（1）构建参数遍历表
parameters<-data.frame()
for (y in (1:50)/50) {
  x=seq(0,1-y,0.02)
  temp<-data.frame(y=rep(y,length(x)),x=x)
  parameters<-rbind(parameters,temp)
}

#（2）关键是或者主键i进行并行运算
parameters<-cbind(parameters,i=1:nrow(parameters))

#（3）借助foreach包进行并行运算；
t1<-Sys.time()
system.time({
  cl<- makeCluster(n_core)      
  registerDoParallel(cl)       #进行进程注册
  clusterEvalQ(cl,{
    N<-50
    M<-100
    beta1<-4
    beta2<-2
    d<-4
    c<-1
    l<-0.5
    temp<-data.frame()
  }) 
  result <- foreach(
    i = 1:1275,
    .combine=rbind   #返回结果的整合
  ) %dopar% get_data_by_yx(i)
  stopCluster(cl)
})

t2<-Sys.time()
t2-t1 


# （4）测试结果：
# 单核运算：16s
# 并行运算（16核）：2.3s【效率提升大约7倍】


















