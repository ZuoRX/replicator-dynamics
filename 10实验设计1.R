suppressPackageStartupMessages(suppressWarnings({
  library(tidyverse)
  library(data.table)
  library(progress)
  library(parallel)
  library(foreach)
  library(doParallel)
}))

setwd("/home/zuo_r/involution")
source("replicator_dynamics3R1.R")


#----------------------------------------------#
#a.经济下行（GDP增速下降），群体如何演化？
#----------------------------------------------#

#a1.参数构建
parameters<-data.frame()
for (y in (1:50)/50) {
  x=seq(0,1-y,0.02)
  temp<-data.frame(y=rep(y,length(x)),x=x)
  parameters<-rbind(parameters,temp)
}

M<-data.frame(M=seq(50,500,50))

parameters1<-data.frame(y=rep(parameters$y,nrow(M)),
                          x=rep(parameters$x,nrow(M)),
                          M=rep(M$M,nrow(parameters)))


#a2.数据收集函数
#只需要变化x,y,M即可
get_data_by_M<-function(i){
  
  y<-parameters1$y[i]
  x<-parameters1$x[i]
  M<-parameters1$M[i]
  
  temp<-replicator_dynamics_xy(y,x,M,beta1=2,beta2=1.1,N=50,d=4,c=1,l=0.5)
  
  gc()
  
  data<-cbind(i,temp)
}

#a3.并行收集数据
t1<-Sys.time()
n_core<-detectCores(logical = F)
system.time({
  cl<- makeCluster(n_core)      
  registerDoParallel(cl)       #进行进程注册
  clusterEvalQ(cl,{
    N<-50
    result<-data.frame()
  }) 
  result <- foreach(
    i = 1:12750,
    .combine=rbind   #返回结果的整合
  ) %dopar% get_data_by_M(i)
  stopCluster(cl)
})

t2<-Sys.time()
t2-t1 

fwrite(result,"result-Exp1.csv",row.names = F)

#--------------a4.数据可视化-------------------#
#fig1:经济下行（GDP增速下降），群体如何演化？

#金昊之前三维可视化z改为M
data<-fread("result-Exp1.csv")




















