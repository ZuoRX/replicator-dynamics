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

#---------------------------#
#-----1.并行运算代码测试----#
#---------------------------#

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



#-----------------------------#
#-----2.求出所有参数的结果----#
#-----------------------------#
#（1）构建参数遍历表
Temp_xy<-data.frame()
for (y in (1:50)/50) {
  x=seq(0,1-y,0.02)
  temp_xy<-data.frame(y=rep(y,length(x)),x=x)
  Temp_xy<-rbind(Temp_xy,temp_xy)
}

Temp_beta<-data.frame()
for (beta2 in seq(1.1,2,0.1)){
  for (beta1 in seq(2.5,4,0.5)) {
    temp_beta<-data.frame(beta1=beta1,beta2=beta2)
    Temp_beta<-rbind(Temp_beta,temp_beta)
  }
}

Temp_dcl<-data.frame()
for(l in seq(0.1,0.5,0.1)){
  for(c in seq(0.6,2,0.2)){
    for (d in seq(2.5,4,0.5)) {
      temp_dcl<-data.frame(d=d,c=c,l=l)
      Temp_dcl<-rbind(Temp_dcl,temp_dcl)
    }
  }
}

parameters<-data.frame(y=rep(Temp_xy$y,nrow(Temp_beta)),
                       x=rep(Temp_xy$x,nrow(Temp_beta)),
                       beta1=rep(Temp_beta$beta1,each=nrow(Temp_xy)),
                       beta2=rep(Temp_beta$beta2,each=nrow(Temp_xy))
                       )

parameters1<-data.frame()
parameters1<-data.frame(y=rep(parameters$y,nrow(Temp_dcl)),
                        x=rep(parameters$x,nrow(Temp_dcl)),
                        beta1=rep(parameters$beta1,nrow(Temp_dcl)),
                        beta2=rep(parameters$beta2,nrow(Temp_dcl)),
                        d=rep(Temp_dcl$d,each=nrow(parameters)),
                        c=rep(Temp_dcl$c,each=nrow(parameters)),
                        l=rep(Temp_dcl$l,each=nrow(parameters))
                        )


#怎么优化这个参数表？
#优化后进行多线程运算，即每个人运行一部分


#（2）关键是或者主键i进行并行运算
parameters<-cbind(parameters,i=1:nrow(parameters))





#-----------------------------------#
#-----3.求出所有参数结果的收益值----#
#-----------------------------------#
#筛选最优参数
#1.总收益：y*N*(beta1*1-d)+x*N*(beta2*1-c)+(1-y-x)*N(1-l)
#2.收益变化量
#3.收益比






































