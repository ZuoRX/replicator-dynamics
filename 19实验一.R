library(tidyverse)
library(data.table)
library(EvolutionaryGames)
library(ggthemes)
library(latex2exp)
library(ggtext)
library(BB)#无梯度优化算法DFO
library(GenSA)#模拟退火
library(reticulate)
library(plotly)
library(parallel)
library(foreach)
library(doParallel)

setwd("/home/zuo_r/involution")

source("game_killer-R3.R")

#遍历方式求解3策略

#1.设置并行程序调节M
n_core<-detectCores(logical = F)

t1<-Sys.time()
system.time({
  cl<- makeCluster(n_core)      
  registerDoParallel(cl)       #进行进程注册
  clusterEvalQ(cl,{
    result<-data.frame()
    library(tidyverse)
  }) 
  result <- foreach(
    M = 50:200,
    .combine=rbind   #返回结果的整合
  ) %dopar% get_point_by_M(M)
  stopCluster(cl)
})

t2<-Sys.time()
t2-t1 
#10.18min
fwrite(result,"Exp1/result.csv",row.names = F)

#2.数据预处理
data1<-result %>% 
  select(xD,xC,M,xD.,xC.,xL.) %>% 
  subset(xD.==0 & xC.==0 &xL.==0)

#3.抽取每个M的多个解，然后画图







