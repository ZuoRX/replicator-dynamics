library(tidyverse)
library(data.table)
library(EvolutionaryGames)
library(ggthemes)
library(latex2exp)
library(ggtext)
library(BB)#无梯度优化算法DFO
library(GenSA)#模拟退火
library(reticulate)
library(parallel)
library(foreach)
library(doParallel)
so<-import("scipy.optimize")

setwd("/home/zuo_r/involution")

#--------------------------------------------------------------#
#------------------测试三策略对两策略的兼容性------------------#
#--------------------------------------------------------------#
source("game_killer-R4.R")
M<-200
#beta=1,d=4,N=100,c=1
bounds = list(c(0.01,0.99))
k<-as.integer(1000)

result<-so$dual_annealing(helper_dual_test,bounds,maxiter = k)
result$x %>% 
  round(4)


#--------------------------------------------------------------#
#---------------------------三策略求解-------------------------#
#--------------------------------------------------------------#
#问题：三个策略的稳定解？
source("game_killer-R4.R")



M<-100
bounds = list(c(0.01,0.99),c(0.01,0.99))
k<-as.integer(1000)

result<-so$dual_annealing(helper_dual,bounds,maxiter = k)
result$x %>% 
  round(2)

result1<-so$dual_annealing(helper_dual,bounds,maxiter = k)
result1$x%>% 
  round(2)

result2<-so$dual_annealing(helper_dual,bounds,maxiter = k)
result2$x%>% 
  round(2)

result3<-so$dual_annealing(helper_dual,bounds,maxiter = k)
result3$x%>% 
  round(2)


#--------------------------------------------------------------#
#--------------------三策略求解-并行运算-----------------------#
#--------------------------------------------------------------#
get_star_by_M_dual<-function(M){
  M<-M
  
  bounds = list(c(0.01,0.99),c(0.01,0.99))
  k<-as.integer(1000)
  
  result<-so$dual_annealing(helper_dual,bounds,maxiter = k)
  r1<-result$x %>% 
    round(2)
  
  result1<-so$dual_annealing(helper_dual,bounds,maxiter = k)
  r2<-result1$x%>% 
    round(2)
  
  result2<-so$dual_annealing(helper_dual,bounds,maxiter = k)
  r3<-result2$x%>% 
    round(2)
  
  result3<-so$dual_annealing(helper_dual,bounds,maxiter = k)
  r4<-result3$x%>% 
    round(2)
  
  rs<-rbind(r1,r2,r3,r4) %>% 
    data.frame() %>% {
    names(.)<-c("involution","sit_up")
    .
  } %>% 
    distinct(involution,sit_up) %>% 
    cbind(M=rep(M,nrow(.)))
  
  return(rs)
}

result<-data.frame()
for(M in 50:100){
  print(M)
  temp<-get_star_by_M_dual(M)
  result<-rbind(result,temp)
}

write.csv(result,"Exp1/resultM50-100.csv",row.names = F)



# #1.设置并行程序调节M
# n_core<-detectCores(logical = F)
# 
# t1<-Sys.time()
# system.time({
#   cl<- makeCluster(n_core)      
#   registerDoParallel(cl)       #进行进程注册
#   clusterEvalQ(cl,{
#     result<-data.frame()
#     library(reticulate)
#     so<-import("scipy.optimize")
#     library(tidyverse)
#   }) 
#   result <- foreach(
#     M = 50:52,
#     .combine=rbind   #返回结果的整合
#   ) %dopar% get_star_by_M_dual(M)
#   stopCluster(cl)
# })
# 
# t2<-Sys.time()
# t2-t1 





















