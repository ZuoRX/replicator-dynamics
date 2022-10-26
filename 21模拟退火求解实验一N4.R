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

#----------------------------------------------------------------#
#--------------------三策略求解N4-批量运算-----------------------#
#----------------------------------------------------------------#
get_star_by_M_dual_N4<-function(M){
  M<-M
  
  bounds = list(c(0.01,0.99),c(0.01,0.99))
  k<-as.integer(1000)
  
  result<-so$dual_annealing(helper_dual_N4,bounds,maxiter = k)
  r1<-result$x %>% 
    round(2)
  
  result1<-so$dual_annealing(helper_dual_N4,bounds,maxiter = k)
  r2<-result1$x%>% 
    round(2)
  
  result2<-so$dual_annealing(helper_dual_N4,bounds,maxiter = k)
  r3<-result2$x%>% 
    round(2)
  
  result3<-so$dual_annealing(helper_dual_N4,bounds,maxiter = k)
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

a<-Sys.time()
result<-data.frame()
for(M in 1:50){
  print(M)
  temp<-get_star_by_M_dual_N4(M)
  result<-rbind(result,temp)
}
b<-Sys.time()
b-a

write.csv(result,"Exp1/resultN4M1-50.csv",row.names = F)
#测试N=4的结果也没有三种稳定

















