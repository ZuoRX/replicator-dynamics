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

setwd("/home/zuo_r/involution")

source("game_killer-R3.R")

#----------------------------------------------------------------------------------------#
#-----------------------------------收集横截面数据---------------------------------------#
#----------------------------------------------------------------------------------------#
#1.构建参数集
parameters<-data.frame(xD=rep(seq(0,1,0.01),each=101),xC=rep(seq(0,1,0.01),101)) %>%
  mutate(Add=xD+xC) %>% 
  mutate(xD=case_when(Add>1~0.3,T~xD)) %>% 
  mutate(xC=case_when(Add>1~0.3,T~xC))

#2.求解收集
get_data_by_M<-function(M){
  Temp<-data.frame(xD=0,xC=0,xL=0,N=0,M=0,betaD=0,betaC=0,d=0,c=0,l=0,xD.=0,xC.=0,xL.=0)
  for(i in 1:nrow(parameters)){
    temp<-duplicate_dynamics3(xD=parameters$xD[i],xC=parameters$xC[i],
                              N=50,M=M,betaD=1.5,betaC=1.1,d=4,c=1,l=0.5) 
    
    Temp<-rbind(Temp,temp)
  }
  
  data<-Temp[-1,] %>% 
    subset(xD.==0 |xC.==0 |xL.==0)
}


#3.设置并行程序调节M
n_core<-detectCores(logical = F)

t1<-Sys.time()
system.time({
  cl<- makeCluster(n_core)
  registerDoParallel(cl)       #进行进程注册
  clusterEvalQ(cl,{
    result<-data.frame()
    library(tidyverse)
    source("game_killer-R3.R")
  })
  result <- foreach(
    M = 50:200,
    .combine=rbind   #返回结果的整合
  ) %dopar% get_data_by_M(M)
  stopCluster(cl)
})

t2<-Sys.time()
t2-t1
#服务器10min

#----------------------------------------------------------------------------------------#
#-----------------------------------截面数据可视化---------------------------------------#
#----------------------------------------------------------------------------------------#
#1.xD.
df<-result %>% 
  subset(xD.==0) %>% 
  select(xD,xC,M)

p1<-plot_ly(x = ~df$xD, y = ~df$xC, z = ~as.vector(df$M), type = 'mesh3d') %>% 
  add_markers()

p1
htmlwidgets::saveWidget(as_widget(p1), "Exp1_single/p1_xD.html")


#2.xC.
df<-result %>% 
  subset(xC.==0) %>% 
  select(xD,xC,M)

p2<-plot_ly(x = ~df$xD, y = ~df$xC, z = ~as.vector(df$M), type = 'mesh3d') %>% 
  add_markers()

p2
htmlwidgets::saveWidget(as_widget(p2), "Exp1_single/p2_xC.html")


#3.xL.
df<-result %>% 
  subset(xL.==0) %>% 
  select(xD,xC,M)

p3<-plot_ly(x = ~df$xD, y = ~df$xC, z = ~as.vector(df$M), type = 'mesh3d') %>% 
  add_markers()

p3
htmlwidgets::saveWidget(as_widget(p3), "Exp1_single/p3_xL.html")
















