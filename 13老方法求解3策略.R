suppressPackageStartupMessages(suppressWarnings({
  library(tidyverse)
  library(data.table)
  library(progress)
  library(parallel)
  library(foreach)
  library(doParallel)
  library(BB)
  library(ggthemes)
  library(latex2exp)#图例添加公式符号
}))

setwd("/home/zuo_r/involution")
source("game_killer-R2.R")


#a1.参数构建
parameters<-data.frame()
for (xD in seq(0,1,by=0.05)) {
  xL<-seq(0,round(1-xD,2),by=0.05)
  temp<-data.frame(xD=rep(xD,length(xL)),xC=round(1-xD-xL,2),xL=xL)
  parameters<-rbind(parameters,temp)
}

#a2.求解point点
Temp<-data.frame(xD=0,xC=0,xL=0,N=0,M=0,betaD=0,betaC=0,d=0,c=0,l=0,xD.=0,xC.=0,xL.=0)
for(i in 1:nrow(parameters)){
  temp<-duplicate_dynamics3(xD=parameters$xD[i],xC=parameters$xC[i],xL=parameters$xL[i],
                            N=50,M=100,betaD=1.5,betaC=1.1,d=4,c=1,l=0.5) 
  
  Temp<-rbind(Temp,temp)
}

data<-Temp[-1,]

#a3.三维可视化找0点
library(plotly)
plot_ly(data, x = ~xD, y = ~xC, z = ~xL,
             marker = list(color = ~xC., 
                           colorscale = c('#FFE1A1', '#683531'), showscale = TRUE)) %>% 
  add_markers() %>% 
  layout(scene = list(xaxis = list(title = 'involution'),
                                   yaxis = list(title = 'sit-ups'),
                                   zaxis = list(title = 'lay down')),
        annotations = list(
          text = 'xD.',
          xref = 'paper',
          yref = 'paper',
          showarrow = FALSE
        ))

data("mtcars")
head(mtcars)






df <- data.frame(x = seq(0, 1, 0.01), y = y.) %>% 
  mutate(y1=c(y.[-1],1)) %>% 
  mutate(y0=y*y1)#通过前一项与后一项相乘是否小于0，来获取临界点ystar

#ystar<-df$x[df$y0<0]+0.005 #即横坐标，因为是以0.01为步长移动，取0.005为均值加上

ystar<-ifelse(sum(df$x[df$y0<0])==0,0,df$x[df$y0<0]+0.005)


















#----------------------------------------------#
#------------------一、实验一------------------#
#----------------------------------------------#

#----------------------------1.1 经济下行情景---调节M-------------------------------#
#1.1-a.遍历M的自定义函数
get_star_by_M = function(M){
  
  out = dfsane(c(0.1,0.8,0.1),helper)$par %>% 
    round(digits=10)
  return(data.frame(ystar=out[1],sstar=out[2],xstar=out[3],M=M))
}

#1.1-b.遍历M，收集star值
pb <- progress_bar$new(format = "  完成百分比 [:bar] :percent 执行时间 :elapsed:elapsedfull",total = 251, clear = FALSE, width= 60)

df1 = data.frame()

for (M in 50:300){
  pb$tick()
  df1=rbind(df1,get_star_by_M(M))
}

#1.1-c.可视化
df<-df1 %>% 
  gather(star,value,-4) %>% 
  mutate(star=fct_reorder(star,c(rep(1,251),rep(2,251),rep(3,251)))) %>%  #重定义图例排序
  mutate(M1=c(rep(300:50,3)))
  
ggplot(df,aes(x=M1,y=value,color=star))+
  geom_line()+
  scale_x_continuous(breaks = seq(50,300,25),
                     labels = as.character(seq(300,50,-25)))+
  scale_color_discrete(labels=c(TeX('$y^*$'),c(TeX('$s^*$'),c(TeX('$x^*$')))))+
  ylab("Equilibrium Value")+
  xlab("M")+
  theme_few()+
  theme(legend.title = element_blank())



#----------------------------1.2 经济下行情景---调节M(beta)-------------------------------#
#1.2-a.遍历M的自定义函数+beta
get_star_by_M_with_beta = function(M){
  
  out = dfsane(c(0.1,0.8,0.1),helper_with_beta)$par
  
  return(data.frame(ystar=out[1],sstar=out[2],xstar=out[3],M=M))
}

#1.2-b.遍历M，收集star值
pb <- progress_bar$new(format = "  完成百分比 [:bar] :percent 执行时间 :elapsed:elapsedfull",total = 251, clear = FALSE, width= 60)

df1 = data.frame()

for (M in 50:300){
  pb$tick()
  df1=rbind(df1,get_star_by_M_with_beta(M))
}

#1.2-c.可视化
df<-df1 %>% 
  gather(star,value,-4) %>% 
  mutate(star=fct_reorder(star,c(rep(1,251),rep(2,251),rep(3,251)))) %>%  #重定义图例排序
  mutate(M1=c(rep(300:50,3)))

ggplot(df,aes(x=M1,y=value,color=star))+
  geom_line()+
  scale_x_continuous(breaks = seq(50,300,25),
                     labels = as.character(seq(300,50,-25)))+
  scale_color_discrete(labels=c(TeX('$y^*$'),c(TeX('$s^*$'),c(TeX('$x^*$')))))+
  ylab("Equilibrium Value")+
  xlab("M")+
  theme_few()+
  theme(legend.title = element_blank())


































