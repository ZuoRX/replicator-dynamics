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

#----------------------------------------------#
#------------------一、实验一------------------#
#----------------------------------------------#

#----------------------------1.1 经济下行情景---调节M-------------------------------#
#1.1-a.遍历M的自定义函数
get_star_by_M = function(M){
  
  out = dfsane(c(0.1,0.8,0.1),helper)$par %>% 
    round(digits=3)
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


































