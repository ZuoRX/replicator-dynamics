suppressPackageStartupMessages(suppressWarnings({
  library(rstudioapi)
  library(tidyverse)
  library(data.table)
  library(progress)
  library(parallel)
  library(foreach)
  library(doParallel)
  library(EvolutionaryGames)
  library(ggthemes)
  library(latex2exp)
  library(ggtext)
  library(progress)
  library(plotly)
  library(RColorBrewer)
  library(BB)
}))

#setwd("/home/zuo_r/involution/code/2Exp4")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#------------------------------------------------------------------#
#-------------------------实验三可视化-----------------------------#
#------------------------------------------------------------------#
df<-read.csv("result_Exp4.csv") %>%  
  arrange(t,xDstar) %>% 
  mutate(xDstar=round(xDstar,2)) %>%
  mutate(xCstar=round(xCstar,2)) %>% 
  unique.data.frame() %>% 
  mutate(xLstar=round(1-xCstar-xDstar,2))
  
#第1个图的可视化 xD&xC
df1<-df %>% 
  subset(xDstar>=0 & xCstar >=0 &xLstar==0) %>% 
  data.table() %>% 
  .[,n:=seq(.N),by=t] %>% 
  subset(n==1) 

p1<-df1%>% 
  select(-n) %>% 
  gather("strategies","value",-3)%>% 
  mutate(strategies=fct_reorder(strategies,c(rep(1,nrow(df1)),rep(2,nrow(df1)),rep(3,nrow(df1))))) %>%  #重定义图例排序
  mutate(t1=c(rep(df1$t[order(df1$t,decreasing = T)],3)))


ggplot(p1,aes(x=t1,y=value,color=strategies))+
  geom_line()+
  scale_x_continuous(breaks = seq(min(df1$t),max(df1$t),0.05),#<-----
                     labels = as.character(seq(max(df1$t),min(df1$t),-0.05)))+
  scale_color_discrete(labels=c("involution","sit-up","lay-down"))+
  ylab("Equilibrium Value")+
  xlab("t")+
  theme_few()+
  theme(legend.title = element_blank(),
        legend.direction  = "horizontal",
        legend.text = element_text(size=16),
        legend.position = c(0.3,0.9))

ggsave("Exp4xD&xC.png",width = 20,height = 10,units = "cm",dpi = 300)

#第2个图的可视化 xD&xL
df1<-df %>% 
  subset(xDstar>=0 & xCstar ==0 &xLstar>=0) %>% 
  data.table() %>% 
  .[,n:=seq(.N),by=t] %>% 
  subset(n==1) 

p1<-df1%>% 
  select(-n) %>% 
  gather("strategies","value",-3)%>% 
  mutate(strategies=fct_reorder(strategies,c(rep(1,nrow(df1)),rep(2,nrow(df1)),rep(3,nrow(df1))))) %>%  #重定义图例排序
  mutate(t1=c(rep(df1$t[order(df1$t,decreasing = T)],3)))


ggplot(p1,aes(x=t1,y=value,color=strategies))+
  geom_line()+
  scale_x_continuous(breaks = seq(min(df1$t),max(df1$t),0.05),#<-----
                     labels = as.character(seq(max(df1$t),min(df1$t),-0.05)))+
  scale_color_discrete(labels=c("involution","sit-up","lay-down"))+
  ylab("Equilibrium Value")+
  xlab("t")+
  theme_few()+
  theme(legend.title = element_blank(),
        legend.direction  = "horizontal",
        legend.text = element_text(size=16),
        legend.position = c(0.3,0.9))

ggsave("Exp4xD&xL.png",width = 20,height = 10,units = "cm",dpi = 300)

# #第3个图的可视化 xC&xL
# df1<-df %>% 
#   subset(xDstar==0 & xCstar >=0 &xLstar>=0) %>% 
#   data.table() %>% 
#   .[,n:=seq(.N),by=t] %>% 
#   subset(n==1) 
# 
# p1<-df1%>% 
#   select(-n) %>% 
#   gather("strategies","value",-3)%>% 
#   mutate(strategies=fct_reorder(strategies,c(rep(1,nrow(df1)),rep(2,nrow(df1)),rep(3,nrow(df1))))) %>%  #重定义图例排序
#   mutate(t1=c(rep(df1$t[order(df1$t,decreasing = T)],3)))
# 
# 
# ggplot(p1,aes(x=t1,y=value,color=strategies))+
#   geom_line()+
#   scale_x_continuous(breaks = seq(min(df1$t),max(df1$t),0.05),#<-----
#                      labels = as.character(seq(max(df1$t),min(df1$t),-0.05)))+
#   scale_color_discrete(labels=c("involution","sit-up","lay-down"))+
#   ylab("Equilibrium Value")+
#   xlab("t")+
#   theme_few()+
#   theme(legend.title = element_blank(),
#         legend.direction  = "horizontal",
#         legend.text = element_text(size=16),
#         legend.position = c(0.3,0.9))
# 
# ggsave("Exp4xD&xL.png",width = 20,height = 10,units = "cm",dpi = 300)


#------------------------------------------------------------------------#
#-------------------------实验三可视化(beta)-----------------------------#
#------------------------------------------------------------------------#
df<-read.csv("result_Exp4_beta.csv")%>%  
  arrange(t,xDstar) %>% 
  mutate(xDstar=round(xDstar,2)) %>%
  mutate(xCstar=round(xCstar,2)) %>% 
  unique.data.frame() %>% 
  mutate(xLstar=round(1-xCstar-xDstar,2))

#第1个图的可视化 xD&xC
df1<-df %>% 
  subset(xDstar>=0 & xCstar >=0 &xLstar==0) %>% 
  data.table() %>% 
  .[,n:=seq(.N),by=t] %>% 
  subset(n==1) 

p1<-df1%>% 
  select(-n) %>% 
  gather("strategies","value",-3)%>% 
  mutate(strategies=fct_reorder(strategies,c(rep(1,nrow(df1)),rep(2,nrow(df1)),rep(3,nrow(df1))))) %>%  #重定义图例排序
  mutate(t1=c(rep(df1$t[order(df1$t,decreasing = T)],3)))


ggplot(p1,aes(x=t1,y=value,color=strategies))+
  geom_line()+
  scale_x_continuous(breaks = seq(min(df1$t),max(df1$t),0.05),#<-----
                     labels = as.character(seq(max(df1$t),min(df1$t),-0.05)))+
  scale_color_discrete(labels=c("involution","sit-up","lay-down"))+
  ylab("Equilibrium Value")+
  xlab("t")+
  theme_few()+
  theme(legend.title = element_blank(),
        legend.direction  = "horizontal",
        legend.text = element_text(size=16),
        legend.position = c(0.3,0.9))

ggsave("Exp4xD&xC-beta.png",width = 20,height = 10,units = "cm",dpi = 300)

#第2个图的可视化 xD&xL
df1<-df %>% 
  subset(xDstar>=0 & xCstar ==0 &xLstar>=0) %>% 
  data.table() %>% 
  .[,n:=seq(.N),by=t] %>% 
  subset(n==1) 

p1<-df1%>% 
  select(-n) %>% 
  gather("strategies","value",-3)%>% 
  mutate(strategies=fct_reorder(strategies,c(rep(1,nrow(df1)),rep(2,nrow(df1)),rep(3,nrow(df1))))) %>%  #重定义图例排序
  mutate(t1=c(rep(df1$t[order(df1$t,decreasing = T)],3)))


ggplot(p1,aes(x=t1,y=value,color=strategies))+
  geom_line()+
  scale_x_continuous(breaks = seq(min(df1$t),max(df1$t),0.05),#<-----
                     labels = as.character(seq(max(df1$t),min(df1$t),-0.05)))+
  scale_color_discrete(labels=c("involution","sit-up","lay-down"))+
  ylab("Equilibrium Value")+
  xlab("t")+
  theme_few()+
  theme(legend.title = element_blank(),
        legend.direction  = "horizontal",
        legend.text = element_text(size=16),
        legend.position = c(0.3,0.9))

ggsave("Exp4xD&xL-beta.png",width = 20,height = 10,units = "cm",dpi = 300)


#第3个图的可视化 xD&xL
df1<-df %>% 
  subset(xDstar==0 & xCstar >=0 &xLstar>=0) %>% 
  data.table() %>% 
  .[,n:=seq(.N),by=t] %>% 
  subset(n==1) 

p1<-df1%>% 
  select(-n) %>% 
  gather("strategies","value",-3)%>% 
  mutate(strategies=fct_reorder(strategies,c(rep(1,nrow(df1)),rep(2,nrow(df1)),rep(3,nrow(df1))))) %>%  #重定义图例排序
  mutate(t1=c(rep(df1$t[order(df1$t,decreasing = T)],3)))


ggplot(p1,aes(x=t1,y=value,color=strategies))+
  geom_line()+
  scale_x_continuous(breaks = seq(min(df1$t),max(df1$t),0.05),#<-----
                     labels = as.character(seq(max(df1$t),min(df1$t),-0.05)))+
  scale_color_discrete(labels=c("involution","sit-up","lay-down"))+
  ylab("Equilibrium Value")+
  xlab("t")+
  theme_few()+
  theme(legend.title = element_blank(),
        legend.direction  = "horizontal",
        legend.text = element_text(size=16),
        legend.position = c(0.3,0.9))

ggsave("Exp4xD&xL-beta.png",width = 20,height = 10,units = "cm",dpi = 300)

