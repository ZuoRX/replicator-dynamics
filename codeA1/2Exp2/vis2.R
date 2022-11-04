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

#setwd("/home/zuo_r/involution/code/2Exp2")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#------------------------------------------------------------------#
#-------------------------实验二可视化-----------------------------#
#------------------------------------------------------------------#
df<-read.csv("result_Exp2.csv") %>%  
  arrange(N,xDstar) %>% 
  unique.data.frame() %>% 
  mutate(xLstar=round(1-xCstar-xDstar,2)) %>% 
  data.table() %>% 
  .[,n:=seq(.N),by=N]


#第1个图的可视化 xD&xC
df1<-df %>% 
  subset(xDstar>0 &xCstar>0) 

p1<-df1%>% 
  select(-n) %>% 
  gather("strategies","value",-N)%>% 
  mutate(strategies=fct_reorder(strategies,c(rep(1,nrow(df1)),rep(2,nrow(df1)),rep(3,nrow(df1)))))

ggplot(p1,aes(x=N,y=value,color=strategies))+
  geom_line()+
  scale_x_continuous(breaks = seq(min(df1$N),max(df1$N),5))+
  scale_color_discrete(labels=c("involution","sit-up","lay-down"))+
  ylab("Equilibrium Value")+
  xlab("N")+
  theme_few()+
  theme(legend.title = element_blank(),
        legend.direction  = "horizontal",
        legend.text = element_text(size=16),
        legend.position = c(0.3,0.9))

ggsave("Exp2-1.png",width = 20,height = 10,units = "cm",dpi = 300)


#第2个图的可视化 xD&xL
df1<-df %>% 
  subset(xLstar>0 & xDstar>0) 

p1<-df1%>% 
  select(-n) %>% 
  gather("strategies","value",-N)%>% 
  mutate(strategies=fct_reorder(strategies,c(rep(1,nrow(df1)),rep(2,nrow(df1)),rep(3,nrow(df1)))))

ggplot(p1,aes(x=N,y=value,color=strategies))+
  geom_line()+
  scale_x_continuous(breaks = seq(min(df1$N),max(df1$N),10))+
  scale_color_discrete(labels=c("involution","sit-up","lay-down"))+
  ylab("Equilibrium Value")+
  xlab("N")+
  theme_few()+
  theme(legend.title = element_blank(),
        legend.direction  = "vertical",#"horizontal" or "vertical"
        legend.text = element_text(size=16),
        legend.position = c(0.85,0.7))

ggsave("Exp2-2.png",width = 20,height = 10,units = "cm",dpi = 300)

#第3个图的可视化 xC&xL
df1<-df %>% 
  subset(xLstar>0 & xCstar>0) 

p1<-df1%>% 
  select(-n) %>% 
  gather("strategies","value",-N)%>% 
  mutate(strategies=fct_reorder(strategies,c(rep(1,nrow(df1)),rep(2,nrow(df1)),rep(3,nrow(df1)))))

ggplot(p1,aes(x=N,y=value,color=strategies))+
  geom_line()+
  scale_x_continuous(breaks = seq(min(df1$N),max(df1$N),10))+
  scale_color_discrete(labels=c("involution","sit-up","lay-down"))+
  ylab("Equilibrium Value")+
  xlab("N")+
  theme_few()+
  theme(legend.title = element_blank(),
        legend.direction  = "vertical",#"horizontal" or "vertical"
        legend.text = element_text(size=16),
        legend.position = c(0.85,0.7))

ggsave("Exp2-3.png",width = 20,height = 10,units = "cm",dpi = 300)

#------------------------------------------------------------------------#
#-------------------------实验二可视化(beta)-----------------------------#
#------------------------------------------------------------------------#
df<-read.csv("result_Exp2_beta.csv") %>%  
  arrange(N,xDstar) %>% 
  unique.data.frame() %>% 
  mutate(xLstar=round(1-xCstar-xDstar,2)) %>% 
  data.table() %>% 
  .[,n:=seq(.N),by=N]


#第1个图的可视化 xD&xC
df1<-df %>% 
  subset(xDstar>0 &xCstar>0)%>% 
  data.table() %>% 
  .[,n:=seq(.N),by=N] %>% 
  subset(n==1)

p1<-df1%>% 
  select(-n) %>% 
  gather("strategies","value",-N)%>% 
  mutate(strategies=fct_reorder(strategies,c(rep(1,nrow(df1)),rep(2,nrow(df1)),rep(3,nrow(df1)))))

ggplot(p1,aes(x=N,y=value,color=strategies))+
  geom_line()+
  scale_x_continuous(breaks = seq(min(df1$N),max(df1$N),5))+
  scale_color_discrete(labels=c("involution","sit-up","lay-down"))+
  ylab("Equilibrium Value")+
  xlab("N")+
  theme_few()+
  theme(legend.title = element_blank(),
        legend.direction  = "horizontal",
        legend.text = element_text(size=16),
        legend.position = c(0.3,0.9))

ggsave("Exp2-1-beta.png",width = 20,height = 10,units = "cm",dpi = 300)


#第2个图的可视化 xD&xL
df1<-df %>% 
  subset(xLstar>0 & xDstar>0 ) %>% 
  data.table() %>% 
  .[,n:=seq(.N),by=N] %>% 
  subset(n==1 & N!=35)

p1<-df1%>% 
  select(-n) %>% 
  gather("strategies","value",-N)%>% 
  mutate(strategies=fct_reorder(strategies,c(rep(1,nrow(df1)),rep(2,nrow(df1)),rep(3,nrow(df1)))))

ggplot(p1,aes(x=N,y=value,color=strategies))+
  geom_line()+
  scale_x_continuous(breaks = seq(min(df1$N),max(df1$N),10))+
  scale_color_discrete(labels=c("involution","sit-up","lay-down"))+
  ylab("Equilibrium Value")+
  xlab("N")+
  theme_few()+
  theme(legend.title = element_blank(),
        legend.direction  = "vertical",#"horizontal" or "vertical"
        legend.text = element_text(size=16),
        legend.position = c(0.85,0.7))

ggsave("Exp2-2-beta.png",width = 20,height = 10,units = "cm",dpi = 300)

#第3个图的可视化 xC&xL
df1<-df %>% 
  subset(xLstar>0 & xCstar>0) %>% 
  data.table() %>% 
  .[,n:=seq(.N),by=N] %>% 
  subset(n==1 &N!=35)

p1<-df1%>% 
  select(-n) %>% 
  gather("strategies","value",-N)%>% 
  mutate(strategies=fct_reorder(strategies,c(rep(1,nrow(df1)),rep(2,nrow(df1)),rep(3,nrow(df1)))))

ggplot(p1,aes(x=N,y=value,color=strategies))+
  geom_line()+
  scale_x_continuous(breaks = seq(min(df1$N),max(df1$N),10))+
  scale_color_discrete(labels=c("involution","sit-up","lay-down"))+
  ylab("Equilibrium Value")+
  xlab("N")+
  theme_few()+
  theme(legend.title = element_blank(),
        legend.direction  = "vertical",#"horizontal" or "vertical"
        legend.text = element_text(size=16),
        legend.position = c(0.85,0.7))

ggsave("Exp2-3-beta.png",width = 20,height = 10,units = "cm",dpi = 300)














