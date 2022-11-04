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
df<-read.csv("result_Exp5.csv") %>%  
  arrange(betaD,xDstar) %>% 
  unique.data.frame() %>% 
  mutate(xLstar=round(1-xCstar-xDstar,2)) %>% 
  data.table() %>% 
  .[,n:=seq(.N),by=betaD]


#第1个图的可视化 xD&xC
df1<-df %>% 
  subset(xDstar>0 &xCstar>0 &xLstar==0) 

p1<-df1%>% 
  select(-n) %>% 
  gather("strategies","value",-betaD)%>% 
  mutate(strategies=fct_reorder(strategies,c(rep(1,nrow(df1)),rep(2,nrow(df1)),rep(3,nrow(df1)))))

ggplot(p1,aes(x=betaD,y=value,color=strategies))+
  geom_line()+
  scale_x_continuous(breaks = seq(min(df1$betaD),max(df1$betaD),1))+
  scale_color_discrete(labels=c("involution","sit-up","lay-down"))+
  ylab("Equilibrium Value")+
  xlab("betaD")+
  theme_few()+
  theme(legend.title = element_blank(),
        legend.direction  = "vertical",
        legend.text = element_text(size=16),
        legend.position = c(0.8,0.3))

ggsave("Exp5xDxC.png",width = 20,height = 10,units = "cm",dpi = 300)


#第2个图的可视化 xD&xL
df1<-df %>% 
  subset(xDstar>0 &xCstar==0 &xLstar>0) 

p1<-df1%>% 
  select(-n) %>% 
  gather("strategies","value",-betaD)%>% 
  mutate(strategies=fct_reorder(strategies,c(rep(1,nrow(df1)),rep(2,nrow(df1)),rep(3,nrow(df1)))))

ggplot(p1,aes(x=betaD,y=value,color=strategies))+
  geom_line()+
  scale_x_continuous(breaks = seq(min(df1$betaD),max(df1$betaD),1))+
  scale_color_discrete(labels=c("involution","sit-up","lay-down"))+
  ylab("Equilibrium Value")+
  xlab("betaD")+
  theme_few()+
  theme(legend.title = element_blank(),
        legend.direction  = "vertical",
        legend.text = element_text(size=16),
        legend.position = c(0.8,0.3))

ggsave("Exp5xDxL.png",width = 20,height = 10,units = "cm",dpi = 300)


# #第3个图的可视化 xC&xL
# df1<-df %>% 
#   subset(xDstar==0 &xCstar>0 &xLstar>0) 
# 
# p1<-df1%>% 
#   select(-n) %>% 
#   gather("strategies","value",-betaD)%>% 
#   mutate(strategies=fct_reorder(strategies,c(rep(1,nrow(df1)),rep(2,nrow(df1)),rep(3,nrow(df1)))))
# 
# ggplot(p1,aes(x=betaD,y=value,color=strategies))+
#   geom_line()+
#   scale_x_continuous(breaks = seq(min(df1$betaD),max(df1$betaD),1))+
#   scale_color_discrete(labels=c("involution","sit-up","lay-down"))+
#   ylab("Equilibrium Value")+
#   xlab("betaD")+
#   theme_few()+
#   theme(legend.title = element_blank(),
#         legend.direction  = "vertical",
#         legend.text = element_text(size=16),
#         legend.position = c(0.8,0.3))
# 
# ggsave("Exp5xCxL.png",width = 20,height = 10,units = "cm",dpi = 300)


#------------------------------------------------------------------------#
#-------------------------实验二可视化(beta)-----------------------------#
#------------------------------------------------------------------------#
df<-read.csv("result_Exp5_beta.csv") %>%  
  arrange(betaD,xDstar) %>% 
  unique.data.frame() %>% 
  mutate(xLstar=round(1-xCstar-xDstar,2)) %>% 
  data.table() %>% 
  .[,n:=seq(.N),by=betaD]


#第1个图的可视化 xD&xC
df1<-df %>% 
  subset(xDstar>0 &xCstar>0 &xLstar==0) 

p1<-df1%>% 
  select(-n) %>% 
  gather("strategies","value",-betaD)%>% 
  mutate(strategies=fct_reorder(strategies,c(rep(1,nrow(df1)),rep(2,nrow(df1)),rep(3,nrow(df1)))))

ggplot(p1,aes(x=betaD,y=value,color=strategies))+
  geom_line()+
  scale_x_continuous(breaks = seq(min(df1$betaD),max(df1$betaD),1))+
  scale_color_discrete(labels=c("involution","sit-up","lay-down"))+
  ylab("Equilibrium Value")+
  xlab("betaD")+
  theme_few()+
  theme(legend.title = element_blank(),
        legend.direction  = "vertical",
        legend.text = element_text(size=16),
        legend.position = c(0.8,0.3))

ggsave("Exp5xDxC-beta.png",width = 20,height = 10,units = "cm",dpi = 300)


#第2个图的可视化 xD&xL
df1<-df %>% 
  subset(xDstar>0 &xCstar==0 &xLstar>0) 

p1<-df1%>% 
  select(-n) %>% 
  gather("strategies","value",-betaD)%>% 
  mutate(strategies=fct_reorder(strategies,c(rep(1,nrow(df1)),rep(2,nrow(df1)),rep(3,nrow(df1)))))

ggplot(p1,aes(x=betaD,y=value,color=strategies))+
  geom_line()+
  scale_x_continuous(breaks = seq(min(df1$betaD),max(df1$betaD),1))+
  scale_color_discrete(labels=c("involution","sit-up","lay-down"))+
  ylab("Equilibrium Value")+
  xlab("betaD")+
  theme_few()+
  theme(legend.title = element_blank(),
        legend.direction  = "vertical",
        legend.text = element_text(size=16),
        legend.position = c(0.8,0.3))

ggsave("Exp5xDxL-beta.png",width = 20,height = 10,units = "cm",dpi = 300)













