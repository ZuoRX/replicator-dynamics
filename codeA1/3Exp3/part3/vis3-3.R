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

#setwd("/home/zuo_r/involution/code/2Exp3-3")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#------------------------------------------------------------------#
#-------------------------实验三可视化-----------------------------#
#------------------------------------------------------------------#
df<-read.csv("result_Exp3-3.csv")  %>%  
  arrange(l,xDstar) %>% 
  mutate(xDstar=round(xDstar,2)) %>%
  mutate(xCstar=round(xCstar,2)) %>% 
  unique.data.frame() %>% 
  mutate(xLstar=round(1-xCstar-xDstar,2))  


#第1个图的可视化  xD&xC
df1<-df %>% 
  subset(xDstar>=0 & xCstar >=0 &xLstar==0) %>% 
  data.table() %>% 
  .[,n:=seq(.N),by=l] %>% 
  subset(n==1) 

p1<-df1%>% 
  select(-n) %>% 
  gather("strategies","value",-l)%>% 
  mutate(strategies=fct_reorder(strategies,c(rep(1,nrow(df1)),rep(2,nrow(df1)),rep(3,nrow(df1)))))

ggplot(p1,aes(x=l,y=value,color=strategies))+
  geom_line()+
  scale_x_continuous(breaks = seq(min(df1$l),max(df1$l),0.1))+
  scale_color_discrete(labels=c("involution","sit-up","lay-down"))+
  ylab("Equilibrium Value")+
  xlab("c")+
  theme_few()+
  theme(legend.title = element_blank(),
        legend.direction  = "vertical",
        legend.text = element_text(size=16),
        legend.position = c(0.85,0.6))

ggsave("Exp3-3xDxC.png",width = 20,height = 10,units = "cm",dpi = 300)


#第2个图的可视化  xD&xL
df1<-df %>% 
  subset(xDstar>=0 & xCstar ==0 &xLstar>0) %>% 
  data.table() %>% 
  .[,n:=seq(.N),by=l] %>% 
  subset(n==1) 

p1<-df1%>% 
  select(-n) %>% 
  gather("strategies","value",-l)%>% 
  mutate(strategies=fct_reorder(strategies,c(rep(1,nrow(df1)),rep(2,nrow(df1)),rep(3,nrow(df1)))))

ggplot(p1,aes(x=l,y=value,color=strategies))+
  geom_line()+
  scale_x_continuous(breaks = seq(min(df1$l),max(df1$l),0.1))+
  scale_color_discrete(labels=c("involution","sit-up","lay-down"))+
  ylab("Equilibrium Value")+
  xlab("c")+
  theme_few()+
  theme(legend.title = element_blank(),
        legend.direction  = "vertical",
        legend.text = element_text(size=16),
        legend.position = c(0.85,0.6))

ggsave("Exp3-3xDxL.png",width = 20,height = 10,units = "cm",dpi = 300)




#------------------------------------------------------------------------#
#-------------------------实验三可视化(beta)-----------------------------#
#------------------------------------------------------------------------#
df<-read.csv("result_Exp3-3_beta.csv") %>%  
  arrange(l,xDstar) %>% 
  mutate(xDstar=round(xDstar,2)) %>%
  mutate(xCstar=round(xCstar,2)) %>% 
  unique.data.frame() %>% 
  mutate(xLstar=round(1-xCstar-xDstar,2))  


#第1个图的可视化  xD&xC
df1<-df %>% 
  subset(xDstar>=0 & xCstar >=0 &xLstar==0) %>% 
  data.table() %>% 
  .[,n:=seq(.N),by=l] %>% 
  subset(n==1) 

p1<-df1%>% 
  select(-n) %>% 
  gather("strategies","value",-l)%>% 
  mutate(strategies=fct_reorder(strategies,c(rep(1,nrow(df1)),rep(2,nrow(df1)),rep(3,nrow(df1)))))

ggplot(p1,aes(x=l,y=value,color=strategies))+
  geom_line()+
  scale_x_continuous(breaks = seq(min(df1$l),max(df1$l),0.1))+
  scale_color_discrete(labels=c("involution","sit-up","lay-down"))+
  ylab("Equilibrium Value")+
  xlab("c")+
  theme_few()+
  theme(legend.title = element_blank(),
        legend.direction  = "vertical",
        legend.text = element_text(size=16),
        legend.position = c(0.85,0.6))

ggsave("Exp3-3xDxC-beta.png",width = 20,height = 10,units = "cm",dpi = 300)


#第2个图的可视化  xD&xL
df1<-df %>% 
  subset(xDstar>=0 & xCstar ==0 &xLstar>0) %>% 
  data.table() %>% 
  .[,n:=seq(.N),by=l] %>% 
  subset(n==1) 

p1<-df1%>% 
  select(-n) %>% 
  gather("strategies","value",-l)%>% 
  mutate(strategies=fct_reorder(strategies,c(rep(1,nrow(df1)),rep(2,nrow(df1)),rep(3,nrow(df1)))))

ggplot(p1,aes(x=l,y=value,color=strategies))+
  geom_line()+
  scale_x_continuous(breaks = seq(min(df1$l),max(df1$l),0.1))+
  scale_color_discrete(labels=c("involution","sit-up","lay-down"))+
  ylab("Equilibrium Value")+
  xlab("c")+
  theme_few()+
  theme(legend.title = element_blank(),
        legend.direction  = "vertical",
        legend.text = element_text(size=16),
        legend.position = c(0.85,0.6))

ggsave("Exp3-3xDxL-beta.png",width = 20,height = 10,units = "cm",dpi = 300)




#第3个图的可视化  xD&xL
df1<-df %>% 
  subset(xDstar==0 & xCstar >0 &xLstar>0) 








