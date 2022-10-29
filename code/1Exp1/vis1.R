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

#setwd("/home/zuo_r/involution/code/1Exp1")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

result<-read.csv("result_Exp1_beta.csv")

#可视化
df<-result %>% 
  subset(x>=0 & y >=0) %>% 
  arrange(M,x) %>% 
  mutate(xDstar=round(x,2)) %>%
  mutate(xCstar=round(y,2)) %>% 
  select(-N,-x,-y) %>% 
  unique.data.frame() %>% 
  mutate(xLstar=round(1-xCstar-xDstar,2)) %>% 
  data.table() %>% 
  .[,n:=seq(.N),by=M]

max(df$n)

df1<-df %>% 
  subset(n==1) %>% 
  select(-n) %>% 
  gather("stratagies","value",-1)

ggplot(df1,aes(x=M,y=value,color=stratagies))+
  geom_line() + 
  theme_few()