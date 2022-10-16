library(tidyverse)
library(data.table)
library(igraph)
library(ggnetwork)
library(progress)
library(RColorBrewer)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(poweRlaw)
setwd("/home/zuo_r/involution")

#------------------------------------------------------------------------------#
#--------------------构建一个关于加班时间的分布函数----------------------------#
#------------------------------------------------------------------------------#

# #遍历x,即alpha参数
# set.seed(0808)
# Nt<-rpldis(n=N,xmin=8,alpha = 6.52, discrete_max = 24 ) %>% 
#   data.frame() %>% 
#   purrr::set_names("t")

N=1000 #网络中的个体

#连续型
set.seed(0808)
x = seq(8, 24, length.out=N)
Nt<-rplcon(x,xmin=8,alpha = 7.5453 ) %>% 
  data.frame() %>% 
  purrr::set_names("t") %>% 
  mutate(t1=case_when(t<8.5~8,
                      t>=8.5 & t<9.5~9,
                      t>=9.5 & t<10.5~10,
                      t>=10.5 & t<11.5~11,
                      t>=11.5 & t<12.5~12,
                      t>=12.5 & t<13.5~13,
                      t>=13.5 & t<14.5~14,
                      t>=14.5 & t<15.5~15,
                      t>=15.5 & t<16.5~16,
                      t>=16.5 & t<17.5~17,
                      t>=17.5 & t<18.5~18,
                      t>=18.5 & t<19.5~19,
                      t>=19.5 & t<20.5~20,
                      t>=20.5 & t<21.5~21,
                      t>=21.5 & t<22.5~22,
                      t>=22.5 & t<23.5~23,
                      T~24)) %>% 
  mutate(strategy=case_when(t1==8~"N",# work normal time
         T~"O"))# work over time

table(Nt$t1)

#1.可视化密度图
plot(x, dplcon(x, 8, alpha= 7.5453), type="l")
max(Nt$t)
min(Nt$t)

#2.可视化散点图
df<-table(Nt$t1) %>% 
  data.frame() %>% 
  purrr::set_names(c("t","Freq"))

ggplot(df,aes(x=t,y=Freq))+
  geom_point()+
  theme_few()

# #密度图可视化
# ggplot(Nt)+
#   geom_density(aes(x=t))+
#   theme_few()+
#   #xlim(8,24)
#   scale_x_continuous(limits=c(8,24),breaks=seq(8,24,1))
# 
# 
# xmin = 1; alpha = 1.5
# x = seq(xmin, 10, length.out=1000)
# plot(x, dplcon(x, xmin, alpha), type="l")
# plot(x, pplcon(x, xmin, alpha), type="l", main="Distribution function")
# n = 1000
# con_rns = rplcon(n, xmin, alpha)
# con_rns = sort(con_rns)
# p = rep(1/n, n)
# #Zipfs plot
# plot(con_rns, rev(cumsum(p)), log="xy", type="l")
# 
# 
# #根据实际数据，计算幂律的参数
# data('moby')
# m_m<-displ$new(moby)
# m_m$getXmin()
# 
# est<-estimate_xmin(m_m)
# m_m$setXmin(est)
# est<-estimate_pars(m_m)
# plot(m_m)
# dd<-plot(m_m)
#------------------------------------------------------------------------------#

# #每个个体具有相同数量的合作伙伴，总共M条边
# #k=2M/N
# k=10    #平均度
# M<-N*k/2
# #合作者、背叛者
# b=1.2  #单方面背叛的收益，1<b<2
# w=1
# p<-0.5#切换到二阶信誉最高邻居的概率，否则为1-p


#------------------------------------------------------------------------------#
#-------------------------------构建一个网络-----------------------------------#
#------------------------------------------------------------------------------#

#构建一个网络(所有个体具有相同数量的边，随机连接到任意玩家)
set.seed(111)
#net<-erdos.renyi.game(N, p.or.m = M,type="gnm") 
#net<-sample_k_regular(N,k=k)#,directed = T如果是单向的，就不是博弈了
net<-watts.strogatz.game(1,N,nei=3,p=0.05)#<---可以调节

plot(net,layout=layout.circle(net))

net_adj<-get.adjacency(net, type="both",sparse=FALSE) %>% {
  colnames(.)<-1:ncol(.);
  .
}

# #对这个网络进行可视化
# df<-ggnetwork(net_adj)
# ggplot(df, aes(x = x, y = y, xend = xend, yend = yend)) + 
#   geom_edges(color = "lightgray", 
#              arrow = arrow(length = unit(6, "pt"), type = "closed")) + 
#   theme_blank() + 
#   guides(color=guide_colorbar(title="attitude"))+
#   scale_color_distiller(palette = "Spectral")+
#   geom_nodes( size = 4)
# ggsave("fig_raw_network.png",width=20, height=20,units="cm",dpi = 300)

#------------------------------------------------------------------------------#
#-------------------------------社会资本计算-----------------------------------#
#------------------------------------------------------------------------------#

#求节点邻居数Ni 和 所有邻居的邻居数Njj
nj<-c()
njj<-c()

for(i in 1:N){
  #邻居id
  Nj<-net_adj[i,net_adj[i,]==1] %>% 
    data.frame() %>% 
    row.names() %>% 
    as.numeric()
  
  #邻居数
  nj[i]<-length(Nj) %>% 
    as.numeric()
  
  #（自己和邻居）的邻居数
  njj[i]<-net_adj[1:N,c(i,Nj)] %>% 
    colSums() %>% 
    sum()
}

Nt1<-Nt %>% 
  mutate(nj=nj) %>% 
  mutate(njj=njj)

#策略和时间进行更新,初始值：
Strategy_time<-Nt1[,c(3,2)] %>% 
  purrr::set_names(c("strategy","t"))

#------------------------------------------------------------------------------#
#---------------------------------演化博弈-------------------------------------#
#------------------------------------------------------------------------------#
#单位时间成本
c<-1            #<---可以调节
#一个group的资源
M<-100          #<---可以调节
#fermi-dirac参数
beta<-0.01      #<---可以调节
#收集每步的策略和时间更新
#初始状态
temp0<-data.frame(step=rep(0,1000),strategy=Strategy_time$strategy,t=Strategy_time$t)
#中间状态
temp<-data.frame()
data<-data.frame()

#----------------------------------------------------------------------------#

#时间演变
pb <- progress_bar$new(format = "  完成百分比 [:bar] :percent 执行时间 :elapsed:elapsedfull",total = 1000, clear = FALSE, width= 60)

for (step in 1:1000){
  pb$tick()
  
  paiNi<-c()
  for(i in 1:N){
    
    pai_collect<-c()
  
    #1.遍历每个group的收益
    for(g in c(i,Nj)){
      
      #邻居id
      Nj<-net_adj[g,net_adj[g,]==1] %>% 
        data.frame() %>% 
        row.names() %>% 
        as.numeric()
      
      #group投入时间 随中心点变化而变化
      group_input<-sum(Strategy_time$t[c(g,Nj)]*c)
      
      #每个
      pai_temp<-((Nt1$nj[i]/Nt1$njj[g])+((Strategy_time$t[i]*c)/group_input))*M/2-Strategy_time$t[i]*c
      
      #group收益收集
      pai_collect<-c(pai_collect,pai_temp)
    }
    
    paiNi[i]<-sum(pai_collect)
  }
  
  for(i in 1:N){
    #2.更新策略
    #i的邻居
    Nj<-net_adj[i,net_adj[i,]==1] %>% 
      data.frame() %>% 
      row.names() %>% 
      as.numeric()
    #随机挑选一个邻居，不设置随机种子
    Nj_select<-Nj[sample(1:length(Nj),1,replace = F)]
    #邻居的收益
    paiNeighbor<-paiNi[Nj_select]
    #节点i的收益
    paiI<-paiNi[i]
    #邻居的策略
    S_j<-Strategy_time$strategy[Nj_select]
    #节点i的策略
    S_i<-Strategy_time$strategy[i]
    
    #策略更新概率：
    #j的策略代替i的策略的似然性概率
    p<-1/(1+exp(beta*(paiI-paiNeighbor)))
    #synchronous update 同步更新
    x<-runif(1,min=0,max=1)
    if(x<p){
      Strategy_time$strategy[i]<-Strategy_time$strategy[Nj_select]
      Strategy_time$t[i]<-Strategy_time$t[Nj_select]
    }
  }
  
  temp<-data.frame(step=rep(step,N),strategy=Strategy_time$strategy,t=Strategy_time$t)
  
  data<-rbind(data,temp)
}

write.csv(data,"data.csv",row.names = F)

#--------------------------#
#-------visualization------#
#--------------------------#
#策略变化趋势图
df<-rbind(temp0,data) %>% 
  data.table() %>% 
  .[,.N,by=c("step","strategy")]

str(df)

ggplot(df, aes(x=step,y=N,color=strategy)) + 
  labs(x="steps",y="strategies update")+
  geom_point()+
  theme_few()






















