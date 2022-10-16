library(tidyverse)
library(data.table)
library(EvolutionaryGames)
library(ggthemes)
library(latex2exp)
library(ggtext)
library(BB)

setwd("/home/zuo_r/involution")


duplicate_dynamic<-function(y,M,beta,d,N,c){
  
  #-----参数解释说明------#
  # #N个个体
  # N<-4
  # #M资源 c(5,15,25)
  # M<-5
  # #less effort的成本
  # c<-1
  # #投入效用
  # beta<-1
  # #more effort的成本
  # d<-4
  #-----------------------#
  
  #这里决定用N-1
  #需要取整
  #Nd<-floor(y*(N-1))  #我重新尝试了取整，没有报错
  Nd<-floor(y*(N-1))
  Nc<-N-1-Nd
  
  
  #群体体选择策略c或d的收益  = 概率*pai
  #等价Pc<(choose(N-1, Nc)*((1-y)^Nc)*(y^Nd))*pai_c
  # Pc<-(choose(N-1, Nd)*(y^Nd)*((1-y)^Nc))*pai_c
  # Pd<-(choose(N-1, Nd)*(y^Nd)*((1-y)^Nc))*pai_d #choose计算组合数
  
  #n即Nd,累加收集概率  
  Pc<-0
  Pd<-0
  for(n in 0:(N-1)){  
    Nc<-n
    Nd<-N-1-n
    #策略c（cooperate, less effort）和策略d(defect，more effort)的收益
    #（1）个体选择策略c的期望收益
    pai_c<-(c*M)/((Nc+1)*c+Nd*beta*d)-c
    #（2）个体选择策略d的期望收益
    pai_d<-(beta*d*M)/(Nc*c+(Nd+1)*beta*d)-d
    
    
    Pc<-Pc+(choose(N-1, n)*(y^n)*((1-y)^(N-1-n)))*pai_c
    Pd<-Pd+(choose(N-1, n)*(y^n)*((1-y)^(N-1-n)))*pai_d
  }
  
  #（3）群体策略的期望收益
  R_<-y*Pd+(1-y)*Pc
  
  #(4)复制动态方程
  #y.<-y*(Pd-R_) = y(Pd-y*Pd-(1-y)*Pc) = y(1-y)(Pd-Pc)
  y.<-ifelse(is.na(y*(1-y)*(Pd-Pc)),0,y*(1-y)*(Pd-Pc)) 
}

#------------------------------------------------------------------------#
#------------------------------原图1-------------------------------------#
#------------------------------------------------------------------------#
plot_y._y<-function(M){
  #M<-200
  y.<-c()
  
  for(y in seq(0, 1, 0.01)){
    temp<-duplicate_dynamic(y,M,beta=1,d=4,N=5,c=1) #调整N=5，即可用M=5,15,25复现
    
    # N: 个体数
    # M: 资源 c(5,15,25)
    # c: less effort的成本
    # beta: 投入效用
    # d: more effort的成本
    
    y.<-c(y.,temp)
  }
  
  df <- data.frame(x = seq(0, 1, 0.01), y = y.) %>% 
    mutate(y1=c(y.[-1],1)) %>% 
    mutate(y0=y*y1)#通过前一项与后一项相乘是否小于0，来获取临界点ystar
  
  #ystar<-df$x[df$y0<0]+0.005 #即横坐标，因为是以0.01为步长移动，取0.005为均值加上
  
  ystar<-ifelse(sum(df$x[df$y0<0])==0,0,df$x[df$y0<0]+0.005)
  
  ggplot(df,aes(x=x,y=y))+
    geom_line(color="black",size=0.3)+
    theme_few() +
    geom_point(aes(x=ystar,y=0))+
    geom_text(aes(x=ystar,y=0),label=paste("y*:",ystar),size=4,nudge_y = 0.001,nudge_x = 0.03,color="red")+
    geom_hline(aes(yintercept = 0),size=0.3)+
    labs(x = "y", y = "y.")
}


#---1. fig1 (a1)---#
plot_y._y(M=17)

#---2. fig1 (b1)---#
plot_y._y(M=200)

plot_y._y(M=250)

#---3. fig1 (c1)---#
plot_y._y(M=398)#右临界资源值

#调整N=5，即可用M=5,15,25复现
plot_y._y(M=25)



#------------------------------------------------------------------------#
#------------------------------新图1-------------------------------------#
#------------------------------------------------------------------------#
#用dfsane方式复现
helper = function(yx){
  M<-M
  
  N=100
  
  beta=1
  d=4
  c=1
  
  #群体初始比例
  xD = yx[1] #原来的y
  xC <-yx[2] #原来的x
  
  #群体数量
  Nd<-floor(xD*(N-1))
  Nc<-N-1-Nd
  
  #个体期望收益
  pai_d<-(beta*d*M)/(Nc*c+(Nd+1)*beta*d)-d
  pai_c<-(c*M)/((Nc+1)*c+Nd*beta*d)-c
  
  Pd<-0
  Pc<-0
  
  for(n in 0:(N-1)){  
    Pd<-Pd+(choose(N-1, n)*(xD^n)*((1-xD)^(N-1-n)))*pai_d
    Pc<-Pc+(choose(N-1, n)*(xD^n)*((1-xD)^(N-1-n)))*pai_c
  }
  
  #设置浮点数k
  k<-10
  
  #群体策略的期望收益
  R_<-xD*Pd+round(1-xD,k)*Pc
  
  #复制动态方程
  xD.<-xD*round(Pd-R_,k)
  xC.<-xC*round(Pd-R_,k)
  
  return (c(xD.,xC.))
}

M<-0
get_star_by_M = function(M){
  
  out = dfsane(c(0.5,0.5),helper)$par %>% 
    round(digits=10)
  
  return(data.frame(ystar=out[1],xstar=out[2],M=M))
}

get_star_by_M(M=100)
get_star_by_M(200)
get_star_by_M(398)


