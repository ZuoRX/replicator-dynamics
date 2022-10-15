library(tidyverse)
library(data.table)
library(EvolutionaryGames)
library(ggthemes)
library(latex2exp)
library(ggtext)
library(progress)
library(plotly)

setwd("/home/zuo_r/involution")

duplicate_dynamic_xy<-function(y,x,M,beta1,beta2,d,N,c,l){
  
  #-----参数解释说明------#
  # #N个个体
  # N<-4
  # #M资源 c(5,15,25)
  # M<-5
  
  # y 整体内卷比例
  # x 整体躺平比例
  
  # #内卷相对躺平的效用
  # beta1<-4
  # #合作相对躺平的效用
  # beta2<-2
  
  # #more effort的成本
  # d<-4
  # #less effort的成本
  # c<-1
  # #躺平的成本
  # l<-0.5
  #-----------------------#
  
  Pc<-0
  Pd<-0
  Pl<-0 
  for (Nd in 0:(N-1)){ 
    for (Nl in 0:(N-1-Nd)){
      Nc <- N-1-Nd-Nl
      pai_d <- beta1*d*M/(beta1*(Nd+1)*d +beta2*Nc*c     +Nl*l)-d 
      pai_c <- beta2*c*M/(beta1*Nd*d     +beta2*(Nc+1)*c +Nl*l)-c          
      pai_l <-       l*M/(beta1*Nd*d     +beta2*Nc*c     +(Nl+1)*l)-l  
      Pd <- Pd + choose(N-1,Nd)*choose(N-1-Nd,Nl)*y^Nd*(1-x-y)^Nc*x^Nl*pai_d
      Pc <- Pc + choose(N-1,Nd)*choose(N-1-Nd,Nl)*y^Nd*(1-x-y)^Nc*x^Nl*pai_c
      Pl <- Pl + choose(N-1,Nd)*choose(N-1-Nd,Nl)*y^Nd*(1-x-y)^Nc*x^Nl*pai_l
    }
  }
  
  R_ <- y*Pd+x*Pl+(1-x-y)*Pc  ##均值
  y. <- y*(Pd-R_)
  x. <- x*(Pl-R_)
  
  result<-data.frame(y=y,x=x,M=M,beta1=beta1,beta2=beta2,d=d,N=N,c=c,l=l,y.=y.,x.=x.)
  return(result)
}


#原文三点结论：一一验证
#1.更充足的资源会促进内卷
#2.增加更努力的相对效用会放大内卷
#3.增加内卷的成本会在一定参数范围内放大内卷，但最终会抑制内卷

#能否找到“卷不动，躺不平”的“点”？
#如何在“卷”和“躺”中找平衡？
#“合作”实质是一中不内卷、也不躺平的一种状态，用什么词描述更准确？45度？斜垮？漂？划水？摸鱼？
#思路：合作的成本c远离d是一种“卷不动”的现象；合作的成本c趋近l但又不等于l来表示“躺不平”

#----------------------------------------------#
#-----1.1当躺平人数一定时，内卷人数的稳定点-----#
#----------------------------------------------#

#x+y<=1
data<-data.frame()
pb <- progress_bar$new(format = "  完成百分比 [:bar] :percent 执行时间 :elapsed:elapsedfull",total = 101, clear = FALSE, width= 60)
for(y in seq(0,1,0.01)){
  pb$tick()
  for(x in seq(0,1-y,0.01)){
    temp<-duplicate_dynamic_xy(y,x,M=100,beta1=4,beta2=2,d=4,N=50,c=1,l=0.5)
    data<-rbind(data,temp)
  }
}


data1<-data %>% 
  mutate(y=round(y,digits=2)) %>% #排序有问题，debug是浮点数的问题#第495行
  mutate(x=round(x,digits=2)) %>%
  arrange(x,y) %>% 
  data.table() %>% 
  .[,y1:=c(y.[-1],0),by=x] %>% 
  mutate(y0=y.*y1) %>% 
  data.table() %>% 
  .[,ystar:=y[y0<0]-0.005,by=x] %>% 
  mutate(ystar=ifelse(is.na(ystar),0,ystar)) %>% #好多NA的问题，是无0,要设置为o
  subset(!is.na(ystar))

#ystar随x是如何变化的？
ggplot(data1,aes(x=x,y=ystar))+
  geom_point()

#如何解释？

#当x=0.1时
df<-data %>% 
  subset(x==0.1)%>% 
  mutate(y1=c(y.[-1],1)) %>% 
  mutate(y0=y*y1)#通过前一项与后一项相乘是否小于0，来获取临界点ystar

ystar<-ifelse(sum(df$y[df$y0<0])==0,0,df$y[df$y0<0]-0.005)

ggplot(df,aes(x=y,y=y.))+
  geom_line(color="black",size=0.3)+
  theme_few() +
  geom_point(aes(x=ystar,y=0))+
  geom_text(aes(x=ystar,y=0),label=paste("y*:",ystar),size=4,nudge_y = 0.001,nudge_x = 0.03,color="red")+
  geom_hline(aes(yintercept = 0),size=0.3)+
  labs(x = "y", y = "y.")


#当x=0.2时
df<-data %>% 
  subset(x==0.2)%>% 
  mutate(y1=c(y.[-1],1)) %>% 
  mutate(y0=y*y1)#通过前一项与后一项相乘是否小于0，来获取临界点ystar

ystar<-ifelse(sum(df$y[df$y0<0])==0,0,df$y[df$y0<0]+0.005)

ggplot(df,aes(x=y,y=y.))+
  geom_line(color="black",size=0.3)+
  theme_few() +
  geom_point(aes(x=ystar,y=0))+
  geom_text(aes(x=ystar,y=0),label=paste("y*:",ystar),size=4,nudge_y = 0.001,nudge_x = 0.03,color="red")+
  geom_hline(aes(yintercept = 0),size=0.3)+
  labs(x = "y", y = "y.")

#x与y对ystar的影响？
#.x,y, fill=y.  然后把y.=0的点标记红色
data2<-data1 %>% 
  subset(y0<0) %>% 
  dplyr::select(y,x)

data3<-data1 %>% 
  mutate(x2=c(data2$x,rep(0,nrow(data1)-nrow(data2)))) %>% 
  mutate(y2=c(data2$y,rep(0,nrow(data1)-nrow(data2))))

ggplot(data3,aes(x=y,y=x))+
  scale_fill_gradientn(values = c(min(data3$y.),0,max(data3$y.)),
                       colors = c('yellow','red'))+
  geom_raster(aes(fill=y.))+
  geom_point(aes(x=y2,y=x2),color="red")

#左上角有点异常点，什么原因？
#如何配色更适合凸显0的分界线？
#如何解释？

#----------------------------------------------#
#--------1.2内卷人数比例对躺平人数的影响-------#
#----------------------------------------------#

data1<-data %>% 
  mutate(y=round(y,digits=2)) %>% #排序有问题，debug是浮点数的问题#第495行
  mutate(x=round(x,digits=2)) %>%
  arrange(y,x) %>% 
  data.table() %>% 
  .[,x1:=c(x.[-1],0),by=y] %>% 
  mutate(x0=x.*x1) %>% 
  data.table() %>% 
  .[,xstar:=x[x0<0]-0.005,by=y] %>% 
  mutate(xstar=ifelse(is.na(xstar),0,xstar)) %>% #好多NA的问题，是无0,要设置为o
  subset(!is.na(ystar))

#ystar随x是如何变化的？
ggplot(data1,aes(x=y,y=xstar))+
  geom_point()

#如何解释？

#当y=0.1时
df<-data %>% 
  subset(y==0.1)%>% 
  mutate(x1=c(x.[-1],0)) %>%                                                                          
  mutate(x0=x.*x1)#通过前一项与后一项相乘是否小于0，来获取临界点ystar

xstar<-ifelse(sum(df$x[df$x0<0])==0,0,df$x[df$x0<0]+0.005)

ggplot(df,aes(x=x,y=x.))+
  geom_line(color="black",size=0.3)+
  theme_few() +
  geom_point(aes(x=xstar,y=0))+
  geom_text(aes(x=xstar,y=0),label=paste("x*:",xstar),size=4,nudge_y = 0.001,nudge_x = 0.03,color="red")+
  geom_hline(aes(yintercept = 0),size=0.3)+
  labs(x = "x", y = "x.")

#当y=0.7时
df<-data %>% 
  subset(y==0.2)%>% 
  mutate(x1=c(x.[-1],0)) %>% 
  mutate(x0=x.*x1)#通过前一项与后一项相乘是否小于0，来获取临界点ystar

xstar<-ifelse(sum(df$x[df$x0<0])==0,0,df$x[df$x0<0]+0.005)

ggplot(df,aes(x=x,y=x.))+
  geom_line(color="black",size=0.3)+
  theme_few() +
  geom_point(aes(x=xstar,y=0))+
  geom_text(aes(x=xstar,y=0),label=paste("x*:",xstar),size=4,nudge_y = 0.001,nudge_x = 0.03,color="red")+
  geom_hline(aes(yintercept = 0),size=0.3)+
  labs(x = "x", y = "x.")

#x与y对xstar的影响？
#.x,y, fill=y.  然后把y.=0的点标记红色
data2<-data1 %>% 
  subset(x0<0) %>% 
  dplyr::select(y,x)

data3<-data1 %>% 
  mutate(x2=c(data2$x,rep(0,nrow(data1)-nrow(data2)))) %>% 
  mutate(y2=c(data2$y,rep(0,nrow(data1)-nrow(data2))))

ggplot(data3,aes(x=y,y=x))+
  scale_fill_gradientn(values = c(min(data3$x.),0,max(data3$x.)),
                       colors = c('yellow','red'))+
  geom_raster(aes(fill=x.))+
  geom_point(aes(x=y2,y=x2),color="red")

#大片异常点，什么原因？
#如何配色更适合凸显0的分界线？
#如何解释？



#复现文章中原图2的可视化
#------------------------------------#
#--------2.1资源量对内卷的影响-------#
#------------------------------------#

# #x+y<=1
# data<-data.frame()
# pb <- progress_bar$new(format = "  完成百分比 [:bar] :percent 执行时间 :elapsed:elapsedfull",total = 46, clear = FALSE, width= 60)
# 
# for(M in seq(50,500,10)){
#   pb$tick()
#   for(y in seq(0,1,0.01)){
#     for(x in seq(0,1-y,0.01)){
#       temp<-duplicate_dynamic_xy(y,x,M=M,beta1=4,beta2=2,d=4,N=50,c=1,l=0.5)
#       data<-rbind(data,temp)
#     }
#   }
# }
# 
# fwrite(data,"data_beta1=4_beta2=2.csv",row.names = F)


#----------------------------#
#--------3.beta1的影响-------#
#----------------------------#

# data<-data.frame()
# pb <- progress_bar$new(format = "  完成百分比 [:bar] :percent 执行时间 :elapsed:elapsedfull",total = 46, clear = FALSE, width= 60)
# 
# for(M in seq(50,500,10)){
#   pb$tick()
#   for(beta1 in c(6,8,10)){
#     for(y in seq(0,1,0.01)){
#       for(x in seq(0,1-y,0.01)){
#         temp<-duplicate_dynamic_xy(y,x,M=M,beta1,beta2=2,d=4,N=50,c=1,l=0.5)
#         data<-rbind(data,temp)
#       }
#     }
#   }
# }
# 
# fwrite(data,"data_beta1=6-8-10_beta2=2.csv",row.names = F)


#----------------------------#
#--------4.beta2的影响-------#
#----------------------------#
# #用多线程试试
# data<-data.frame()
# pb <- progress_bar$new(format = "  完成百分比 [:bar] :percent 执行时间 :elapsed:elapsedfull",total = 46, clear = FALSE, width= 60)
# 
# for(M in seq(50,500,10)){
#   pb$tick()
#   for(beta2 in 3:7){
#     for(y in seq(0,1,0.01)){
#       for(x in seq(0,1-y,0.01)){
#         temp<-duplicate_dynamic_xy(y,x,M=M,beta1=8,beta2=beta2,d=4,N=50,c=1,l=0.5)
#         data<-rbind(data,temp)
#       }
#     }
#   }
# }
# 
# fwrite(data,"data_beta1=8_beta2=3-4-5-6-7.csv",row.names = F)







