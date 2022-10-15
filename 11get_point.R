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
source("game_killer-R1.R")

#a1.参数构建
parameters<-data.frame()
for (y in seq(0,1,by=0.05)) {
  x<-seq(0,round(1-y,2),by=0.05)
  temp<-data.frame(y=rep(y,length(x)),s=round(1-x-y,2),x=x)
  parameters<-rbind(parameters,temp)
}

# #a2获取一个M的多个解
# Temp<-data.frame()
# M=50
# for (i in 1:231) {
#   print(paste("i=",i))
#   out = dfsane(c(parameters$y[i],parameters$s[i],parameters$x[i]),helper)$par %>% 
#     round(digits=3)
#   temp<-(data.frame(ystar=out[1],sstar=out[2],xstar=out[3],M=M,
#                     y=parameters$y[i],s=parameters$s[i],x=parameters$x[i]))
#   Temp<-rbind(Temp,temp)
# }
# return(Temp)
# 
# #取都大于0的解，求均值
# data<-Temp %>% 
#   subset(ystar>0 & sstar>0 & xstar>0)
# 
# result_M_mean<-apply(data,2,mean)
# result_M_mean
# 
# #查看三个策略的比例和
# result_M_mean[1]+result_M_mean[2]+result_M_mean[3]



#a3 批量获取均值
Temp1<-data.frame()
get_star_by_M1 = function(M){
  Temp<-data.frame()
  M<-50
  for (i in 1:231) {
    print(paste("M=",M,";  ","i=",i))
    out = dfsane(c(parameters$y[i],parameters$s[i],parameters$x[i]),helper1)$par %>% 
      round(digits=3)
    temp<-(data.frame(ystar=out[1],sstar=out[2],xstar=out[3],M=M,
                      y=parameters$y[i],s=parameters$s[i],x=parameters$x[i]))
    Temp<-rbind(Temp,temp)
  }
  
  Temp1<-rbind(Temp1,Temp)
  
  return(Temp1)
}

get_star_by_M1(50)
#a<-Temp
b<-Temp

#a4-1.遍历M，收集每个初始值对应的star值
pb <- progress_bar$new(format = "  完成百分比 [:bar] :percent 执行时间 :elapsed:elapsedfull",total = 251, clear = FALSE, width= 60)

data_raw = data.frame()

a<-Sys.time()
for (M in 50:300){
  pb$tick()
  data_raw=rbind(data_raw,get_star_by_M1(M))
}
b<-Sys.time()
b-a

write.csv(data_raw,"data_raw.csv",row.names = F)



#a4-2.遍历M，收集每个初始值对应的star值【并行运算版】




#a5, 对初始值进行清洗
data_positive<-data_raw %>% 
  subset(ystar>=0 & sstar>=0 & xstar>=0 & ystar<=1 & sstar<=1 & xstar<=1) 
 
#思路一：抽取一个初始值有最多可行解的解
#排序查看每组初始值对应的解的数量
data_max_value<-data_positive %>% 
  data.table() %>% 
  .[,.N,by=c("y","s","x")] %>% 
  arrange(N)
data_max_value

#查看对应的star值
data_max_value1<-data_positive %>% 
  data.table() %>% 
  .[,N:=.N,by=c("y","s","x")] %>% 
  arrange(desc(N)) 

sample<-data_raw %>% 
  subset(y=0.10 & s=0.05 & x=0.85)

#思路二：调整其他参数，再重复思路一

















