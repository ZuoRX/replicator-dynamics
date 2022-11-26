#方法一
library(cmna)
f <- function(x) { x^6 - 4 * x^5 - 7 * x^4 + 22 * x^3 + 24 * x^2 + 2}
sa(f, 0)
#取绝对值

#方法二
library(GenSA)
set.seed(1234) # The user can use any seed.
Rastrigin <- function(x) {
  sum(x^2 - 10 * cos(2 * pi  * x)) + 10 * length(x)
}

dimension <- 30
global.min <- 0
tol <- 1e-13
lower <- rep(-5.12, dimension)
upper <- rep(5.12, dimension)
out <- GenSA(lower = lower, upper = upper, fn = Rastrigin,
             control=list(threshold.stop=global.min+tol,verbose=TRUE))
out[c("value","par","counts")]

# #方法三  特征降维
# library(subselect)
# data(swiss)
# anneal(cor(swiss),2,3,nsol=4,niter=10,criterion="RM")
# 
# anneal(cor(swiss),2,3,nsol=4,niter=10,criterion="RM",exclude=c(6))
# # https://www.rdocumentation.org/packages/subselect/versions/0.15.2/topics/anneal


#------------------------------------------------------------------------#
#------------------------------复现求解-------------------------------------#
#------------------------------------------------------------------------#
library(tidyverse)
library(data.table)
library(EvolutionaryGames)
library(ggthemes)
library(latex2exp)
library(ggtext)
library(BB)

setwd("/home/zuo_r/involution")

duplicate_dynamic<-function(y){
  
  y<-y
  M<-200
  beta=1
  d=4
  N=100
  c=1
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
  
  #策略c（cooperate, less effort）和策略d(defect，more effort)的收益
  #（1）个体选择策略c的期望收益
  pai_c<-(c*M)/((Nc+1)*c+Nd*beta*d)-c
  #（2）个体选择策略d的期望收益
  pai_d<-(beta*d*M)/(Nc*c+(Nd+1)*beta*d)-d
  
  #群体体选择策略c或d的收益  = 概率*pai
  #等价Pc<(choose(N-1, Nc)*((1-y)^Nc)*(y^Nd))*pai_c
  # Pc<-(choose(N-1, Nd)*(y^Nd)*((1-y)^Nc))*pai_c
  # Pd<-(choose(N-1, Nd)*(y^Nd)*((1-y)^Nc))*pai_d #choose计算组合数
  
  #n即Nd,累加收集概率  
  Pc<-0
  Pd<-0
  for(n in 0:(N-1)){  
    Pc<-Pc+(choose(N-1, n)*(y^n)*((1-y)^(N-1-n)))*pai_c
    Pd<-Pd+(choose(N-1, n)*(y^n)*((1-y)^(N-1-n)))*pai_d
  }
  
  #（3）群体策略的期望收益
  R_<-y*Pd+(1-y)*Pc
  
  #(4)复制动态方程
  y.<-y*(Pd-R_) #= y(Pd-y*Pd-(1-y)*Pc) = y(1-y)(Pd-Pc)
  #y.<-ifelse(is.na(y*(1-y)*(Pd-Pc)),0,y*(1-y)*(Pd-Pc)) 
  
  result<-ifelse(y.==0,1,abs(y.))
}


#方法一
a<-Sys.time()
sa(duplicate_dynamic(),0)
b<-Sys.time()
b-a


#方法二
set.seed(1234) # The user can use any seed.
Rastrigin <- function(y){
  
  y<-y
  M<-200
  beta=1
  d=4
  N=100
  c=1
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
  
  #策略c（cooperate, less effort）和策略d(defect，more effort)的收益
  #（1）个体选择策略c的期望收益
  pai_c<-(c*M)/((Nc+1)*c+Nd*beta*d)-c
  #（2）个体选择策略d的期望收益
  pai_d<-(beta*d*M)/(Nc*c+(Nd+1)*beta*d)-d
  
  #群体体选择策略c或d的收益  = 概率*pai
  #等价Pc<(choose(N-1, Nc)*((1-y)^Nc)*(y^Nd))*pai_c
  # Pc<-(choose(N-1, Nd)*(y^Nd)*((1-y)^Nc))*pai_c
  # Pd<-(choose(N-1, Nd)*(y^Nd)*((1-y)^Nc))*pai_d #choose计算组合数
  
  #n即Nd,累加收集概率  
  Pc<-0
  Pd<-0
  for(n in 0:(N-1)){  
    Pc<-Pc+(choose(N-1, n)*(y^n)*((1-y)^(N-1-n)))*pai_c
    Pd<-Pd+(choose(N-1, n)*(y^n)*((1-y)^(N-1-n)))*pai_d
  }
  
  #（3）群体策略的期望收益
  R_<-y*Pd+(1-y)*Pc
  
  #(4)复制动态方程
  y.<-y*(Pd-R_) #= y(Pd-y*Pd-(1-y)*Pc) = y(1-y)(Pd-Pc)
  #y.<-ifelse(is.na(y*(1-y)*(Pd-Pc)),0,y*(1-y)*(Pd-Pc)) 
  
  return(abs(y.))
}

dimension <- 30
global.min <- 0
tol <- 0.01
lower <- rep(0.05, dimension)
upper <- rep(0.95, dimension)
out <- GenSA(lower = lower, upper = upper, fn = Rastrigin,
             control=list(threshold.stop=global.min+tol,verbose=TRUE))
out[c("value","par","counts")]












#方法三
#Derivative-Free Spectral Approach
#无梯度优化算法DFO
out = dfsane(seq(0,1,0.001),duplicate_dynamic)$par %>% 
  round(digits=4)
out

`%ni%` <- Negate(`%in%`)

out[out %ni% c(0,1)] %>% 
  mean() %>% 
  round(digits=4)


library(reticulate)
#py_install("scipy")
so<-import("scipy.optimize")

udf<-function(x){
  min((x-2)^2+2)
}

bounds = list(c(-10,10))

a<-so$dual_annealing(udf,bounds)
a$x

bounds = list(c(0.01,0.99))

a<-so$dual_annealing(Rastrigin,bounds)
a$x

b<-so$dual_annealing(Rastrigin,bounds)
b$x

c<-so$dual_annealing(Rastrigin,bounds)
c$x

d<-so$dual_annealing(Rastrigin,bounds)
d$x

library(GenSA)
# Try Rastrgin function (The objective function value for global minimum
# is 0 with all components of par are 0.)
Rastrigin <- function(y){
  
  y<-y
  M<-200
  beta=1
  d=4
  N=100
  c=1
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
  
  #策略c（cooperate, less effort）和策略d(defect，more effort)的收益
  #（1）个体选择策略c的期望收益
  pai_c<-(c*M)/((Nc+1)*c+Nd*beta*d)-c
  #（2）个体选择策略d的期望收益
  pai_d<-(beta*d*M)/(Nc*c+(Nd+1)*beta*d)-d
  
  #群体体选择策略c或d的收益  = 概率*pai
  #等价Pc<(choose(N-1, Nc)*((1-y)^Nc)*(y^Nd))*pai_c
  # Pc<-(choose(N-1, Nd)*(y^Nd)*((1-y)^Nc))*pai_c
  # Pd<-(choose(N-1, Nd)*(y^Nd)*((1-y)^Nc))*pai_d #choose计算组合数
  
  #n即Nd,累加收集概率  
  Pc<-0
  Pd<-0
  for(n in 0:(N-1)){  
    Pc<-Pc+(choose(N-1, n)*(y^n)*((1-y)^(N-1-n)))*pai_c
    Pd<-Pd+(choose(N-1, n)*(y^n)*((1-y)^(N-1-n)))*pai_d
  }
  
  #（3）群体策略的期望收益
  R_<-y*Pd+(1-y)*Pc
  
  #(4)复制动态方程
  y.<-y*(Pd-R_) #= y(Pd-y*Pd-(1-y)*Pc) = y(1-y)(Pd-Pc)
  #y.<-ifelse(is.na(y*(1-y)*(Pd-Pc)),0,y*(1-y)*(Pd-Pc)) 
  
  result<-ifelse(y.==0,1,abs(y.))
  min(result)
}
# Perform the search on a 30 dimensions rastrigin function. Rastrigin
# function with dimension 30 is known as the most
# difficult optimization problem according to "Yao X, Liu Y, Lin G (1999).
# \Evolutionary Programming Made Faster."
# IEEE Transactions on Evolutionary Computation, 3(2), 82-102.

# GenSA will stop after finding the targeted function value 0 with
# absolute tolerance 1e-13
set.seed(134) # The user can use any seed.
dimension <- 30
global.min <- 0
tol <- 0.001
lower <- rep(0, dimension)
upper <- rep(1, dimension)
out <- GenSA(lower = lower, upper = upper, fn = Rastrigin,
             control=list(threshold.stop=global.min+tol,verbose=TRUE))
out[c("value","par","counts")]

# GenSA will stop after running for about 2 seconds
# Note: The time for solving this problem by GenSA may vary
# depending on the computer used. 
set.seed(1234) # The user can use any seed.
dimension <- 30
global.min <- 0
tol <- 1e-13
lower <- rep(-5.12, dimension)
upper <- rep(5.12, dimension)
out <- GenSA(lower = lower, upper = upper, fn = Rastrigin,
             control=list(max.time=2))
out[c("value","par","counts")]

