library(tidyverse)
library(data.table)
library(EvolutionaryGames)
library(ggthemes)
library(latex2exp)
library(ggtext)
library(BB)#无梯度优化算法DFO
library(GenSA)#模拟退火
library(reticulate)
so<-import("scipy.optimize")


setwd("/home/zuo_r/involution")

#--------------------------------------------------------------#
#---------------------------三策略求解-------------------------#
#--------------------------------------------------------------#
#方法一 双重退火
# #多元方程格式测试
# udf<-function(x){
#   min((x[1]-2)^2+(x[2]-3)^2+2)
# }
# 
# bounds = list(c(-10,10),c(-10,10))
# 
# a<-so$dual_annealing(udf,bounds)
# a$x

source("game_killer-R3.R")
M<-100
bouds = list(c(0.01,0.99),c(0.01,0.99))
k<-as.integer(2000)

result<-so$dual_annealing(helper,bounds,maxiter = k)
result$x

result1<-so$dual_annealing(helper,bounds,maxiter = k)
result1$x

result2<-so$dual_annealing(helper,bounds,maxiter = k)
result2$x

result3<-so$dual_annealing(helper,bounds,maxiter = k)
result3$x


# #方法二 无梯度优化算法DFO
# out = dfsane(c(0,0,0),helper)$par %>% 
#   round(digits=4)
# out
# 
# `%ni%` <- Negate(`%in%`)
# 
# out[out %ni% c(0,1)] %>% 
#   mean() %>% 
#   round(digits=4)
# 
# #方法三 模拟退火
# set.seed(1234)
# dimension <- 30
# global.min <- 0
# tol <- 0.01
# lower <- rep(0.05, dimension)
# upper <- rep(0.95, dimension)
# out <- GenSA(lower = lower, upper = upper, fn = helper,
#              control=list(threshold.stop=global.min+tol,verbose=TRUE))
# out[c("value","par","counts")]




















































