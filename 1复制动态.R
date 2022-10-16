library(tidyverse)
library(data.table)
library(EvolutionaryGames)


setwd("/home/zuo_r/involution")


#---------------------------------------------------------------------#
#网页demo求解https://max.book118.com/html/2018/0411/161004783.shtm
#1.矩阵
#dynamic <- Replicator
v<-2
c<-12
A<-matrix(c((v-c)/2,v,0,v/2),2,byrow = T)
strategies <- c("Hawk", "Dove")
ESS(A)
ESS(A, strategies)
ESS(A, strategies,floats = F)

phaseDiagram2S(A, Replicator, strategies = strategies)
#---------------------------------------------------------------------#


#----------------------------#
#---phaseDiagram2S数据收集---#
#----------------------------#
dynamic <- function(time, state, parameters) {
  a <- parameters
  states <- sqrt(length(a))
  A <- matrix(a, states, byrow = TRUE)
  A <- t(A)
  
  dX <- c()
  
  #（1）单策略适应度
  for(i in 1:states) {
    dX[i] <- sum(state * A[i, ])
  }
  
  #（2）平均适应度
  avgFitness <- sum(dX * state)
  
  #（3）复制动态方程
  for(i in 1:states) {
    dX[i] <- state[i] * (dX[i] - avgFitness)
  }
  
  return(list(dX))
}

#1.数据y收集
times <- seq(0, 1, 0.01)
y <- c()
param = c((v-c)/2,0,v,v/2)  #---->横向顺序

for (i in times) {
  y <- c(y, dynamic(state = c(i, 1 - i), parameters = param)[[1]][1])
}

dynData <- data.frame(x = times, y = y)

#2.在曲线上做箭头标记
num <- 10
dist <- 1/(num + 1)
step <- seq(dist, 1 - dist, dist)

x <- xend <- step
for (i in 1:num) {
  s <- step[i]
  val <- dynamic(state = c(s, 1 - s), parameters = param)[[1]][1]
  fac <- 0.001
  if (val > 0) {
    xend[i] <- xend[i] + fac
  }
  else if (val < 0) {
    x[i] <- x[i] + fac
  }
}

arrData <- data.frame(x = x, xend = xend)  #arrow的缩写

#3.数据可视化
strategies <- c("Hawk", "Dove")
lineData <- data.frame(x = c(0, 1), y = c(0, 0))
xAxis <- paste("population share of strategy", strategies[1])
p <- ggplot2::ggplot() + 
  ggplot2::geom_path(data = dynData,ggplot2::aes(x = times, y = y), size = 0.3, color = "black") + 
  ggplot2::scale_x_continuous(expand = c(0, 0)) + 
  ggplot2::theme_classic() + 
  ggplot2::labs(x = xAxis, y = "dx/dt") + 
  ggplot2::theme(plot.margin = ggplot2::unit(c(1,1, 1, 1), "cm"))

vField <- list(ggplot2::geom_segment(data = arrData, size = 0.3,
                                     ggplot2::aes(x = x, xend = xend, y = 0, yend = 0), 
                                     arrow = ggplot2::arrow(length = ggplot2::unit(0.2,"cm"))), 
               ggplot2::geom_line(data = lineData, ggplot2::aes(x = x,y = y), size = 0.3))

print(p)
print(p + vField)





