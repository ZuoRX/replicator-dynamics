library(tidyverse)
library(data.table)
library(EvolutionaryGames)
library(ggthemes)
library(latex2exp)
library(ggtext)
library(BB)#无梯度优化算法DFO
library(GenSA)#模拟退火
library(reticulate)
library(plotly)

setwd("/home/zuo_r/involution")

source("game_killer-R3.R")

#-----------------------------------------------------------------------------------#
#-----------------------------------一、M=200---------------------------------------#
#-----------------------------------------------------------------------------------#
#1.构建参数集
parameters<-data.frame(xD=rep(seq(0,1,0.01),each=101),xC=rep(seq(0,1,0.01),101)) %>%
  mutate(Add=xD+xC) %>% 
  mutate(xD=case_when(Add>1~0.3,T~xD)) %>% 
  mutate(xC=case_when(Add>1~0.3,T~xC))

#2.求解收集
Temp<-data.frame(xD=0,xC=0,xL=0,N=0,M=0,betaD=0,betaC=0,d=0,c=0,l=0,xD.=0,xC.=0,xL.=0)
for(i in 1:nrow(parameters)){
  temp<-duplicate_dynamics3(xD=parameters$xD[i],xC=parameters$xC[i],
                            N=50,M=200,betaD=1.5,betaC=1.1,d=4,c=1,l=0.5) 
  
  Temp<-rbind(Temp,temp)
}

data<-Temp[-1,]

#3.数据格式处理与可视化
#(1)xD.
matrix<-matrix(data$xD.,nrow = 101, byrow = FALSE)#上面是xD,即x轴为xD

p1<-plot_ly(x=seq(0,1,0.01),y=seq(0,1,0.01),z = ~matrix)%>%
  add_surface() %>% 
  layout(scene = list(xaxis=list(title="involution"),
                      yaxis=list(title="sit up"),
                      zaxis=list(title="value")
                      ))
p1
htmlwidgets::saveWidget(as_widget(p1), "Exp1/M200-p1.html")
        
#(2)xC.
matrix<-matrix(data$xC.,nrow = 101, byrow = FALSE)#上面是xD

p2<-plot_ly(x=seq(0,1,0.01),y=seq(0,1,0.01),z = ~matrix)%>%
  add_surface() %>% 
  layout(scene = list(xaxis=list(title="involution"),
                      yaxis=list(title="sit up"),
                      zaxis=list(title="value")
  ))
p2
htmlwidgets::saveWidget(as_widget(p2), "Exp1/M200-p2.html")

#(3)xL.
matrix<-matrix(data$xL.,nrow = 101, byrow = FALSE)#上面是xD

p3<-plot_ly(x=seq(0,1,0.01),y=seq(0,1,0.01),z = ~matrix)%>%
  add_surface() %>% 
  layout(scene = list(xaxis=list(title="involution"),
                      yaxis=list(title="sit up"),
                      zaxis=list(title="value")
  )) 
p3
htmlwidgets::saveWidget(as_widget(p3), "Exp1/M200-p3.html")


#4.交点可视化---三种策略的稳定解
data1<-data %>% 
  select(xD,xC,xD.,xC.,xL.) %>% 
  subset(xD.==0 | xC.==0 |xL.==0) %>% 
  gather(key = "strategies","value",-c(1,2))

data2<-data %>% 
  select(xD,xC,xD.,xC.,xL.) %>% 
  subset(xD.==0 & xC.==0 &xL.==0) 

p4<-plot_ly(data1,x=~xD,y=~xC,z=~value,color=~strategies) %>% 
  add_markers(size=1) %>% 
  layout(scene = list(xaxis=list(title="involution"),
                     yaxis=list(title="sit up"),
                     zaxis=list(title="value")
  )) %>% 
  add_markers(data2,x=~data2$xD,y=~data2$xC,z=0,color=I("red"))
p4

htmlwidgets::saveWidget(as_widget(p4), "Exp1/M200-p4.html")



#-----------------------------------------------------------------------------------#
#-----------------------------------二、M=120---------------------------------------#
#-----------------------------------------------------------------------------------#
#1.构建参数集
parameters<-data.frame(xD=rep(seq(0,1,0.01),each=101),xC=rep(seq(0,1,0.01),101)) %>%
  mutate(Add=xD+xC) %>% 
  mutate(xD=case_when(Add>1~0.3,T~xD)) %>% 
  mutate(xC=case_when(Add>1~0.3,T~xC))

#2.求解收集
Temp<-data.frame(xD=0,xC=0,xL=0,N=0,M=0,betaD=0,betaC=0,d=0,c=0,l=0,xD.=0,xC.=0,xL.=0)
for(i in 1:nrow(parameters)){
  temp<-duplicate_dynamics3(xD=parameters$xD[i],xC=parameters$xC[i],
                            N=50,M=120,betaD=1.5,betaC=1.1,d=4,c=1,l=0.5) 
  
  Temp<-rbind(Temp,temp)
}

data<-Temp[-1,]

#3.数据格式处理与可视化
#(1)xD.
matrix<-matrix(data$xD.,nrow = 101, byrow = FALSE)#上面是xD,即x轴为xD

p1<-plot_ly(x=seq(0,1,0.01),y=seq(0,1,0.01),z = ~matrix)%>%
  add_surface() %>% 
  layout(scene = list(xaxis=list(title="involution"),
                      yaxis=list(title="sit up"),
                      zaxis=list(title="value")
  ))
p1
htmlwidgets::saveWidget(as_widget(p1), "Exp1/M150-p1.html")

#(2)xC.
matrix<-matrix(data$xC.,nrow = 101, byrow = FALSE)#上面是xD

p2<-plot_ly(x=seq(0,1,0.01),y=seq(0,1,0.01),z = ~matrix)%>%
  add_surface() %>% 
  layout(scene = list(xaxis=list(title="involution"),
                      yaxis=list(title="sit up"),
                      zaxis=list(title="value")
  ))
p2
htmlwidgets::saveWidget(as_widget(p2), "Exp1/M150-p2.html")

#(3)xL.
matrix<-matrix(data$xL.,nrow = 101, byrow = FALSE)#上面是xD

p3<-plot_ly(x=seq(0,1,0.01),y=seq(0,1,0.01),z = ~matrix)%>%
  add_surface() %>% 
  layout(scene = list(xaxis=list(title="involution"),
                      yaxis=list(title="sit up"),
                      zaxis=list(title="value")
  )) 
p3
htmlwidgets::saveWidget(as_widget(p3), "Exp1/M150-p3.html")


#4.交点可视化---三种策略的稳定解
data1<-data %>% 
  select(xD,xC,xD.,xC.,xL.) %>% 
  subset(abs(xD.)<0.001 | abs(xC.)<0.001 |abs(xL.)<0.001) %>% 
  gather(key = "strategies","value",-c(1,2))

data2<-data %>% 
  select(xD,xC,xD.,xC.,xL.) %>% 
  subset(abs(xD.)<0.001 & abs(xC.)<0.001 &abs(xL.)<0.001) 

p4<-plot_ly(data1,x=~xD,y=~xC,z=~value,color=~strategies) %>% 
  add_markers(size=1) %>% 
  layout(scene = list(xaxis=list(title="involution"),
                      yaxis=list(title="sit up"),
                      zaxis=list(title="value")
  )) %>% 
  add_markers(data2,x=~data2$xD,y=~data2$xC,z=0,color=I("red"))
p4

htmlwidgets::saveWidget(as_widget(p4), "Exp1/M150-p4.html")



#-----------------------------------------------------------------------------------#
#-----------------------------------三、M=100---------------------------------------#
#-----------------------------------------------------------------------------------#
#1.构建参数集
parameters<-data.frame(xD=rep(seq(0,1,0.01),each=101),xC=rep(seq(0,1,0.01),101)) %>%
  mutate(Add=xD+xC) %>% 
  mutate(xD=case_when(Add>1~0.3,T~xD)) %>% 
  mutate(xC=case_when(Add>1~0.3,T~xC))

#2.求解收集
Temp<-data.frame(xD=0,xC=0,xL=0,N=0,M=0,betaD=0,betaC=0,d=0,c=0,l=0,xD.=0,xC.=0,xL.=0)
for(i in 1:nrow(parameters)){
  temp<-duplicate_dynamics3(xD=parameters$xD[i],xC=parameters$xC[i],
                            N=50,M=100,betaD=1.5,betaC=1.1,d=4,c=1,l=0.5) 
  
  Temp<-rbind(Temp,temp)
}

data<-Temp[-1,]

#3.数据格式处理与可视化
#(1)xD.
matrix<-matrix(data$xD.,nrow = 101, byrow = FALSE)#上面是xD,即x轴为xD

p1<-plot_ly(x=seq(0,1,0.01),y=seq(0,1,0.01),z = ~matrix)%>%
  add_surface() %>% 
  layout(scene = list(xaxis=list(title="involution"),
                      yaxis=list(title="sit up"),
                      zaxis=list(title="value")
  ))
p1
htmlwidgets::saveWidget(as_widget(p1), "Exp1/M100-p1.html")

#(2)xC.
matrix<-matrix(data$xC.,nrow = 101, byrow = FALSE)#上面是xD

p2<-plot_ly(x=seq(0,1,0.01),y=seq(0,1,0.01),z = ~matrix)%>%
  add_surface() %>% 
  layout(scene = list(xaxis=list(title="involution"),
                      yaxis=list(title="sit up"),
                      zaxis=list(title="value")
  ))
p2
htmlwidgets::saveWidget(as_widget(p2), "Exp1/M100-p2.html")

#(3)xL.
matrix<-matrix(data$xL.,nrow = 101, byrow = FALSE)#上面是xD

p3<-plot_ly(x=seq(0,1,0.01),y=seq(0,1,0.01),z = ~matrix)%>%
  add_surface() %>% 
  layout(scene = list(xaxis=list(title="involution"),
                      yaxis=list(title="sit up"),
                      zaxis=list(title="value")
  )) 
p3
htmlwidgets::saveWidget(as_widget(p3), "Exp1/M100-p3.html")


#4.交点可视化---三种策略的稳定解
data1<-data %>% 
  select(xD,xC,xD.,xC.,xL.) %>% 
  subset(xD.==0 | xC.==0 |xL.==0) %>% 
  gather(key = "strategies","value",-c(1,2))

data2<-data %>% 
  select(xD,xC,xD.,xC.,xL.) %>% 
  subset(xD.==0 & xC.==0 &xL.==0) 

p4<-plot_ly(data1,x=~xD,y=~xC,z=~value,color=~strategies) %>% 
  add_markers(size=1) %>% 
  layout(scene = list(xaxis=list(title="involution"),
                      yaxis=list(title="sit up"),
                      zaxis=list(title="value")
  )) %>% 
  add_markers(data2,x=~data2$xD,y=~data2$xC,z=0,color=I("red"))
p4

htmlwidgets::saveWidget(as_widget(p4), "Exp1/M100-p4.html")



#-----------------------------------------------------------------------------------#
#-----------------------------------四、M=50---------------------------------------#
#-----------------------------------------------------------------------------------#
#1.构建参数集
parameters<-data.frame(xD=rep(seq(0,1,0.01),each=101),xC=rep(seq(0,1,0.01),101)) %>%
  mutate(Add=xD+xC) %>% 
  mutate(xD=case_when(Add>1~0.3,T~xD)) %>% 
  mutate(xC=case_when(Add>1~0.3,T~xC))

#2.求解收集
Temp<-data.frame(xD=0,xC=0,xL=0,N=0,M=0,betaD=0,betaC=0,d=0,c=0,l=0,xD.=0,xC.=0,xL.=0)
for(i in 1:nrow(parameters)){
  temp<-duplicate_dynamics3(xD=parameters$xD[i],xC=parameters$xC[i],
                            N=50,M=50,betaD=1.5,betaC=1.1,d=4,c=1,l=0.5) 
  
  Temp<-rbind(Temp,temp)
}

data<-Temp[-1,]

#3.数据格式处理与可视化
#(1)xD.
matrix<-matrix(data$xD.,nrow = 101, byrow = FALSE)#上面是xD,即x轴为xD

p1<-plot_ly(x=seq(0,1,0.01),y=seq(0,1,0.01),z = ~matrix)%>%
  add_surface() %>% 
  layout(scene = list(xaxis=list(title="involution"),
                      yaxis=list(title="sit up"),
                      zaxis=list(title="value")
  ))
p1
htmlwidgets::saveWidget(as_widget(p1), "Exp1/M50-p1.html")

#(2)xC.
matrix<-matrix(data$xC.,nrow = 101, byrow = FALSE)#上面是xD

p2<-plot_ly(x=seq(0,1,0.01),y=seq(0,1,0.01),z = ~matrix)%>%
  add_surface() %>% 
  layout(scene = list(xaxis=list(title="involution"),
                      yaxis=list(title="sit up"),
                      zaxis=list(title="value")
  ))
p2
htmlwidgets::saveWidget(as_widget(p2), "Exp1/M50-p2.html")

#(3)xL.
matrix<-matrix(data$xL.,nrow = 101, byrow = FALSE)#上面是xD

p3<-plot_ly(x=seq(0,1,0.01),y=seq(0,1,0.01),z = ~matrix)%>%
  add_surface() %>% 
  layout(scene = list(xaxis=list(title="involution"),
                      yaxis=list(title="sit up"),
                      zaxis=list(title="value")
  )) 
p3
htmlwidgets::saveWidget(as_widget(p3), "Exp1/M50-p3.html")


#4.交点可视化---三种策略的稳定解
data1<-data %>% 
  select(xD,xC,xD.,xC.,xL.) %>% 
  subset(xD.==0 | xC.==0 |xL.==0) %>% 
  gather(key = "strategies","value",-c(1,2))

data2<-data %>% 
  select(xD,xC,xD.,xC.,xL.) %>% 
  subset(xD.==0 & xC.==0 &xL.==0) 

p4<-plot_ly(data1,x=~xD,y=~xC,z=~value,color=~strategies) %>% 
  add_markers(size=1) %>% 
  layout(scene = list(xaxis=list(title="involution"),
                      yaxis=list(title="sit up"),
                      zaxis=list(title="value")
  )) %>% 
  add_markers(data2,x=~data2$xD,y=~data2$xC,z=0,color=I("red"))
p4

htmlwidgets::saveWidget(as_widget(p4), "Exp1/M50-p4.html")
