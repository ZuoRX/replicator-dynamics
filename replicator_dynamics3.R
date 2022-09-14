setwd("/home/zuo_r/involution")

#source("replicator_dynamics3.R")

replicator_dynamics_xy<-function(y,x,M,beta1,beta2,d,N,c,l){
  
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
  
  result1<-data.frame(y=y,x=x,M=M,beta1=beta1,beta2=beta2,d=d,N=N,c=c,l=l,y.=y.,x.=x.)
  return(result1)
}


#1.
get_data_by_yx<-function(i){
  
  y<-parameters$y[i]
  x<-parameters$x[i]
  
  temp<-replicator_dynamics_xy(y,x,M,beta1,beta2,d,N,c,l)
  data<-cbind(i,temp)
}


#2.
get_data_by_parameters1<-function(i){
  
  y<-parameters1$y[i]
  x<-parameters1$x[i]
  M<-parameters1$M[i]
  beta1<-parameters1$beta1[i]
  beta2<-parameters1$beta2[i]
  d<-parameters1$d[i]
  c<-parameters1$c[i]
  l<-parameters1$l[i]
  
  temp<-replicator_dynamics_xy(y,x,M,beta1,beta2,d,N,c,l)
  
  gc()
  
  data<-cbind(i,temp)
}







