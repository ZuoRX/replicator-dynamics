library(BB)

#1.给定一个M，在一组xD、xC下求解稳定point值
helper = function(x){
  
  xD<-x[1]
  xC<-x[2]
  
  xL<-round(1-xD-xC,5)  #xL可能小于0
  
  M<-M
  
  N<-50
  
  #相对效用
  betaD<-1.5  #1.不合理的地方：betaD是1.5倍，d确是l的8倍
  betaC<-1.1
  
  #成本
  d<-4
  c<-1
  l<-0.5
  
  Pd<-0 #内卷群体    的累加净收益
  Pc<-0 #sit-up群体  的累加净收集
  Pl<-0 #躺平群体    的累加净收益
  for (Nd in 0:(N-1)){ 
    for (Nc in 0:(N-1-Nd)){
      #个体策略选择的期望收益
      Nl <-N-1-Nd-Nc
      pai_d <- betaD*d*M/(betaD*(Nd+1)*d +betaC*Nc*c     +Nl*l)-d 
      pai_c <- betaC*c*M/(betaD*Nd*d     +betaC*(Nc+1)*c +Nl*l)-c          
      pai_l <-       l*M/(betaD*Nd*d     +betaC*Nc*c     +(Nl+1)*l)-l 
      
      Pd <- Pd + choose(N-1,Nd)*choose(N-1-Nd,Nl)*(xD^Nd)*((1-xL-xD)^Nc)*(xL^Nl)*pai_d
      Pc <- Pc + choose(N-1,Nd)*choose(N-1-Nd,Nl)*(xD^Nd)*((1-xL-xD)^Nc)*(xL^Nl)*pai_c
      Pl <- Pl + choose(N-1,Nd)*choose(N-1-Nd,Nl)*(xD^Nd)*((1-xL-xD)^Nc)*(xL^Nl)*pai_l
    }
  }
  #设置浮点数k
  k<-10
  R_  <- xD*Pd+round(1-xD-xL,k)*Pc+xL*Pl  ##均值
  xD. <- xD*round(Pd-R_,k)
  xC. <- round(1-xL-xD,k)*round(Pc-R_,k)
  xL. <- xL*round(Pl-R_,k)
  
  return(c(xD.,xC.)) 
}

#2.给定一个M，遍历xD\xC的参数范围，求解稳定的star值
get_star_by_M = function(M){
  all =data.frame()
  for (x in seq(0.1,0.9,0.1)){
    for (y in seq(0.1,round(1-x,1),0.1)){
      out = dfsane(c(x,y),helper,control=list(trace=FALSE))
      if (out$convergence==0  &  all(round(out$par,2)<1) & any(round(out$par,2)>0) & sum(out$par)<1){
        out = round(out$par,4)
        all=rbind(all,data.frame(x=out[1],y=out[2]))
      }
    }
  }
  out = unique.data.frame(all) %>% 
    mutate(M=rep(M,nrow(.))) 
  
  return(out)
}



#------------------------------------------------------------------------------#
#-----------------------------1.2实验一（beta）--------------------------------#
#------------------------------------------------------------------------------#
#3.给定一个M，在一组xD、xC下求解稳定point值
helper_beta = function(x){
  
  xD<-x[1]
  xC<-x[2]
  
  xL<-round(1-xD-xC,5)  #xL可能小于0
  
  M<-M
  
  N<-50
  
  #相对效用
  betaD<-1.5  #1.不合理的地方：betaD是1.5倍，d确是l的8倍
  betaC<-1.1
  
  #成本
  d<-4
  c<-1
  l<-0.5
  
  Pd<-0 #内卷群体    的累加净收益
  Pc<-0 #sit-up群体  的累加净收集
  Pl<-0 #躺平群体    的累加净收益
  for (Nd in 0:(N-1)){ 
    for (Nc in 0:(N-1-Nd)){
      #个体策略选择的期望收益
      Nl <-N-1-Nd-Nc
      
      betaD1 = betaD - (betaD-betaC)/(1+exp((-10)*(Nd/N-0.5)))
      betaC1 = betaC - (betaC-1)/(1+exp((-10)*(Nc/N-0.5)))
      
      pai_d <- betaD1*d*M/(betaD1*(Nd+1)*d +betaC1*Nc*c     +Nl*l)-d 
      pai_c <- betaC1*c*M/(betaD1*Nd*d     +betaC1*(Nc+1)*c +Nl*l)-c          
      pai_l <-        l*M/(betaD1*Nd*d     +betaC1*Nc*c     +(Nl+1)*l)-l 
      
      Pd <- Pd + choose(N-1,Nd)*choose(N-1-Nd,Nl)*(xD^Nd)*((1-xL-xD)^Nc)*(xL^Nl)*pai_d
      Pc <- Pc + choose(N-1,Nd)*choose(N-1-Nd,Nl)*(xD^Nd)*((1-xL-xD)^Nc)*(xL^Nl)*pai_c
      Pl <- Pl + choose(N-1,Nd)*choose(N-1-Nd,Nl)*(xD^Nd)*((1-xL-xD)^Nc)*(xL^Nl)*pai_l
    }
  }
  #设置浮点数k
  k<-10
  R_  <- xD*Pd+round(1-xD-xL,k)*Pc+xL*Pl  ##均值
  xD. <- xD*round(Pd-R_,k)
  xC. <- round(1-xL-xD,k)*round(Pc-R_,k)
  xL. <- xL*round(Pl-R_,k)
  
  return(c(xD.,xC.)) 
}



#4.给定一个M，遍历xD\xC的参数范围，求解稳定的star值
get_star_by_M_beta = function(M){
  all =data.frame()
  for (x in seq(0.1,0.9,0.1)){
    for (y in seq(0.1,round(1-x,1),0.1)){
      out = dfsane(c(x,y),helper_beta,control=list(trace=FALSE))
      if (out$convergence==0  &  all(round(out$par,2)<1) & any(round(out$par,2)>0) & sum(out$par)<1){
        out = round(out$par,4)
        all=rbind(all,data.frame(xDstar=out[1],xCstar=out[2]))
      }
    }
  }
  out = unique.data.frame(all) %>% 
    mutate(M=rep(M,nrow(.))) 
  
  return(out)
}


















