library(BB)

helper = function(x){
  
  xD<-x[1]
  xC<-x[2]
  xL<-round(1-xD-xC,5)
  
  M<-200
  
  N<-50
  
  #相对效用
  betaD<-1.5  #1.不合理的地方：betaD是1.5倍，d确是l的8倍
  betaC<-1.1
  
  #成本
  d<-4
  c<-1
  l<-0.5
  
  #初始比例
  # xD = ysx[1] #原来的y
  # xC = ysx[2] #原来的s
  # xL <-ysx[3] #原来的x
  
  #初始数量-取整
  Nd <- floor(xD*(N-1)) #1
  Nl <- floor(xC*(N-1)) #3
  Nc <- N-1-Nd-Nl       #2 因为有个减法，所以放在最下面位置，否则保持d,c,l
  
  
  #个体策略选择的期望收益
  pai_d <- betaD*d*M/(betaD*(Nd+1)*d +betaC*Nc*c     +Nl*l)-d 
  pai_c <- betaC*c*M/(betaD*Nd*d     +betaC*(Nc+1)*c +Nl*l)-c          
  pai_l <-       l*M/(betaD*Nd*d     +betaC*Nc*c     +(Nl+1)*l)-l 
  
  Pd<-0 #内卷群体    的累加净收益
  Pc<-0 #sit-up群体  的累加净收集
  Pl<-0 #躺平群体    的累加净收益
  for (Nd in 0:(N-1)){ 
    for (Nl in 0:(N-1-Nd)){
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
  
  xD.abs<-abs(xD.)
  # xC.abs<-abs(xC.)
  # xL.abs<-abs(xL.)
  # 
  # result<-xD.abs+xC.abs+xL.abs
  
  min(xD.abs) 
}

duplicate_dynamics3<-function(xD,xC,xL,N,M,betaD,betaC,d,c,l){
  M<-M
  
  N<-N#50
  
  #相对效用
  betaD<-betaD#1.5  #1.不合理的地方：betaD是1.5倍，d确是l的8倍
  betaC<-betaC#1.1
  
  #成本
  d<-d#4
  c<-c#1
  l<-l#0.5
  
  #初始比例
  # xD = ysx[1] #原来的y
  # xC = ysx[2] #原来的s
  # xL <-ysx[3] #原来的x
  
  #初始数量-取整
  Nd <- floor(xD*(N-1)) #1
  Nl <- floor(xC*(N-1)) #3
  Nc <- N-1-Nd-Nl       #2 因为有个减法，所以放在最下面位置，否则保持d,c,l
  
  
  #个体策略选择的期望收益
  pai_d <- betaD*d*M/(betaD*(Nd+1)*d +betaC*Nc*c     +Nl*l)-d 
  pai_c <- betaC*c*M/(betaD*Nd*d     +betaC*(Nc+1)*c +Nl*l)-c          
  pai_l <-       l*M/(betaD*Nd*d     +betaC*Nc*c     +(Nl+1)*l)-l 
  
  Pd<-0 #内卷群体    的累加净收益
  Pc<-0 #sit-up群体  的累加净收集
  Pl<-0 #躺平群体    的累加净收益
  for (Nd in 0:(N-1)){ 
    for (Nl in 0:(N-1-Nd)){
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
  
  return (c(xD,xC,xL,N,M,betaD,betaC,d,c,l,xD.,xC.,xL.))
}




