library(BB)

#1.基础3策略复制动态函数
duplicate_dynamics3<-function(xD,xC,N,M,betaD,betaC,d,c,l){
  M<-M
  
  xL<-round(1-xD-xC,2)
  
  N<-N#50
  
  #相对效用
  betaD<-betaD#1.5  #1.不合理的地方：betaD是1.5倍，d确是l的8倍
  betaC<-betaC#1.1
  
  #成本
  d<-d#4
  c<-c#1
  l<-l#0.5

  Pd<-0 #内卷群体    的累加净收益
  Pc<-0 #sit-up群体  的累加净收集
  Pl<-0 #躺平群体    的累加净收益
  for (Nd in 0:(N-1)){ 
    for (Nc in 0:(N-1-Nd)){
      Nl<-N-1-Nd-Nc
      
      #个体策略选择的期望收益
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
  R_  <- xD*Pd+xC*Pc+xL*Pl  ##均值
  xD. <- xD*round(Pd-R_,k)
  xC. <- round(1-xL-xD,k)*round(Pc-R_,k)
  xL. <- xL*round(Pl-R_,k)
  
  return (c(xD,xC,xL,N,M,betaD,betaC,d,c,l,xD.,xC.,xL.))
}

#2.实验一调节M
get_point_by_M<-function(M){
  parameters<-data.frame(xD=rep(seq(0,1,0.01),each=101),xC=rep(seq(0,1,0.01),101)) %>%
    mutate(Add=xD+xC) %>% 
    mutate(xD=case_when(Add>1~0.3,T~xD)) %>% 
    mutate(xC=case_when(Add>1~0.3,T~xC))
  
  Temp<-data.frame(xD=0,xC=0,xL=0,N=0,M=0,betaD=0,betaC=0,d=0,c=0,l=0,xD.=0,xC.=0,xL.=0)
  for(i in 1:nrow(parameters)){
    temp<-duplicate_dynamics3(xD=parameters$xD[i],xC=parameters$xC[i],
                              N=50,M=M,betaD=1.5,betaC=1.1,d=4,c=1,l=0.5) 
    Temp<-rbind(Temp,temp)
  }
  
  data<-Temp[-1,]
}

#3.1模拟退火求解实验一---test
helper_dual_test = function(x){
  
  xD<-x[1]
  xC<-0
  
  if(xD+xC>1){
    result<-99999
  }else{
    xL<-round(1-xD-xC,5)  #xL可能小于0
    
    #M<-200
    
    N<-100
    
    #相对效用
    betaD<-1  #1.不合理的地方：betaD是1.5倍，d确是l的8倍
    betaC<-1.1
    
    #成本
    d<-4
    c<-1
    l<-1
    
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
    
    result<-abs(xD.)
  }
  
  min(result) 
}


#3.2模拟退火求解实验一
helper_dual = function(x){
  
  xD<-x[1]
  xC<-x[2]
  
  if(xD+xC>1){
    result<-99999
  }else{
    xL<-round(1-xD-xC,5)  #xL可能小于0
    
    #M<-200
    
    N<-100
    
    #相对效用
    betaD<-1.5  #1.不合理的地方：betaD是1.5倍，d确是l的8倍
    betaC<-1.1
    
    #成本
    d<-4    #内卷
    c<-1    #挣扎
    l<-0.5  #躺平
    
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
    
    result<-(xD.)^4+(xC.)^4+(xL.)^4
  }
  
  min(result) 
}