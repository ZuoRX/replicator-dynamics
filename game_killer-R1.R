library(BB)
helper = function(ysx){
  M<-M
  beta1<-1.5
  beta2<-1.1
  N<-50
  d<-4
  c<-1
  l<-0.5
  y = ysx[1]
  s = ysx[2]
  x <-ysx[3]
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
  R_ <- y*Pd+x*Pl+round(1-x-y,2)*Pc  ##均值
  y. <- y*round(Pd-R_,2)
  s.<-round(1-x-y,2)*round(Pc-R_,2)
  x. <- x*round(Pl-R_,2)
  return (c(y.,s.,x.))
}

#2.添加分母R_
helper1 = function(ysx){
  M<-M
  beta1<-1.5
  beta2<-1.1
  N<-50
  d<-4
  c<-1
  l<-0.5
  y = ysx[1]
  s = ysx[2]
  x <-ysx[3]
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
  R_ <- y*Pd+round(1-x-y,2)*Pc+x*Pl  ##均值
  y. <- y*round(Pd-R_,2)/log(M+1)
  s.<-round(1-x-y,2)*round(Pc-R_,2)/log(M+1)
  x. <- x*round(Pl-R_,2)/log(M+1)
  return (c(y.,s.,x.))
}
