library(BB)

#---------------------------------------------------------------------#
#-----------------------------3.3 Experiment 3(1)---------------------#
#---------------------------------------------------------------------#
#1. Given an l, solving for the stable point values for a given set of xD, xC
helper = function(x){
  
  xD <- x[1]
  xC <- x[2]
  
  xL <- round(1 - xD - xC, 5)  # xL may be less than 0
  
  M <- 100
  
  N <- 50
  
  # Relative utility
  betaD <- 1.5  # 1. Issue: betaD is 1.5 times, but d is 8 times l
  betaC <- 1.1
  
  # Costs
  d <- 4
  c <- 1
  l <- l
  
  Pd <- 0 # Net cumulative profit of the involution group
  Pc <- 0 # Net cumulative profit of the sit-up group
  Pl <- 0 # Net cumulative profit of the lay-down group
  for (Nd in 0:(N - 1)){ 
    for (Nc in 0:(N - 1 - Nd)){
      # Expected profit for individual strategy choices
      Nl <- N - 1 - Nd - Nc
      pai_d <- betaD * d * M / (betaD * (Nd + 1) * d + betaC * Nc * c + Nl * l) - d 
      pai_c <- betaC * c * M / (betaD * Nd * d + betaC * (Nc + 1) * c + Nl * l) - c          
      pai_l <- l * M / (betaD * Nd * d + betaC * Nc * c + (Nl + 1) * l) - l 
      
      Pd <- Pd + choose(N - 1, Nd) * choose(N - 1 - Nd, Nl) * (xD^Nd) * ((1 - xL - xD)^Nc) * (xL^Nl) * pai_d
      Pc <- Pc + choose(N - 1, Nd) * choose(N - 1 - Nd, Nl) * (xD^Nd) * ((1 - xL - xD)^Nc) * (xL^Nl) * pai_c
      Pl <- Pl + choose(N - 1, Nd) * choose(N - 1 - Nd, Nl) * (xD^Nd) * ((1 - xL - xD)^Nc) * (xL^Nl) * pai_l
    }
  }
  # Set floating-point precision k
  k <- 10
  R_  <- xD * Pd + round(1 - xD - xL, k) * Pc + xL * Pl  ## Mean value
  xD. <- xD * round(Pd - R_, k)
  xC. <- round(1 - xL - xD, k) * round(Pc - R_, k)
  xL. <- xL * round(Pl - R_, k)
  
  return(c(xD., xC.)) 
}

#2. Given an l, iterating over the parameter range of xD\xC to find stable star values
get_star_by_l = function(l){
  all = data.frame()
  for (x in seq(0.1, 0.9, 0.1)){
    for (y in seq(0.1, round(1 - x, 1), 0.1)){
      out = dfsane(c(x, y), helper, control = list(trace = FALSE))
      if (out$convergence == 0  &  all(round(out$par, 2) < 1) & any(round(out$par, 2) > 0) & sum(out$par) < 1){
        out = round(out$par, 4)
        all = rbind(all, data.frame(xDstar = out[1], xCstar = out[2]))
      }
    }
  }
  out = unique.data.frame(all) %>% 
    mutate(l = rep(l, nrow(.)))
  
  return(out)
}


#------------------------------------------------------------------------------#
#-----------------------------3.3 Experiment 3(2) (beta)-----------------------#
#------------------------------------------------------------------------------#
#3. Given an l, solving for the stable point values for a given set of xD, xC
helper_beta = function(x){
  
  xD <- x[1]
  xC <- x[2]
  
  xL <- round(1 - xD - xC, 5)  # xL may be less than 0
  
  M <- 100
  
  N <- 50
  
  # Relative utility
  betaD <- 1.5  # 1. Issue: betaD is 1.5 times, but d is 8 times l
  betaC <- 1.1
  
  # Costs
  d <- 4
  c <- 1
  l <- l
  
  Pd <- 0 # Net cumulative profit of the involution group
  Pc <- 0 # Net cumulative profit of the sit-up group
  Pl <- 0 # Net cumulative profit of the lay-down group
  for (Nd in 0:(N - 1)){ 
    for (Nc in 0:(N - 1 - Nd)){
      # Expected profit for individual strategy choices
      Nl <- N - 1 - Nd - Nc
      
      betaD1 = betaD - (betaD - betaC) / (1 + exp((-10) * (Nd / N - 0.5)))
      betaC1 = betaC - (betaC - 1) / (1 + exp((-10) * (Nc / N - 0.5)))
      
      pai_d <- betaD1 * d * M / (betaD1 * (Nd + 1) * d + betaC1 * Nc * c + Nl * l) - d 
      pai_c <- betaC1 * c * M / (betaD1 * Nd * d + betaC1 * (Nc + 1) * c + Nl * l) - c          
      pai_l <- l * M / (betaD1 * Nd * d + betaC1 * Nc * c + (Nl + 1) * l) - l 
      
      Pd <- Pd + choose(N - 1, Nd) * choose(N - 1 - Nd, Nl) * (xD^Nd) * ((1 - xL - xD)^Nc) * (xL^Nl) * pai_d
      Pc <- Pc + choose(N - 1, Nd) * choose(N - 1 - Nd, Nl) * (xD^Nd) * ((1 - xL - xD)^Nc) * (xL^Nl) * pai_c
      Pl <- Pl + choose(N - 1, Nd) * choose(N - 1 - Nd, Nl) * (xD^Nd) * ((1 - xL - xD)^Nc) * (xL^Nl) * pai_l
    }
  }
  # Set floating-point precision k
  k <- 10
  R_  <- xD * Pd + round(1 - xD - xL, k) * Pc + xL * Pl  ## Mean value
  xD. <- xD * round(Pd - R_, k)
  xC. <- round(1 - xL - xD, k) * round(Pc - R_, k)
  xL. <- xL * round(Pl - R_, k)
  
  return(c(xD., xC.)) 
}

#4. Given an l, iterating over the parameter range of xD\xC to find stable star values
get_star_by_l_beta = function(l){
  all = data.frame()
  
  for (x in seq(0.1, 0.9, 0.1)){
    for (y in seq(0.1, round(1 - x, 1), 0.1)){
      out = dfsane(c(x, y), helper_beta, control = list(trace = FALSE))
      if (out$convergence == 0  &  all(round(out$par, 2) < 1) & any(round(out$par, 2) > 0) & sum(out$par) < 1){
        out = round(out$par, 4)
        all = rbind(all, data.frame(xDstar = out[1], xCstar = out[2]))
      }
    }
  }
  out = unique.data.frame(all) %>% 
    mutate(l = rep(l, nrow(.))) 
  
  return(out)
}






























