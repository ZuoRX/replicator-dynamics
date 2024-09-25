library(BB)

#---------------------------------------------------------------------#
#----------------------------- Experiment 4 (1) --------------------------------#
#---------------------------------------------------------------------#
# 1. Given a betaD, solve for the equilibrium point value under a set of xD and xC
helper = function(x){
  
  xD <- x[1]
  xC <- x[2]
  
  xL <- round(1 - xD - xC, 5)  # xL may be less than 0
  
  M <- 100
  
  N <- 50
  
  # Relative utility
  betaD <- betaD  # 1. Unreasonable situation: betaD is 1.5x, but d is 8x of l
  betaC <- 1.1
  
  # Costs
  d <- 4
  c <- 1
  l <- 0.5
  
  Pd <- 0 # Accumulated net payoff of the involution group
  Pc <- 0 # Accumulated net payoff of the sit-up group
  Pl <- 0 # Accumulated net payoff of the lay-down group
  for (Nd in 0:(N - 1)){ 
    for (Nc in 0:(N - 1 - Nd)){
      # Expected payoff for individual strategy selection
      Nl <- N - 1 - Nd - Nc
      pai_d <- betaD * d * M / (betaD * (Nd + 1) * d + betaC * Nc * c + Nl * l) - d 
      pai_c <- betaC * c * M / (betaD * Nd * d + betaC * (Nc + 1) * c + Nl * l) - c          
      pai_l <- l * M / (betaD * Nd * d + betaC * Nc * c + (Nl + 1) * l) - l 
      
      Pd <- Pd + choose(N - 1, Nd) * choose(N - 1 - Nd, Nl) * (xD^Nd) * ((1 - xL - xD)^Nc) * (xL^Nl) * pai_d
      Pc <- Pc + choose(N - 1, Nd) * choose(N - 1 - Nd, Nl) * (xD^Nd) * ((1 - xL - xD)^Nc) * (xL^Nl) * pai_c
      Pl <- Pl + choose(N - 1, Nd) * choose(N - 1 - Nd, Nl) * (xD^Nd) * ((1 - xL - xD)^Nc) * (xL^Nl) * pai_l
    }
  }
  # Set floating point precision
  k <- 10
  R_  <- xD * Pd + round(1 - xD - xL, k) * Pc + xL * Pl  ## Mean value
  xD. <- xD * round(Pd - R_, k)
  xC. <- round(1 - xL - xD, k) * round(Pc - R_, k)
  xL. <- xL * round(Pl - R_, k)
  
  return(c(xD., xC.)) 
}

# 2. Given a betaD, iterate through the parameter range of xD and xC to solve for the equilibrium star value
get_star_by_betaD = function(betaD){
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
    mutate(betaD = rep(betaD, nrow(.)))
  
  return(out)
}

#------------------------------------------------------------------------------#
#----------------------------- Experiment 4 (2) (beta) ------------------------#
#------------------------------------------------------------------------------#
# 3. Given a betaD, solve for the equilibrium point value under a set of xD and xC
helper_beta = function(x){
  
  xD <- x[1]
  xC <- x[2]
  
  xL <- round(1 - xD - xC, 5)  # xL may be less than 0
  
  M <- 100
  
  N <- 50
  
  # Relative utility
  betaD <- betaD  # 1. Unreasonable situation: betaD is 1.5x, but d is 8x of l
  betaC <- 1.1
  
  # Costs
  d <- 4
  c <- 1
  l <- 0.5
  
  Pd <- 0 # Accumulated net payoff of the involution group
  Pc <- 0 # Accumulated net payoff of the sit-up group
  Pl <- 0 # Accumulated net payoff of the lay-down group
  for (Nd in 0:(N - 1)){ 
    for (Nc in 0:(N - 1 - Nd)){
      # Expected payoff for individual strategy selection
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
  # Set floating point precision
  k <- 10
  R_  <- xD * Pd + round(1 - xD - xL, k) * Pc + xL * Pl  ## Mean value
  xD. <- xD * round(Pd - R_, k)
  xC. <- round(1 - xL - xD, k) * round(Pc - R_, k)
  xL. <- xL * round(Pl - R_, k)
  
  return(c(xD., xC.)) 
}

# 4. Given a betaD, iterate through the parameter range of xD and xC to solve for the equilibrium star value
get_star_by_betaD_beta = function(betaD){
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
    mutate(betaD = rep(betaD, nrow(.))) 
  
  return(out)
}






























