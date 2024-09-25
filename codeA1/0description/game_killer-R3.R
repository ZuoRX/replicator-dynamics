library(BB)

helper = function(x){
  
  xD <- x[1]
  xC <- x[2]
  
  if (xD + xC > 1) {
    result <- 99999
  } else {
    xL <- round(1 - xD - xC, 5)  # xL may be less than 0
    
    M <- 200
    
    N <- 50
    
    # Relative utility
    betaD <- 1.5  # 1. Unreasonable point: betaD is 1.5 times, but d is 8 times l
    betaC <- 1.1
    
    # Cost
    d <- 4
    c <- 1
    l <- 0.5
    
    # Initial proportions
    # xD = ysx[1] # Previous y
    # xC = ysx[2] # Previous s
    # xL = ysx[3] # Previous x
    
    Pd <- 0 # Net cumulative benefit of involution group
    Pc <- 0 # Net cumulative benefit of sit-up group
    Pl <- 0 # Net cumulative benefit of lying-flat group
    for (Nd in 0:(N-1)){ 
      for (Nc in 0:(N-1-Nd)){
        # Expected benefit of individual strategy selection
        Nl <- N-1-Nd-Nc
        pai_d <- betaD * d * M / (betaD * (Nd + 1) * d + betaC * Nc * c + Nl * l) - d 
        pai_c <- betaC * c * M / (betaD * Nd * d + betaC * (Nc + 1) * c + Nl * l) - c          
        pai_l <- l * M / (betaD * Nd * d + betaC * Nc * c + (Nl + 1) * l) - l 
        
        Pd <- Pd + choose(N-1, Nd) * choose(N-1-Nd, Nl) * (xD^Nd) * ((1 - xL - xD)^Nc) * (xL^Nl) * pai_d
        Pc <- Pc + choose(N-1, Nd) * choose(N-1-Nd, Nl) * (xD^Nd) * ((1 - xL - xD)^Nc) * (xL^Nl) * pai_c
        Pl <- Pl + choose(N-1, Nd) * choose(N-1-Nd, Nl) * (xD^Nd) * ((1 - xL - xD)^Nc) * (xL^Nl) * pai_l
      }
    }
    # Set floating point precision k
    k <- 10
    R_ <- xD * Pd + round(1 - xD - xL, k) * Pc + xL * Pl  # Mean
    xD. <- xD * round(Pd - R_, k)
    xC. <- round(1 - xL - xD, k) * round(Pc - R_, k)
    xL. <- xL * round(Pl - R_, k)
    
    result <- abs(xL.)
  }
  
  min(result) 
}

# 1. Basic 3-strategy replicator dynamics function
duplicate_dynamics3 <- function(xD, xC, N, M, betaD, betaC, d, c, l) {
  M <- M
  
  xL <- round(1 - xD - xC, 2)
  
  N <- N # 50
  
  # Relative utility
  betaD <- betaD # 1.5  # 1. Unreasonable point: betaD is 1.5 times, but d is 8 times l
  betaC <- betaC # 1.1
  
  # Cost
  d <- d # 4
  c <- c # 1
  l <- l # 0.5
  
  Pd <- 0 # Net cumulative benefit of involution group
  Pc <- 0 # Net cumulative benefit of sit-up group
  Pl <- 0 # Net cumulative benefit of lying-flat group
  for (Nd in 0:(N-1)){ 
    for (Nc in 0:(N-1-Nd)){
      Nl <- N-1-Nd-Nc
      
      # Expected benefit of individual strategy selection
      pai_d <- betaD * d * M / (betaD * (Nd + 1) * d + betaC * Nc * c + Nl * l) - d 
      pai_c <- betaC * c * M / (betaD * Nd * d + betaC * (Nc + 1) * c + Nl * l) - c          
      pai_l <- l * M / (betaD * Nd * d + betaC * Nc * c + (Nl + 1) * l) - l 
      
      Pd <- Pd + choose(N-1, Nd) * choose(N-1-Nd, Nl) * (xD^Nd) * ((1 - xL - xD)^Nc) * (xL^Nl) * pai_d
      Pc <- Pc + choose(N-1, Nd) * choose(N-1-Nd, Nl) * (xD^Nd) * ((1 - xL - xD)^Nc) * (xL^Nl) * pai_c
      Pl <- Pl + choose(N-1, Nd) * choose(N-1-Nd, Nl) * (xD^Nd) * ((1 - xL - xD)^Nc) * (xL^Nl) * pai_l
    }
  }
  # Set floating point precision k
  k <- 10
  R_ <- xD * Pd + xC * Pc + xL * Pl  # Mean
  xD. <- xD * round(Pd - R_, k)
  xC. <- round(1 - xL - xD, k) * round(Pc - R_, k)
  xL. <- xL * round(Pl - R_, k)
  
  return (c(xD, xC, xL, N, M, betaD, betaC, d, c, l, xD., xC., xL.))
}

# 2. Experiment one: Adjust M
get_point_by_M <- function(M) {
  parameters <- data.frame(xD = rep(seq(0, 1, 0.01), each = 101), xC = rep(seq(0, 1, 0.01), 101)) %>%
    mutate(Add = xD + xC) %>% 
    mutate(xD = case_when(Add > 1 ~ 0.3, T ~ xD)) %>% 
    mutate(xC = case_when(Add > 1 ~ 0.3, T ~ xC))
  
  Temp <- data.frame(xD = 0, xC = 0, xL = 0, N = 0, M = 0, betaD = 0, betaC = 0, d = 0, c = 0, l = 0, xD. = 0, xC. = 0, xL. = 0)
  for (i in 1:nrow(parameters)) {
    temp <- duplicate_dynamics3(xD = parameters$xD[i], xC = parameters$xC[i],
                                N = 50, M = M, betaD = 1.5, betaC = 1.1, d = 4, c = 1, l = 0.5) 
    Temp <- rbind(Temp, temp)
  }
  
  data <- Temp[-1,]
}

# 3. Supplementary interface data collection
parameters <- data.frame(xD = rep(seq(0, 1, 0.01), each = 101), xC = rep(seq(0, 1, 0.01), 101)) %>%
  mutate(Add = xD + xC) %>% 
  mutate(xD = case_when(Add > 1 ~ 0.3, T ~ xD)) %>% 
  mutate(xC = case_when(Add > 1 ~ 0.3, T ~ xC))

get_data_by_M <- function(M) {
  Temp <- data.frame(xD = 0, xC = 0, xL = 0, N = 0, M = 0, betaD = 0, betaC = 0, d = 0, c = 0, l = 0, xD. = 0, xC. = 0, xL. = 0)
  for (i in 1:nrow(parameters)) {
    temp <- duplicate_dynamics3(xD = parameters$xD[i], xC = parameters$xC[i],
                                N = 50, M = M, betaD = 1.5, betaC = 1.1, d = 4, c = 1, l = 0.5) 
    
    Temp <- rbind(Temp, temp)
  }
  
  data <- Temp[-1,] %>% 
    subset(xD. == 0 | xC. == 0 | xL. == 0)
}
