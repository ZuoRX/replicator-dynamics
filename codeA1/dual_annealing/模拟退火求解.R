suppressPackageStartupMessages(suppressWarnings({
  library(rstudioapi)
  library(tidyverse)
  library(data.table)
  library(progress)
  library(parallel)
  library(foreach)
  library(doParallel)
  library(EvolutionaryGames)
  library(ggthemes)
  library(latex2exp)
  library(ggtext)
  library(progress)
  library(plotly)
  library(RColorBrewer)
  library(BB)
}))

#setwd("/home/zuo_r/involution/code/4Exp5")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#---------------------------------------------------------------------#
#----------------------------- Experiment 4 --------------------------------#
#---------------------------------------------------------------------#
source("udf_Exp5.R")
n_core <- detectCores(logical = F)

t1 <- Sys.time()
system.time({
  cl <- makeCluster(n_core)
  registerDoParallel(cl)       # Register parallel processing
  clusterEvalQ(cl,{
    result <- data.frame()
    library(tidyverse)
    source("udf_Exp5.R")
  })
  result <- foreach(
    betaD = seq(1.2, 10, 0.1),
    .combine = rbind   # Combine the results
  ) %dopar% get_star_by_betaD(betaD)
  stopCluster(cl)
})

t2 <- Sys.time()
t2 - t1

write.csv(result, "result_Exp5.csv", row.names = F)

#------------------------------------------------------------------------------#
#----------------------------- Experiment 3.1 (beta) --------------------------------#
#------------------------------------------------------------------------------#
source("udf_Exp5.R")

n_core <- detectCores(logical = F)

t1 <- Sys.time()
system.time({
  cl <- makeCluster(n_core)
  registerDoParallel(cl)       # Register parallel processing
  clusterEvalQ(cl,{
    result <- data.frame()
    library(tidyverse)
    source("udf_Exp5.R")
  })
  result <- foreach(
    betaD = seq(1.2, 10, 0.1),
    .combine = rbind   # Combine the results
  ) %dopar% get_star_by_betaD_beta(betaD)
  stopCluster(cl)
})

t2 <- Sys.time()
t2 - t1
# hours

write.csv(result, "result_Exp5_beta.csv", row.names = F)
