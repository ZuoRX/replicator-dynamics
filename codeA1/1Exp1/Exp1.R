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

#setwd("/home/zuo_r/involution/code/1Exp1")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("udf_Exp1.R")

#---------------------------------------------------------------------#
#-----------------------------1.1 Experiment 1-------------------------#
#---------------------------------------------------------------------#
n_core <- detectCores(logical = FALSE)

t1 <- Sys.time()
system.time({
  cl <- makeCluster(n_core)
  registerDoParallel(cl)       # Register processes
  clusterEvalQ(cl, {
    result <- data.frame()
    library(tidyverse)
    source("udf_Exp1.R")
  })
  result <- foreach(
    M = 1:300,
    .combine = rbind   # Combine the results
  ) %dopar% get_star_by_M(M)
  stopCluster(cl)
})

t2 <- Sys.time()
t2 - t1

write.csv(result, "result_Exp1.csv", row.names = FALSE)


#------------------------------------------------------------------------------#
#-----------------------------1.2 Experiment 1 (beta)--------------------------#
#------------------------------------------------------------------------------#
n_core <- detectCores(logical = FALSE)

t1 <- Sys.time()
system.time({
  cl <- makeCluster(n_core)
  registerDoParallel(cl)       # Register processes
  clusterEvalQ(cl, {
    result <- data.frame()
    library(tidyverse)
    source("udf_Exp1.R")
  })
  result <- foreach(
    M = 1:300,
    .combine = rbind   # Combine the results
  ) %dopar% get_star_by_M_beta(M)
  stopCluster(cl)
})

t2 <- Sys.time()
t2 - t1

write.csv(result, "result_Exp1_beta.csv", row.names = FALSE)

