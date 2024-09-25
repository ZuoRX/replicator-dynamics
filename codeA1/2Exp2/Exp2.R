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

#setwd("/home/zuo_r/involution/code/2Exp2")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#---------------------------------------------------------------------#
#-----------------------------2.1 Experiment 1------------------------#
#---------------------------------------------------------------------#
source("udf_Exp2.R")
n_core <- detectCores(logical = FALSE)

t1 <- Sys.time()
system.time({
  cl <- makeCluster(n_core)
  registerDoParallel(cl)       # Register processes
  clusterEvalQ(cl, {
    result <- data.frame()
    library(tidyverse)
    source("udf_Exp2.R")
  })
  result <- foreach(
    N = seq(5, 300, 2),
    .combine = rbind   # Combine the results
  ) %dopar% get_star_by_N(N)
  stopCluster(cl)
})

t2 <- Sys.time()
t2 - t1

write.csv(result, "result_Exp2.csv", row.names = FALSE)

#------------------------------------------------------------------------------#
#-----------------------------2.2 Experiment 1 (beta)--------------------------#
#------------------------------------------------------------------------------#
source("udf_Exp2.R")
n_core <- detectCores(logical = FALSE)

t1 <- Sys.time()
system.time({
  cl <- makeCluster(n_core)
  registerDoParallel(cl)       # Register processes
  clusterEvalQ(cl, {
    result <- data.frame()
    library(tidyverse)
    source("udf_Exp2.R")
  })
  result <- foreach(
    N = seq(5, 300, 5),
    .combine = rbind   # Combine the results
  ) %dopar% get_star_by_N_beta(N)
  stopCluster(cl)
})

t2 <- Sys.time()
t2 - t1

write.csv(result, "result_Exp2_beta.csv", row.names = FALSE)












