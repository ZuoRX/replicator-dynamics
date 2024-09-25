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

#setwd("/home/zuo_r/involution/code/3Exp3/part3")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#---------------------------------------------------------------------#
#-----------------------------3.1 Experiment 1------------------------#
#---------------------------------------------------------------------#
source("udf_Exp3-3.R")
n_core <- detectCores(logical = FALSE)

t1 <- Sys.time()
system.time({
  cl <- makeCluster(n_core)
  registerDoParallel(cl)       # Register parallel processes
  clusterEvalQ(cl,{
    result <- data.frame()
    library(tidyverse)
    source("udf_Exp3-3.R")
  })
  result <- foreach(
    l = seq(0.01, 1, 0.01),
    .combine = rbind   # Combine the results
  ) %dopar% get_star_by_l(l)
  stopCluster(cl)
})

t2 <- Sys.time()
t2 - t1

write.csv(result, "result_Exp3-3.csv", row.names = FALSE)

#------------------------------------------------------------------------------#
#-----------------------------3.1 Experiment 2 (beta)--------------------------#
#------------------------------------------------------------------------------#
source("udf_Exp3-3.R")

n_core <- detectCores(logical = FALSE)

t1 <- Sys.time()
system.time({
  cl <- makeCluster(n_core)
  registerDoParallel(cl)       # Register parallel processes
  clusterEvalQ(cl,{
    result <- data.frame()
    library(tidyverse)
    source("udf_Exp3-3.R")
  })
  result <- foreach(
    l = seq(0.01, 1, 0.01),
    .combine = rbind   # Combine the results
  ) %dopar% get_star_by_l_beta(l)
  stopCluster(cl)
})

t2 <- Sys.time()
t2 - t1
# 3.575636 hours

write.csv(result, "result_Exp3-3_beta.csv", row.names = FALSE)












