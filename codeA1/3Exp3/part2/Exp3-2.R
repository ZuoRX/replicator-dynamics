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

#setwd("/home/zuo_r/involution/code/3Exp3/part2")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#---------------------------------------------------------------------#
#-----------------------------3.1 Experiment 1------------------------#
#---------------------------------------------------------------------#
source("udf_Exp3-2.R")
n_core <- detectCores(logical = F)

t1 <- Sys.time()
system.time({
  cl <- makeCluster(n_core)
  registerDoParallel(cl)       # Register processes
  clusterEvalQ(cl,{
    result <- data.frame()
    library(tidyverse)
    source("udf_Exp3-2.R")
  })
  result <- foreach(
    c = seq(0.5, 4, 0.1),
    .combine = rbind   # Combine the results
  ) %dopar% get_star_by_c(c)
  stopCluster(cl)
})

t2 <- Sys.time()
t2 - t1

write.csv(result, "result_Exp3-2.csv", row.names = F)

#------------------------------------------------------------------------------#
#-----------------------------3.1 Experiment 2 (beta)--------------------------#
#------------------------------------------------------------------------------#
source("udf_Exp3-2.R")

n_core <- detectCores(logical = F)

t1 <- Sys.time()
system.time({
  cl <- makeCluster(n_core)
  registerDoParallel(cl)       # Register processes
  clusterEvalQ(cl,{
    result <- data.frame()
    library(tidyverse)
    source("udf_Exp3-2.R")
  })
  result <- foreach(
    c = seq(0.5, 4, 0.1),
    .combine = rbind   # Combine the results
  ) %dopar% get_star_by_c_beta(c)
  stopCluster(cl)
})

t2 <- Sys.time()
t2 - t1
# 3.575636 hours

write.csv(result, "result_Exp3-2_beta.csv", row.names = F)












