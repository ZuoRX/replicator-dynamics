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
#-----------------------------2.1实验二--------------------------------#
#---------------------------------------------------------------------#
source("udf_Exp2.R")
n_core<-7

t1<-Sys.time()
system.time({
  cl<- makeCluster(n_core)
  registerDoParallel(cl)       #进行进程注册
  clusterEvalQ(cl,{
    result<-data.frame()
    library(tidyverse)
    source("udf_Exp2.R")
  })
  result <- foreach(
    N=seq(10,200,2),
    .combine=rbind   #返回结果的整合
  ) %dopar% get_star_by_N(N)
  stopCluster(cl)
})

t2<-Sys.time()
t2-t1

write.csv(result,"result_Exp2.csv",row.names = F)

#------------------------------------------------------------------------------#
#-----------------------------2.2实验二（beta）--------------------------------#
#------------------------------------------------------------------------------#
source("udf_Exp2.R")

n_core<-7

t1<-Sys.time()
system.time({
  cl<- makeCluster(n_core)
  registerDoParallel(cl)       #进行进程注册
  clusterEvalQ(cl,{
    result<-data.frame()
    library(tidyverse)
    source("udf_Exp2.R")
  })
  result <- foreach(
    N=seq(10,200,2),
    .combine=rbind   #返回结果的整合
  ) %dopar% get_star_by_N_beta(N)
  stopCluster(cl)
})

t2<-Sys.time()
t2-t1


write.csv(result,"result_Exp2_beta.csv",row.names = F)












