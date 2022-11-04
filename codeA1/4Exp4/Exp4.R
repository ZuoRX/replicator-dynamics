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

#setwd("/home/zuo_r/involution/code/4Exp4")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#---------------------------------------------------------------------#
#-----------------------------4实验一--------------------------------#
#---------------------------------------------------------------------#
source("udf_Exp4.R")
n_core<-detectCores(logical = F)

t1<-Sys.time()
system.time({
  cl<- makeCluster(n_core)
  registerDoParallel(cl)       #进行进程注册
  clusterEvalQ(cl,{
    result<-data.frame()
    library(tidyverse)
    source("udf_Exp4.R")
  })
  result <- foreach(
    t=seq(0.01,1,0.01),
    .combine=rbind   #返回结果的整合
  ) %dopar% get_star_by_t(t)
  stopCluster(cl)
})

t2<-Sys.time()
t2-t1

write.csv(result,"result_Exp4.csv",row.names = F)

#------------------------------------------------------------------------------#
#-----------------------------3.1实验二（beta）--------------------------------#
#------------------------------------------------------------------------------#
source("udf_Exp4.R")

n_core<-detectCores(logical = F)

t1<-Sys.time()
system.time({
  cl<- makeCluster(n_core)
  registerDoParallel(cl)       #进行进程注册
  clusterEvalQ(cl,{
    result<-data.frame()
    library(tidyverse)
    source("udf_Exp4.R")
  })
  result <- foreach(
    t=seq(0.01,1,0.01),
    .combine=rbind   #返回结果的整合
  ) %dopar% get_star_by_t_beta(t)
  stopCluster(cl)
})

t2<-Sys.time()
t2-t1
# hours

write.csv(result,"result_Exp4_beta.csv",row.names = F)












