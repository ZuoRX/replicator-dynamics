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

#setwd("/home/zuo_r/involution/code/2Exp3-1")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#------------------------------------------------------------------#
#-------------------------Experiment 3 Visualization---------------#
#------------------------------------------------------------------#
df <- read.csv("result_Exp3-1.csv") %>%  
  arrange(d, xDstar) %>% 
  mutate(xDstar = round(xDstar, 2)) %>%
  mutate(xCstar = round(xCstar, 2)) %>% 
  unique.data.frame() %>% 
  mutate(xLstar = round(1 - xCstar - xDstar, 2))  

# Visualization of the 1st plot: xD & xC
df1 <- df %>% 
  subset(xDstar >= 0 & xCstar >= 0 & xLstar == 0) %>% 
  data.table() %>% 
  .[, n := seq(.N), by = d] %>% 
  subset(n == 1)

p1 <- df1 %>% 
  select(-n) %>% 
  gather("strategies", "value", -d) %>% 
  mutate(strategies = fct_reorder(strategies, c(rep(1, nrow(df1)), rep(2, nrow(df1)), rep(3, nrow(df1)))))

ggplot(p1, aes(x = d, y = value, color = strategies)) +
  geom_line() +
  scale_x_continuous(breaks = seq(min(df1$d), max(df1$d), 1)) +
  scale_color_discrete(labels = c("involution", "sit-up", "lay-down")) +
  ylab("Equilibrium Value") +
  xlab("d") +
  theme_few() +
  theme(legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.text = element_text(size = 16),
        legend.position = c(0.3, 0.9))

ggsave("Exp3-1.png", width = 20, height = 10, units = "cm", dpi = 300)

# Visualization of the 2nd plot: xD & xL
df1 <- df %>% 
  subset(xDstar >= 0 & xCstar == 0 & xLstar > 0) %>% 
  data.table() %>% 
  .[, n := seq(.N), by = d] %>% 
  subset(n == 1)

p1 <- df1 %>% 
  select(-n) %>% 
  gather("strategies", "value", -d) %>% 
  mutate(strategies = fct_reorder(strategies, c(rep(1, nrow(df1)), rep(2, nrow(df1)), rep(3, nrow(df1)))))

ggplot(p1, aes(x = d, y = value, color = strategies)) +
  geom_line() +
  scale_x_continuous(breaks = seq(min(df1$d), max(df1$d), 1)) +
  scale_color_discrete(labels = c("involution", "sit-up", "lay-down")) +
  ylab("Equilibrium Value") +
  xlab("d") +
  theme_few() +
  theme(legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.text = element_text(size = 16),
        legend.position = c(0.3, 0.9))

ggsave("Exp3-2.png", width = 20, height = 10, units = "cm", dpi = 300)

# Visualization of the 3rd plot: xC & xL <----------- not available
df1 <- df %>%
  subset(xDstar == 0 & xCstar > 0 & xLstar > 0) 
# 
# p1 <- df1 %>% 
#   select(-n) %>% 
#   gather("strategies", "value", -d) %>% 
#   mutate(strategies = fct_reorder(strategies, c(rep(1, nrow(df1)), rep(2, nrow(df1)), rep(3, nrow(df1)))))
# 
# ggplot(p1, aes(x = d, y = value, color = strategies)) +
#   geom_line() +
#   scale_x_continuous(breaks = seq(min(df1$d), max(df1$d), 1)) +
#   scale_color_discrete(labels = c("involution", "sit-up", "lay-down")) +
#   ylab("Equilibrium Value") +
#   xlab("d") +
#   theme_few() +
#   theme(legend.title = element_blank(),
#         legend.direction = "horizontal",
#         legend.text = element_text(size = 16),
#         legend.position = c(0.3, 0.9))
# 
# ggsave("Exp3-3.png", width = 20, height = 10, units = "cm", dpi = 300)


#------------------------------------------------------------------------#
#-------------------------Experiment 3 Visualization (beta)--------------#
#------------------------------------------------------------------------#
df <- read.csv("result_Exp3-1_beta.csv") %>%  
  arrange(d, xDstar) %>% 
  mutate(xDstar = round(xDstar, 2)) %>%
  mutate(xCstar = round(xCstar, 2)) %>% 
  unique.data.frame() %>% 
  mutate(xLstar = round(1 - xCstar - xDstar, 2))  

# Visualization of the 1st plot: xD & xC (beta)
df1 <- df %>% 
  subset(xDstar >= 0 & xCstar >= 0 & xLstar == 0) %>% 
  data.table() %>% 
  .[, n := seq(.N), by = d] %>% 
  subset(n == 1)

p1 <- df1 %>% 
  select(-n) %>% 
  gather("strategies", "value", -d) %>% 
  mutate(strategies = fct_reorder(strategies, c(rep(1, nrow(df1)), rep(2, nrow(df1)), rep(3, nrow(df1)))))

ggplot(p1, aes(x = d, y = value, color = strategies)) +
  geom_line() +
  scale_x_continuous(breaks = seq(min(df1$d), max(df1$d), 1)) +
  scale_color_discrete(labels = c("involution", "sit-up", "lay-down")) +
  ylab("Equilibrium Value") +
  xlab("d") +
  theme_few() +
  theme(legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.text = element_text(size = 16),
        legend.position = c(0.3, 0.9))

ggsave("Exp3-1-beta.png", width = 20, height = 10, units = "cm", dpi = 300)

# Visualization of the 2nd plot: xD & xL (beta)
df1 <- df %>% 
  subset(xDstar >= 0 & xCstar == 0 & xLstar > 0) %>% 
  data.table() %>% 
  .[, n := seq(.N), by = d] %>% 
  subset(n == 1)

p1 <- df1 %>% 
  select(-n) %>% 
  gather("strategies", "value", -d) %>% 
  mutate(strategies = fct_reorder(strategies, c(rep(1, nrow(df1)), rep(2, nrow(df1)), rep(3, nrow(df1)))))

ggplot(p1, aes(x = d, y = value, color = strategies)) +
  geom_line() +
  scale_x_continuous(breaks = seq(min(df1$d), max(df1$d), 1)) +
  scale_color_discrete(labels = c("involution", "sit-up", "lay-down")) +
  ylab("Equilibrium Value") +
  xlab("d") +
  theme_few() +
  theme(legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.text = element_text(size = 16),
        legend.position = c(0.3, 0.92))

ggsave("Exp3-2-beta.png", width = 20, height = 10, units = "cm", dpi = 300)

# Visualization of the 3rd plot: xC & xL <----------- not available
df1 <- df %>%
  subset(xDstar == 0 & xCstar > 0 & xLstar > 0) 
# 
# p1 <- df1 %>% 
#   select(-n) %>% 
#   gather("strategies", "value", -d) %>% 
#   mutate(strategies=fct_reorder(strategies,c(rep(1,nrow(df1)),rep(2,nrow(df1)),rep(3,nrow(df1)))))
# 
# ggplot(p1,aes(x=d,y=value,color=strategies))+
#   geom_line()+
#   scale_x_continuous(breaks = seq(min(df1$d),max(df1$d),1))+
#   scale_color_discrete(labels=c("involution","sit-up","lay-down"))+
#   ylab("Equilibrium Value")+
#   xlab("d")+
#   theme_few()+
#   theme(legend.title = element_blank(),
#         legend.direction  = "horizontal",
#         legend.text = element_text(size=16),
#         legend.position = c(0.3,0.9))
# 
# ggsave("Exp3-3.png",width = 20,height = 10,units = "cm",dpi = 300)













