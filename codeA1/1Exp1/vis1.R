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

#------------------------------------------------------------------#
#-------------------------Experiment 1 Visualization----------------#
#------------------------------------------------------------------#
df <- read.csv("result_Exp1.csv") %>%  
  subset(x >= 0 & y >= 0) %>% 
  arrange(M, x) %>% 
  mutate(xDstar = round(x, 2)) %>%
  mutate(xCstar = round(y, 2)) %>% 
  select(-x, -y) %>% 
  unique.data.frame() %>% 
  mutate(xLstar = round(1 - xCstar - xDstar, 2)) %>% 
  data.table() %>% 
  .[, n := seq(.N), by = M]

max(df$n)


# Visualization of the 1st plot
df1 <- df %>% 
  subset(n == 1) %>% 
  subset(M != 69) %>% 
  subset(M >= 40) %>% 
  mutate(xCstar = 1 - xDstar) %>% 
  mutate(xLstar = 0)  


p1 <- df1 %>% 
  select(-n) %>% 
  gather("strategies", "value", -1) %>% 
  mutate(strategies = fct_reorder(strategies, c(rep(1, nrow(df1)), rep(2, nrow(df1)), rep(3, nrow(df1))))) %>%  # Redefine legend order
  mutate(M1 = c(rep(df1$M[order(df1$M, decreasing = T)], 3)))

ggplot(p1, aes(x = M1, y = value, color = strategies)) +
  geom_line() +
  scale_x_continuous(breaks = seq(40, 190, 10),
                     labels = as.character(seq(190, 40, -10))) +
  scale_color_discrete(labels = c("involution", "sit-up", "lay-down")) +
  ylab("Equilibrium Value") +
  xlab("M") +
  theme_few() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.position = c(0.85, 0.5)
  )

ggsave("Exp1-1.png", width = 20, height = 10, units = "cm", dpi = 300)


# Visualization of the 2nd plot
df1 <- df %>% 
  subset(M >= 40) %>% 
  arrange(M, -xLstar) %>% 
  subset(xLstar != 0 & xDstar != 0) %>% 
  data.table() %>% 
  .[, n := seq(.N), by = M] %>% 
  subset(n == 1)

p1 <- df1 %>% 
  select(-n) %>% 
  gather("strategies", "value", -1) %>% 
  mutate(strategies = fct_reorder(strategies, c(rep(1, nrow(df1)), rep(2, nrow(df1)), rep(3, nrow(df1))))) %>%  # Redefine legend order
  mutate(M1 = c(rep(df1$M[order(df1$M, decreasing = T)], 3)))

ggplot(p1, aes(x = M1, y = value, color = strategies)) +
  geom_line() +
  scale_x_continuous(breaks = seq(42, 190, 10),
                     labels = as.character(seq(190, 42, -10))) +
  scale_color_discrete(labels = c("involution", "sit-up", "lay-down")) +
  ylab("Equilibrium Value") +
  xlab("M") +
  theme_few() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.position = c(0.85, 0.5)
  )


ggsave("Exp1-2.png", width = 20, height = 10, units = "cm", dpi = 300)


# Visualization of the 3rd plot
df1 <- df %>% 
  subset(M >= 40) %>% 
  subset(xDstar == 0) 

p1 <- df1 %>% 
  select(-n) %>% 
  gather("strategies", "value", -1) %>% 
  mutate(strategies = fct_reorder(strategies, c(rep(1, nrow(df1)), rep(2, nrow(df1)), rep(3, nrow(df1))))) %>%  # Redefine legend order
  mutate(M1 = c(rep(df1$M[order(df1$M, decreasing = T)], 3)))

ggplot(p1, aes(x = M1, y = value, color = strategies)) +
  geom_line() +
  scale_x_continuous(breaks = seq(41, 46, 1),
                     labels = as.character(seq(46, 41, -1))) +
  scale_color_discrete(labels = c("involution", "sit-up", "lay-down")) +
  ylab("Equilibrium Value") +
  xlab("M") +
  theme_few() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.position = c(0.85, 0.5)
  )


ggsave("Exp1-3.png", width = 20, height = 10, units = "cm", dpi = 300)



#------------------------------------------------------------------------#
#---------------------Experiment 1 Visualization (beta)-------------------#
#------------------------------------------------------------------------#
df <- read.csv("result_Exp1_beta.csv") %>%  
  mutate(xLstar = round(1 - xCstar - xDstar, 2)) %>% 
  subset(xDstar >= 0 & xCstar >= 0 & xLstar >= 0) %>% 
  arrange(M, xDstar) %>% 
  mutate(xDstar = round(xDstar, 2)) %>%
  mutate(xCstar = round(xCstar, 2)) %>% 
  unique.data.frame() %>% 
  data.table() %>% 
  .[, n := seq(.N), by = M]

max(df$n)


# Visualization of the 1st plot xD & xC
df1 <- df %>% 
  subset(M >= 35 & xDstar > 0) %>% 
  mutate(xCstar = 1 - xDstar) %>% 
  subset(n == 1) %>% 
  mutate(xLstar = 0)  


p1 <- df1 %>% 
  select(-n) %>% 
  gather("strategies", "value", -M) %>% 
  mutate(strategies = fct_reorder(strategies, c(rep(1, nrow(df1)), rep(2, nrow(df1)), rep(3, nrow(df1))))) %>%  # Redefine legend order
  mutate(M1 = c(rep(df1$M[order(df1$M, decreasing = T)], 3)))


ggplot(p1, aes(x = M1, y = value, color = strategies)) +
  geom_line() +
  scale_x_continuous(breaks = seq(36, 200, 10),
                     labels = as.character(seq(200, 36, -10))) +
  scale_color_discrete(labels = c("involution", "sit-up", "lay-down")) +
  ylab("Equilibrium Value") +
  xlab("M") +
  theme_few() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.position = c(0.85, 0.5)
  )

ggsave("Exp1-1-beta.png", width = 20, height = 10, units = "cm", dpi = 300)


# Visualization of the 2nd plot xD & xL
df1 <- df %>% 
  subset(M >= 35 & xLstar > 0) %>% 
  arrange(M, -xLstar) %>% 
  subset(xLstar != 0 & xDstar != 0) %>% 
  mutate(xDstar = 1 - xLstar) %>% 
  mutate(xCstar = 0) %>% 
  data.table() %>% 
  .[, n := seq(.N), by = M] %>% 
  subset(n == 1) %>% 
  subset(M != 135 & M != 136 & M != 140 & M != 141 & M != 146)

p1 <- df1 %>% 
  select(-n) %>% 
  gather("strategies", "value", -M) %>% #<-----
mutate(strategies = fct_reorder(strategies, c(rep(1, nrow(df1)), rep(2, nrow(df1)), rep(3, nrow(df1))))) %>%  # Redefine legend order
  mutate(M1 = c(rep(df1$M[order(df1$M, decreasing = T)], 3)))

ggplot(p1, aes(x = M1, y = value, color = strategies)) +
  geom_line() +
  scale_x_continuous(breaks = seq(min(df1$M), max(df1$M), 10), #<-----
                     labels = as.character(seq(max(df1$M), min(df1$M), -10))) +
  scale_color_discrete(labels = c("involution", "sit-up", "lay-down")) +
  ylab("Equilibrium Value") +
  xlab("M") +
  theme_few() +
  theme(legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.text = element_text(size = 16),
        legend.position = c(0.6, 0.9) #<-----
  )

ggsave("Exp1-2-beta.png", width = 20, height = 10, units = "cm", dpi = 300)


# Only one point, cannot be plotted
# Visualization of the 3rd plot xC & xL
df1 <- df %>% 
  subset(M >= 35) %>% 
  subset(xDstar == 0)

p1 <- df1 %>% 
  select(-n) %>% 
  gather("strategies", "value", -M) %>% 
  mutate(strategies = fct_reorder(strategies, c(rep(1, nrow(df1)), rep(2, nrow(df1)), rep(3, nrow(df1))))) %>%  # Redefine legend order
  mutate(M1 = c(rep(df1$M[order(df1$M, decreasing = T)], 3)))

ggplot(p1, aes(x = M1, y = value, color = strategies)) +
  geom_point() +
  scale_x_continuous(breaks = seq(min(df1$M), max(df1$M), 1), #<-----
                     labels = as.character(seq(max(df1$M), min(df1$M), -1))) +
  scale_color_discrete(labels = c("involution", "sit-up", "lay-down")) +
  ylim(0, 1) +
  ylab("Equilibrium Value") +
  xlab("M") +
  theme_few() +
  theme(legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.text = element_text(size = 16),
        legend.position = c(0.6, 0.9) #<-----
  )


ggsave("Exp1-3-beta.png", width = 20, height = 10, units = "cm", dpi = 300)











