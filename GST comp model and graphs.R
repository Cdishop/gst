

# . -----------------------------------------------------------------------


# . -----------------------------------------------------------------------


# . -----------------------------------------------------------------------


# The Comp Model ----------------------------------------------------------

set.seed(3)


experience <- function(){
  
  value <- rnorm(1,0,1)
  return(value)
  
}

time <- 20
b0 <- 0.3
b1 <- 0.3
utility_a_start <- 0.2
utility_b_start <- 0.4


df_matrix <- matrix(, ncol = 8, nrow = time)
counter <- 0

for(i in 1:time){
  counter <- counter + 1
  
  
  
  
  
  
  # first time point --------------------------------------------------------
  
  
  if(i == 1){
    
    # time
    df_matrix[counter, 1] <- i
    # utility for goal A
    df_matrix[counter, 2] <- utility_a_start
    # utility for goal B
    df_matrix[counter, 3] <- utility_b_start
    # likelihood for goal A
    df_matrix[counter, 4] <- 1 / 1 + exp(df_matrix[counter, 2])
    likelihood_a <- df_matrix[counter, 4]
    # likelihood for goal B
    df_matrix[counter, 5] <- 1 / 1 + exp(df_matrix[counter, 3])
    likelihood_b <- df_matrix[counter, 5]
    # Goal choice
    choice <- NULL
    
    if(likelihood_a > likelihood_b){
      choice <- 1
    }else{
      choice <- 2
    }
    
    df_matrix[counter, 6] <- choice
    
    
    # Experience goal A if they choose A
    experience_a_b <- c(0,0)
    
    if(choice == 1){
      experience_a_b[1] <- experience()
    }else{
      experience_a_b[2] <- experience()
    }
    
    # experience for goal A
    df_matrix[counter, 7] <- experience_a_b[1]
    # experience for goal B
    df_matrix[counter, 8] <- experience_a_b[2]
    
    
    
    
    
    
    
    
    # other time points -------------------------------------------------------
    
    
  }else{
    
    
    
    # time
    df_matrix[counter, 1] <- i
    # utility for goal A
    df_matrix[counter, 2] <- b0*df_matrix[counter - 1, 2] + b1*df_matrix[counter - 1, 7]
    # utility for goal B
    df_matrix[counter, 3] <- b0*df_matrix[counter - 1, 3] + b1*df_matrix[counter - 1, 8]
    # likelihood for goal A
    df_matrix[counter, 4] <- 1 / 1 + exp(df_matrix[counter, 2])
    likelihood_a <- df_matrix[counter, 4]
    # likelihood for goal B
    df_matrix[counter, 5] <- 1 / 1 + exp(df_matrix[counter, 3])
    likelihood_b <- df_matrix[counter, 5]
    # Goal choice
    choice <- NULL
    
    if(likelihood_a > likelihood_b){
      choice <- 1
    }else{
      choice <- 2
    }
    
    df_matrix[counter, 6] <- choice
    
    
    # Experience goal A if they choose A
    experience_a_b <- c(0,0)
    
    if(choice == 1){
      experience_a_b[1] <- experience()
    }else{
      experience_a_b[2] <- experience()
    }
    
    # experience for goal A
    df_matrix[counter, 7] <- experience_a_b[1]
    # experience for goal B
    df_matrix[counter, 8] <- experience_a_b[2]
    
    
    
  }
  
  
  
}



# ... ---------------------------------------------------------------------


# ... ---------------------------------------------------------------------


# ... ---------------------------------------------------------------------


# Graphs ------------------------------------------------------------------



library(ggplot2)
library(tidyverse)
library(stringr)
library(stringi)
library(gridExtra)

df <- data.frame(df_matrix)
names(df) <- c('Time', 'Utility_a', 'Utility_b', 'Likelihood_a', 'Likelihood_b', 'Choice', 'Experience_a', 'Experience_b')

df_long <- df %>%
  gather(Utility_a, Utility_b, Likelihood_a, Likelihood_b, Experience_a, Experience_b, key = 'goal_initial', value = 'Utility') %>%
  mutate(Goal = 
           ifelse(goal_initial == 'Utility_a', 'A', 
                  ifelse(goal_initial == 'Utility_b', 'B', 
                         ifelse(goal_initial == 'Likelihood_a', 'A',
                                ifelse(goal_initial == 'Likelihood_b', 'B',
                                       ifelse(goal_initial == 'Experience_a', 'A', 'B'))))), 
  )

df_long$goal_initial <- str_replace(df_long$goal_initial, '_a', '')
df_long$goal_initial <- str_replace(df_long$goal_initial, '_b', '')

df_long <- df_long %>%
  spread(key = goal_initial, value = Utility)

df_long$Choice <- ifelse(df_long$Choice == 1, 'A', 'B')

df_long$Choice <- as.character(df_long$Choice)
df_long$Goal <- as.character(df_long$Goal)


df_text <- df_long %>%
  select(Time, Choice)

use_these <- seq(from = 1, to = 40, by = 2)

df_text <- df_text[use_these, ]
df_text$Utility <- c(rep(0.65, 20))
df_text$Goal <- df_text$Choice

# ... ---------------------------------------------------------------------


# ... ---------------------------------------------------------------------


# ... ---------------------------------------------------------------------


# Goal choice over time above utility ------------------------------------------------


plot1 <- ggplot(df_long, aes(x = Time, y = Utility, linetype = Goal, shape = Goal)) + 
  geom_point() + 
  geom_line() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_text(data = df_text, aes(label = Choice, fontface = 'bold'))


plot1



# ... ---------------------------------------------------------------------


# ... ---------------------------------------------------------------------


# ... ---------------------------------------------------------------------


# Goal choice over time above utility and experiences ---------------------



# Tidy --------------------------------------------------------------------



df_long_long <- df %>%
  gather(Utility_b, Utility_a, Likelihood_a, Likelihood_b, Experience_a, Experience_b, key = 'variable', value = 'Value') %>%
  mutate(Goal = 
           ifelse(variable == 'Utility_b', 'B',
                  ifelse(variable == 'Likelihood_b', 'B', 
                         ifelse(variable == 'Experience_b', 'B',
                                ifelse(variable == 'Utility_a', 'A',
                                       ifelse(variable == 'Likelihood_a', 'A', 'A'))))))

df_long_long$variable <- str_replace(df_long_long$variable, '_b', '')
df_long_long$variable <- str_replace(df_long_long$variable, '_a', '')


df_wo_l <- df_long_long %>%
  filter(variable != 'Likelihood')

df_utility_a <- df_wo_l %>%
  filter(variable == 'Utility' & Goal == 'A')

df_exp_a <- df_wo_l %>%
  filter(variable == 'Experience' & Goal == 'A')

df_utility_b <- df_wo_l %>%
  filter(variable == 'Utility' & Goal == 'B')

df_exp_b <- df_wo_l %>%
  filter(variable == 'Experience' & Goal == 'B')


# Plots -------------------------------------------------------------------



ue_a <- ggplot() + 
  geom_point(data = df_utility_a, aes(x = Time, y = Value)) + 
  geom_line(data = df_utility_a, aes(x = Time, y = Value)) + 
  geom_bar(data = df_exp_a, stat = 'identity', aes(x = Time, y = Value), fill = 'gray85', color = 'black', alpha = 0.2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line.y = element_line(colour = "black",),
        axis.line.x = element_blank(),
        axis.text.x = element_blank()) +
  scale_x_continuous(breaks = NULL) + 
  labs(title = 'Goal A') +
  xlab(NULL) +
  annotate('text', x = 1, y = 1.5, label = 'Experiences = ') + 
  annotate('rect', xmin = 2.5, xmax = 3.5, ymin = 1.4, ymax = 1.6, alpha = 0.2, color = 'black', fill = 'gray85') + 
  annotate('text', x = 1, y = 1, label = 'Utility = ') + 
  annotate('segment', x = 2.5, xend = 3.5, y = 1, yend = 1, color = 'black') +
  annotate('pointrange', x = 2.5, y = 1, ymin = 1, ymax = 1, color = 'black', size = 0.2) +
  annotate('pointrange', x = 3.5, y = 1, ymin = 1, ymax = 1, color = 'black', size = 0.2)



ue_b <- ggplot() + 
  geom_point(data = df_utility_b, aes(x = Time, y = Value)) + 
  geom_line(data = df_utility_b, aes(x = Time, y = Value)) + 
  geom_bar(data = df_exp_b, stat = 'identity', aes(x = Time, y = Value), fill = 'gray85', color = 'black', alpha = 0.2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = 'Goal B') 


grid.arrange(ue_a, ue_b)
