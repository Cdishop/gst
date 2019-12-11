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


library(ggplot2)
library(tidyverse)
library(stringr)
library(stringi)

df <- data.frame(df_matrix)
names(df) <- c('Time', 'Utility_a', 'Utility_b', 'Likelihood_a', 'Likelihood_b', 'Choice', 'Experience_a', 'Experience_b')


# Utility over time -------------------------------------------------------

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


ggplot(df_long, aes(x = Time, y = Utility, linetype = Goal, shape = Goal)) + 
  geom_point() + 
  geom_line() + 
  theme_bw()




# Goal choice over time ---------------------------------------------------

ggplot(df_long, aes(x = Time, y = Choice, shape = Choice)) + 
  geom_point(size = 3) + 
  ylab('') +
  theme_bw()



# Utility follows experience across time for b -----------------------------------------

df_long_long <- df %>%
  gather(utility_b, likelihood_b, experience_b, key = 'variable', value = 'Value') %>%
  mutate(goal = 
                  ifelse(variable == 'utility_b', 'b',
                                ifelse(variable == 'likelihood_b', 'b', 'b')))

df_long_long$variable <- str_replace(df_long_long$variable, '_b', '')

ggplot(df_long_long, aes(x = time, y = Value, color = variable)) + 
  geom_line()
  geom_point()

df_wo_l <- df_long_long %>%
  filter(variable != 'Likelihood')

df_utility <- df_wo_l %>%
  filter(variable == 'Utility')

df_exp <- df_wo_l %>%
  filter(variable == 'Experience')


ggplot() + 
  geom_point(data = df_utility, aes(x = time, y = Value)) + 
  geom_line(data = df_utility, aes(x = time, y = Value)) + 
  geom_bar(data = df_exp, stat = 'identity', aes(x = time, y = value), fill = 'gray85', color = 'black')


ggplot() + 
  geom_point(data = df_utility, aes(x = time, y = Value)) + 
  geom_line(data = df_utility, aes(x = time, y = Value)) + 
  geom_point(data = df_exp, aes(x = time, y = Value), color = 'red')


