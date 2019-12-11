Time <- 10

df_mat <- matrix(, ncol = 3, nrow = Time)
count <- 0
b0 <- 0.9

for(i in 1:Time){
  count <- count + 1
  
  if(i == 1){
    
    df_mat[count, 1] <- i
    # utility
    df_mat[count, 2] <- 0.4
    
  }else if(i < 4){
    
    df_mat[count, 1] <- i
    # utility
    df_mat[count, 2] <- b0*df_mat[count - 1,  2] + rnorm(1, 0, 0.15)
    
  }else if(i == 5){
    
    df_mat[count, 1] <- i
    # utility
    df_mat[count, 2] <- b0*df_mat[count - 1,  2] - 1.2
    
  }
  
  
    else {
    
    df_mat[count, 1] <- i
    df_mat[count, 2] <- b0*df_mat[count - 1, 2] + rnorm(1, 0, 0.05)
    
  }
  
  
  
}

data_f <- data.frame(df_mat)
names(data_f) <- c('Time', 'Utility')

library(ggplot2)

ggplot(data_f, aes(x = Time, y = Utility)) + 
  geom_point() + 
  geom_line()
