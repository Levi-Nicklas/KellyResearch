
library(CVXR)
library(tidyverse)
library(reshape2)
library(gganimate)



functional_kelly <-  function(kelly_amount, cap, time_limit, current_budget, expected_value, current_time){
 
  
  
  
  
  time_function <- (((cap/current_budget)^(1/(time_limit-current_time))) - 1)/ expected_value
  cap_function <- (cap/current_budget) - 1
  print(paste0(c(kelly_amount, cap, time_limit, current_budget, expected_value, current_time, cap_function, time_function)))

  
  chosen <- min(kelly_amount, time_function, cap_function)
  #print(paste0(c(kelly_amount, time_function, cap_function)))
  return(chosen)
  
}

kelly_function <- function(P){
  P_dim <- dim(P)
  # Probabilities
  ps <- rep(1, P_dim[1]) / P_dim[1]
  # Proportion of Wealth Bet
  b <- CVXR::Variable(P_dim[2])
  
  constr <- list(b >= 0, sum(b) == 1)
  
  prob <- CVXR::Problem(
    CVXR::Maximize(sum(t(ps) %*% log(P %*% b))),
    constr)
  result <- solve(prob)
  bets <- result$getValue(b)
  return(list(result= result,ps= ps, bets=bets))
}


run_sim <- function(starting_amount=100, row_number=10, column_number=5, n_sim=50, std=0.2, winning_cap=300, time_cap=10, f_kelly=F){
  

# Initial Bankroll
w0 = starting_amount


# Input data from CVXR Example.

P <- c(3.5000, 1.1100, 1.1100, 1.0400, 1.0100,
       0.5000, 0.9700, 0.9800, 1.0500, 1.0100,
       0.5000, 0.9900, 0.9900, 0.9900, 1.0100,
       0.5000, 1.0500, 1.0600, 0.9900, 1.0100,
       0.5000, 1.1600, 0.9900, 1.0700, 1.0100,
       0.5000, 0.9900, 0.9900, 1.0600, 1.0100,
       0.5000, 0.9200, 1.0800, 0.9900, 1.0100,
       0.5000, 1.1300, 1.1000, 0.9900, 1.0100,
       0.5000, 0.9300, 0.9500, 1.0400, 1.0100,
       3.5000, 0.9900, 0.9700, 0.9800, 1.0100)

P <- rnorm(row_number*column_number, 1, std)


# Reformat to Matrix
P <- matrix(P, ncol = column_number, nrow = row_number, byrow = TRUE)


result_vect <- kelly_function(P)


result <- result_vect$result
ps <- result_vect$ps
bets <- result_vect$bets

simulations <- matrix(rep(0,row_number*n_sim), nrow = row_number)

set.seed(100)


for(i in 1:n_sim){
  
  idx <- sample.int(row_number, size = row_number, prob =  ps, replace = TRUE)
  
  if(f_kelly){
    count <- 1
    wealth <- list()
    for(index in idx){
      
      # expected_value <-  P[index,] - 1
      # winning_prob <- sum(expected_value>0)/length(P[index,])
      # losing_prob <- sum(expected_value<0)/length(P[index,])
      # winning_amount <- abs(sum(expected_value[expected_value>0]))
      # losing_amount <- abs(sum(expected_value[expected_value<0]))
      
      final_expected_value <- ((winning_amount*winning_prob) - (losing_amount*losing_prob))
      
      wealth[[count]] <- w0*functional_kelly(result$value, winning_cap, time_cap, w0, final_expected_value,count)
      w0 <- wealth[[count]]
      count <- count + 1
    }
    wealth <- unlist(wealth)
    
  }else{
    winnings <- P[idx,] %*% bets #rowxcolumn, columnx1
    wealth <- starting_amount * cumprod(winnings) #row X 1
    
  }
  
  simulations[,i] <- wealth
}

  return(simulations)

}

show_plot <- function(simulations, starting_amount=1, n_sim=1, row_number=1){
  simulations <- simulations[1:row_number,1:n_sim]
  
  
  plot_var <- simulations %>% 
    melt() %>% 
    group_by(Var2) %>% 
    ggplot(aes(Var1, value, group = Var2)) +
    geom_line(color = "purple" , alpha = 0.5) +
    geom_hline(yintercept = starting_amount, color = "black", size = 1.15, linetype = "dashed") +
    geom_vline(xintercept = 1,  color = "black", size = 1.15, linetype = "dashed")+
    labs(title = paste0(n_sim," Simulations of ", row_number ," Rounds of Game"),
         x = "Round",
         y = "Wealth ($)") +
    theme_minimal()
  
  
  return(plot_var)
  
}


show_plot_new <- function(simulations, starting_amount=1, n_sim=1, row_number=1){
  simulations <- simulations[1:row_number,1:n_sim]
  
  

  gg_result <- simulations[row_number,] %>% 
    as.data.frame() %>% 
    ggplot(aes(.)) + 
    #geom_histogram(color = "white", fill = "purple", alpha = 0.75) +
    geom_density(color = "#4e0f71", size = 1.1, fill = "#d6a0f3")+
    ggtitle("End Wealth Result")+
    xlab("Dollars") + 
    coord_flip() +
    theme_minimal()
  
  return(gg_result)
  
  
}

#column is the number of simulations, and each role is the round


my_sim <- run_sim(n_sim=500, starting_amount =100, row_number=50, std=0.2, f_kelly=T)


show_plot(my_sim)
