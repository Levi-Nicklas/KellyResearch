
library(CVXR)
library(tidyverse)
library(reshape2)
library(gganimate)

run_sim <- function(starting_amount=100, row_number=10, column_number=5, n_sim=50, std=0.2){
  

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




simulations <- matrix(rep(0,row_number*n_sim), nrow = row_number)
set.seed(100)


for(i in 1:n_sim){
  bets <- result$getValue(b)
  idx <- sample.int(row_number, size = row_number, prob =  ps, replace = TRUE)
  winnings <- P[idx,] %*% bets
  wealth <- w0 * cumprod(winnings)
  
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
  
  
  # dat1 <- simulations %>% 
  #   melt() %>% 
  #   group_by(Var2) 
  
  
  # plt1 <- dat1 %>%
  #   mutate(var2proxy = Var2) %>% 
  #   ggplot(aes(Var1, value, group = var2proxy)) +
  #   geom_line(color = "purple" , alpha = 0.5) +
  #   geom_hline(yintercept = 100, color = "black", size = 1.15, linetype = "dashed") +
  #   geom_vline(xintercept = 1,  color = "black", size = 1.15, linetype = "dashed")+
  #   labs(title = "50 Simulations of 10 Rounds of Game",
  #        x = "Round",
  #        y = "Wealth ($)") +
  #   theme_minimal() +
  #   geom_point(color = "purple") +
  #   transition_reveal(along = Var1) +
  #   ease_aes('linear')
  # 
  #plot_value <- animate(plt1)
  #anim_save(plot_value, "out.gif")

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
