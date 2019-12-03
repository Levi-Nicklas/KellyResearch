library(CVXR)
library(tidyverse)
library(reshape2)
library(gganimate)


# Initial Bankroll
w0 = 100

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

# Reformat to Matrix
P <- matrix(P, ncol = 5, byrow = TRUE)

#P <- rnorm(100, mean = 1, sd = 0.2)
#P <- matrix(P, ncol = 5, byrow = TRUE)

# Save Matrix Dimensions
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
result$value
result$getValue(b)

n_sim <- 50
simulations <- matrix(rep(0,10*n_sim), nrow = 10)

for(i in 1:n_sim){
  bets <- result$getValue(b)
  idx <- sample.int(10, size = 10, prob =  ps, replace = TRUE)
  winnings <- P[idx,] %*% bets
  wealth <- w0 * cumprod(winnings)
  
  simulations[,i] <- wealth
}


dat1 <- simulations %>% 
  melt() %>% 
  group_by(Var2) 


plt1 <- dat1 %>%
  mutate(var2proxy = Var2) %>% 
  ggplot(aes(Var1, value, group = var2proxy)) +
  geom_line(color = "purple" , alpha = 0.5) +
  geom_hline(yintercept = 100, color = "black", size = 1.15, linetype = "dashed") +
  geom_vline(xintercept = 1,  color = "black", size = 1.15, linetype = "dashed")+
  labs(title = "50 Simulations of 10 Rounds of Game",
       x = "Round",
       y = "Wealth ($)") +
  theme_minimal() +
  geom_point(color = "purple") +
  transition_reveal(along = Var1) +
  ease_aes('linear')

animate(plt1)

