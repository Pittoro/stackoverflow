# link to stackoverflow page: https://stackoverflow.com/questions/65303912/infinite-loop-in-coin-flipping-game/65332005#65332005

rm(list=ls())
library(ggplot2)

# simulation function
coin_game <- function(n_runs, n_games, bias = 0.5){
  game_payoff <- c()
  expected_value_record <- c()
  
  for (j in seq_len(n_runs)) {
    
    for (i in seq_len(n_games)) {
      
      cost <- 0
      flip_record <- c()
      payoff <- c()
      
      repeat{
        cost <- cost + 1
        flip <- rbinom(1, 1, prob = bias)
        flip_record <- c(flip_record, flip)
        # print(flip_record)
        
        n_tails <- length(flip_record) - sum(flip_record) # number of 0s/tails
        n_heads <- sum(flip_record) # number of 1s/heads
        
        if (abs(n_tails - n_heads) == 4) {
          game_payoff <- c(game_payoff, 10 - cost) # record game payoff
          # print(paste0("single game payoff: ", 10 - cost))
          break
        }
      }
    }
    expected_value_record <- c(expected_value_record, mean(game_payoff))
    game_payoff <- c()
  }
  return(expected_value_record)
}

# run coin_game() on a vector of probabilities - introduce bias to find fair game conditions
n_runs = 1
n_games = 1000
expected_value_record <- sapply(seq(0.01, 0.99, by = 0.01), coin_game, n_runs = n_runs, n_games = n_games)

# plot expected value
expected_value_record <- cbind.data.frame("run" = seq_len(length(expected_value_record)), "bias" = c(seq(0.01, 0.99, by = 0.01)), expected_value_record)

ggplot(data = expected_value_record) +
  geom_line(aes(x = bias, y = expected_value_record)) +
  # geom_line(aes(x = run, y = bias)) +
  scale_x_continuous(breaks = c(seq(min(expected_value_record$bias), max(expected_value_record$bias), by = 0.1), max(expected_value_record$bias))) +
  labs(
    title = "Coin flip experiment: expected value for each probability level", 
    caption = paste0("Number of runs per 'bias': ", n_runs, ". ", "Number of games in each run: ", n_games, "."), 
    x = "Run", 
    y = "Expected value") +
  geom_hline(yintercept = 0, size = 1.4, color = "red") +
  geom_text(aes(x = 0.1, y = 0, label = "Fair game", hjust = 1, vjust = -1), size = 4, color = "red") +
  # annotate(
  #   geom = "text", 
  #   x = 0.85 * n_runs, 
  #   y = max(expected_value_record$expected_value_record), 
  #   label = paste0("Mean across runs: ", mean(expected_value_record$expected_value_record))) +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))
