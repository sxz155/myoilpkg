rolling_sd <- function(input, win = 2) {
  time_len <- length(input)
  roll_sd <- rep(NA, time_len)
  roll_sd[win: time_len] <- rollapply(input, win, sd)
  return(roll_sd)
}

rolling_mean <- function(input, win = 2) {
  time_len <- length(input)
  roll_mean <- rep(NA, time_len)
  roll_mean[win: time_len] <- rollmean(input, win)
  return(roll_mean)
}

lag_fun <- function(input, order_lag = 1) {
  time_len <- length(input)
  lag_order <- rep(NA, time_len)
  lag_order[(order_lag + 1): time_len] <- input[1: (time_len - order_lag)]
  return(lag_order)
}

lead_fun <- function(input, order_lead = 1) {
  time_len <- length(input)
  lead_order <- rep(NA, time_len)
  lead_order[1: (time_len - order_lead)] <- input[(order_lead + 1): time_len]
  return(lead_order)
}

diff_fun <- function(input) {
  lag_1 <- lag_fun(input)
  diff_1 <- input - lag_1
  return(diff_1)
}

spread_fun <- function(input_1, input_2) {
  spread_12 <- input_1 - input_2
  return(spread_12)
}

ratio_fun <- function(input_1, input_2) {
  ratio_12 <- input_1 / input_2
  return(ratio_12)
}

prod_fun <- function(input_1, input_2) {
  prod_12 <- input_1 * input_2
  return(prod_12)
}
