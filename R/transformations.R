#' Rolling sd function
#'
#' @param input A vector to find rolling sd.
#' @param win A double as rolling window.
#' @return Calculated rolling sd column.
#' @description Find rolling sd for the provided input and window.
rolling_sd <- function(input, win = 2) {
  time_len <- length(input)
  roll_sd <- rep(NA, time_len)
  roll_sd[win: time_len] <- rollapply(input, win, sd)
  return(roll_sd)
}

#' Rolling mean function
#'
#' @param input A vector to find rolling mean.
#' @param win A double as rolling window.
#' @return Calculated rolling mean column.
#' @description Find rolling mean for the provided input and window.
rolling_mean <- function(input, win = 2) {
  time_len <- length(input)
  roll_mean <- rep(NA, time_len)
  roll_mean[win: time_len] <- rollmean(input, win)
  return(roll_mean)
}

#' Lagging function
#'
#' @param input A vector to find lag.
#' @param order_lag A double as lag order.
#' @return Calculated lag vector.
#' @description Find lag of \code{order_lag} for the provided input vector.
lag_fun <- function(input, order_lag = 1) {
  time_len <- length(input)
  lag_order <- rep(NA, time_len)
  lag_order[(order_lag + 1): time_len] <- input[1: (time_len - order_lag)]
  return(lag_order)
}

#' Leading function
#'
#' @param input A vector to find lead.
#' @param order_lead A double as lead order.
#' @return Calculated lead vector.
#' @description Find lead of \code{order_lead} for the provided input vector.
lead_fun <- function(input, order_lead = 1) {
  time_len <- length(input)
  lead_order <- rep(NA, time_len)
  lead_order[1: (time_len - order_lead)] <- input[(order_lead + 1): time_len]
  return(lead_order)
}

#' Difference function
#'
#' @param input A vector to find difference.
#' @return Calculated difference vector.
#' @description Find difference for the provided input vector.
diff_fun <- function(input) {
  lag_1 <- lag_fun(input)
  diff_1 <- input - lag_1
  return(diff_1)
}

#' Spread function
#'
#' @param input_1 A vector as the first input.
#' @param input_2 A vector as the second input.
#' @return Calculated spread vector.
#' @description Find spread between the provided input vectors.
spread_fun <- function(input_1, input_2) {
  spread_12 <- input_1 - input_2
  return(spread_12)
}

#' Ratio function
#'
#' @param input_1 A vector as the first input.
#' @param input_2 A vector as the second input.
#' @return Calculated ratio vector.
#' @description Find ratio between the provided input vectors.
ratio_fun <- function(input_1, input_2) {
  ratio_12 <- input_1 / input_2
  return(ratio_12)
}

#' Product function
#'
#' @param input_1 A vector as the first input.
#' @param input_2 A vector as the second input.
#' @return Calculated product vector.
#' @description Find product of the provided input vectors.
prod_fun <- function(input_1, input_2) {
  prod_12 <- input_1 * input_2
  return(prod_12)
}
