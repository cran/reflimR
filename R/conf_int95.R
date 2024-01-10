conf_int95 <- function(n, lower.limit, upper.limit, lognormal = TRUE, apply.rounding = TRUE){
  if(upper.limit <= lower.limit){stop("upper.limit must be higher than lower.limit.")}
  if(lognormal){
    lower.limit <- log(lower.limit)
    upper.limit <- log(upper.limit)
  }
  sigma <- (upper.limit - lower.limit) / 3.92
  result <- rep(0, 5)
  names(result) <- c("lower.lim.low", "lower.lim.upp", "upper.lim.low", "upper.lim.upp", "n")

  diff.outer <- sigma * 5.81 / (sqrt(n) + 0.66)
  diff.inner <- sigma * 7.26 / (sqrt(n) - 5.58)
  result[1] <- lower.limit - diff.outer
  result[2] <- lower.limit + diff.inner
  result[3] <- upper.limit - diff.inner
  result[4] <- upper.limit + diff.outer

  if (lognormal){result <- exp(result)}
  if(apply.rounding){
    digits <- adjust_digits(result[1])$digits
    result = round(result, digits)
  }
  result[5] <- n
  return(result)
}
