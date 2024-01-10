permissible_uncertainty <- function(lower.limit, upper.limit, apply.rounding = TRUE){
  if(lower.limit <= 0 | upper.limit <= 0 | lower.limit >= upper.limit ){
    stop("Please check lower and upper limits: only positive values allowed.")}

  g <- sqrt(lower.limit * upper.limit)
  slog<-(log(upper.limit) - log(lower.limit)) / 3.92
  CV.E <- 100 * (sqrt(exp(slog ^ 2) - 1))
  f <- sqrt(CV.E - 0.25)
  pU1 <- (f * (1.024 * lower.limit + 0.256 * g)) / 100
  pU2 <- (f * (1.024 * upper.limit + 0.256 * g)) / 100

  result <- rep(0, 4)
  names(result) <- c("lower.lim.low", "lower.lim.upp", "upper.lim.low", "upper.lim.upp")
  result[1] <- lower.limit - pU1
  result[2] <- lower.limit + pU1
  result[3] <- upper.limit - pU2
  result[4] <- upper.limit + pU2
  if(apply.rounding){
    digits <- adjust_digits(result[1])$digits
    result = round(result, digits)
  }
  return(result)
}
