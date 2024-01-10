bowley <- function(x, alpha = 0.25){
  q <- quantile(x, c(alpha, 0.5, 1 - alpha))
  bs <- as.numeric((q[1] - 2 * q[2] + q[3]) / (q[3] - q[1]))
  return (bs)
}
