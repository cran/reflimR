adjust_digits <- function(x){
  if(length(x) > 1 | !is.numeric(x)){stop("(adjust_digits) x must be a single number.")}
  if (x == 0){return(list(x.round = 0, digits = 0))}
  xx <- abs(x)
  digits <- 2 - floor(log10(xx))
  if(digits < 0){digits <- 0}
  return(list(x.round = round(x, digits), digits = digits))
}
