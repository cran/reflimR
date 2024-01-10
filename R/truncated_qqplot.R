truncated_qqplot <- function(x.trunc, lognormal = NULL, perc.trunc = 2.5, n.min = 200,
                             apply.rounding = TRUE, plot.it = TRUE,
                             main = "Q-Q plot", xlab = "theoretical quantiles", ylab = "sample quantiles"){

  x.trunc <- na.omit(x.trunc)
  if(!is.numeric(x.trunc)){stop("x.trunc must be numeric.")}
  if(min(x.trunc) <= 0){stop("Only positive values allowed.")}

  n <- length(x.trunc)
  if(n < 40){stop(paste0("n = ", n, ". The absolute minimum for reference limit estimation is 40."))}
  if(n < n.min){
    warning(paste("(truncated_qqplot) n =", n, "where a minimum of", n.min, "is required. You may try to reduce n.min at the loss of accuracy."))
    return(list(result = NULL, lognormal = NULL))
  }
  n.quantiles <- 100
  if(n < n.quantiles){n.quantiles <- n}

  digits <- adjust_digits(median(x.trunc))$digits
  if(is.null(lognormal)){lognormal <- lognorm(x.trunc, plot.it = FALSE)$lognormal}
  if(lognormal){x.trunc <- log(x.trunc)}

  p1 <- seq(from = perc.trunc/100, to = 1-perc.trunc/100, length.out = n.quantiles)
  p2 <- seq(from = 0, to = 1, length.out = n.quantiles)
  x.ax <- qnorm(p1)
  y.ax <-quantile(x.trunc, p2)

  central.part <- floor(0.05 * n.quantiles) : ceiling(0.95 * n.quantiles)
  reg <- lm(y.ax[central.part] ~ x.ax[central.part])
  a <- reg$coefficients[2]
  b <- reg$coefficients[1]
  result <- c(b, a, b - 1.96 * a, b + 1.96 * a)
  result <- setNames(result, c("mean", "sd", "lower.lim", "upper.lim"))
  if(lognormal){
    names(result)[1 : 2] <- paste0(names(result)[1 : 2], "log")
    result[1 : 2] <- round(result[1 : 2], 3)
    result[3 : 4] <- exp(result[3 : 4])
  }
  if(result[3] < 0){result[3] <- 0}
  if(apply.rounding){result[3 : 4] <- round(result[3 : 4], digits)}

  if (plot.it){
    if(!lognormal){
      ll <- result[3]
      ul <- result[4]
      diff <- ul - ll
      plot(y.ax ~ x.ax, pch = 20, col = "blue",
           xlim = c(-3, 3),
           ylim = c(ll - 0.2 * diff, ul + 0.2 * diff),
           main = main, xlab = xlab, ylab = ylab)
    }else{
      ll <- log(result[3])
      ul <- log(result[4])
      diff <- ul - ll
      plot(y.ax ~ x.ax, yaxt = "n", xlim = c(-3, 3),
           ylim = c(ll - 0.2 * diff, ul + 0.2 * diff),
           main = main, xlab = xlab, ylab = ylab)
      y.pos <- c(50, 100, 150, 200, 300, 400, 500, 1000, 1500)/(10 ^ digits)
      axis(2, at = log(y.pos),labels  = y.pos)
    }
    abline(v = 0)
    abline(v = c(-1.96, 1.96), lty = 2)
    abline(h = c(ll, ul),
           col = "green", lwd = 2)
    abline(reg$coefficients, lwd = 2, col = "blue")
    points(c(-1.96, 1.96), c(ll, ul), pch = 19, col = "green")
    text(-2, ll, formatC(result[3], digits, format = "f"), pos = 1)
    text(2, ul, formatC(result[4], digits, format = "f"), pos = 3)
  }
  return(list(result = result, lognormal = lognormal))
}
