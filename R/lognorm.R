lognorm <- function(x, cutoff = 0.05, alpha = 0.25, digits = 3,
                    plot.it = TRUE, plot.logtype = TRUE,
                    main = "Bowley skewness", xlab = "x"){
  xx <- x[!is.na(x)]

  if(!is.numeric(xx)){stop("x must be numeric.")}
  if(length(xx) < 2){stop("x must be a vector of at least 2 numeric values.")}
  if(min(xx) <= 0){stop("Negative values not allowed.")}

  bs <- rep(NA, 2)
  bs[1] <- bowley(xx, alpha)
  bs[2] <- bowley(log(xx), alpha)
  if(bs[1] < 0){lognormal <- FALSE} else {lognormal <- (bs[1] - bs[2]) >= cutoff}
  if(!is.na(digits)){bs <- round(bs, digits)}

  if(plot.it){
    xxx <- xx[xx < median(xx) + 6 * IQR(xx)]
    df.x <- data.frame(lin = xxx, log = log(xxx))
    df.quant <- cbind(lin = quantile(df.x[, 1], c(0.1, 0.5)), log = quantile(df.x[, 2], c(0.1, 0.5)))
    slope <- (df.quant[2, 1] - df.quant[1, 1])/(df.quant[2, 2] - df.quant[1, 2])
    intercept <- df.quant[1, 1] - slope * df.quant[1, 2]
    df.x <- cbind(df.x, transformed = intercept + slope * df.x[, 2])
    d1 <- density(df.x[, 1])
    d2 <- density(df.x[, 3])
    ymax <- max(max(d1$y), max(d2$y)) * 1.4
    plot(d1$x, d1$y, main = main, xlab = xlab, ylab = "",
         type = "l", lwd = 2, yaxt = "n",
         xlim = c(0.9 * min(df.x[, 1]), max(max(d1$x), max(d2$x))),
         ylim = c(0, ymax))
    lines(d2$x, d2$y, col = "blue", lwd = 2)
    base.scale <- c(1, 1.5, 2, 3, 5, 7)
    num.scale <- c(base.scale * 10^-1,
                   base.scale * 10^0,
                   base.scale * 10^1,
                   base.scale * 10^2,
                   base.scale * 10^3,
                   base.scale * 10^4)
    log.scale <- intercept + slope * log(num.scale)
    axis(3, at = log.scale, labels = as.character(num.scale), col.axis = "blue",
         mgp = c(3, 0.1, 0), tcl = -0.1)
    boxplot(df.x[, 1], horizontal = TRUE, at = 0.75 * ymax, boxwex = ymax / 20,
            col = "lightgrey", pch = 20, add = TRUE)
    boxplot(df.x[, 3], horizontal = TRUE, at = 0.85 * ymax, boxwex = ymax / 20,
            col = "blue", pch = 20, add = TRUE)
    text(median(df.x[, 1]), 0, paste("delta =", bs[1] - bs[2]), pos = 3)
    if (plot.logtype){
      tx <- ifelse(lognormal, "lognormal distribution", "normal distribution")
      text(0.9 * min(df.x[, 1]), 0.95 * ymax, tx, pos = 4)
    }
  }
  return(list("lognormal" = lognormal,
              "BowleySkewness" = c(normal = bs[1], lognormal = bs[2], delta = bs[1] - bs[2])))
}
