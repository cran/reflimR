iboxplot <- function(x, lognormal = NULL, perc.trunc = 2.5, apply.rounding = TRUE,
                     plot.it = TRUE, main = "iBoxplot", xlab = "x"){
  xx <- na.omit(x)
  if (!is.numeric(xx)) {
    stop("x must be numeric.")
  }
  if (min(xx) <= 0) {
    stop("x must be a vector of positive numbers.")
  }
  digits <- adjust_digits(median(xx))$digits
  n <- length(xx)
  if (n < 40) {
    stop(paste0("n = ", n, ". The absolute minimum for reference limit estimation is 40."))
  }
  progress <- data.frame(n = n, min = min(xx), max = max(xx))
  if (is.null(lognormal)) {
    lognormal <- lognorm(xx, plot.it = FALSE)$lognormal
  }
  if (lognormal) {
    xx <- log(xx)
  }
  if (is.null(perc.trunc) || perc.trunc <= 0 || perc.trunc > 25) {
    stop("perc.trunc must not be NULL, negative, zero, or greater than 25.")
  }

  q.trunc <- perc.trunc / 100
  truncate.x <- function(x, qf) {
    Q <- quantile(x, c(0.25, 0.5, 0.75))
    var1 <- Q[2] - Q[1]
    var2 <- Q[3] - Q[2]
    var <- min(var1, var2)
    lim <- c(Q[2] - qf * var, Q[2] + qf * var)
    return(subset(x, x >= lim[1] & x <= lim[2]))
  }
  print.progress <- function(x, lognormal = FALSE) {
    if (lognormal) {
      x <- exp(x)
    }
    return(c(length(x), min(x), max(x)))
  }

  n.steps <- 5
  for (i in 1:n.steps) {
    target.quantile <- 1 - q.trunc * i / n.steps
    alpha <- (2 * q.trunc) * (i - 1) / n.steps
    qf <- qnorm(target.quantile)/qnorm(0.75 - 0.25 * alpha) # qf = quantile factor

    xx <- truncate.x(xx, qf)
    progress <- rbind(progress, print.progress(xx, lognormal = lognormal))
  }

  n0 <- 1
  n1 <- 0
  qf <- qnorm(1 - q.trunc)/qnorm(0.75 - 0.5 * q.trunc)
  while (n0 > n1) {
    i <- i + 1
    n0 <- length(xx)
    xx <- truncate.x(xx, qf)
    n1 <- length(xx)
    progress <- rbind(progress, print.progress(xx, lognormal = lognormal))
  }
  if (lognormal) {
    xx <- exp(xx)
  }
  lim <- c(lower = min(xx), upper = max(xx))
  if (apply.rounding) {
    lim <- round(lim, digits)
  }
  prop <- round(length(xx) * 100/(1 - (2 * perc.trunc / 100))/n, 1)
  if (prop > 100) {
    prop <- 100
  }
  if (plot.it) {
    x1 <- x[x < median(x) + 8 * IQR(x)]
    d0 <- density(x1)
    d1 <- data.frame(d0$x, d0$y)
    d <- subset(d1, d1[, 1] >= lim[1] & d1[, 1] <= lim[2])
    if (n < 200) {
      breaks = "Sturges"
    }
    else {
      delta <- max(x1) - min(x1)
      breaks = seq(from = min(x1) - 0.1 * delta, to = max(x1) + 0.1 * delta, by = (lim[2] - lim[1])/10)
    }
    hist(x1, freq = FALSE, breaks = breaks, yaxt = "n",
         ylim = c(0, max(d[, 2]) * 1.5), main = main, xlab = xlab,
         ylab = "", col = "white", border = "grey")
    lines(d[, 1], d[, 2], col = "blue", lwd = 2)
    lines(rep(lim[1], 2), c(0, d[1, 2]), col = "blue",
          lwd = 2)
    lines(rep(lim[2], 2), c(0, d[nrow(d), 2]), col = "blue",
          lwd = 2)
    text(lim[1], 0, round(lim[1], digits), pos = 3)
    text(lim[2], 0, round(lim[2], digits), pos = 3)
    boxplot(x1, at = max(d[, 2]) * 1.4, boxwex = max(d[,2])/10,
            col = "white", pch = 20, horizontal = T,
            add = T)
    boxplot(xx, at = max(d[, 2]) * 1.25, boxwex = max(d[,2])/10,
            col = "blue", pch = 20, horizontal = T,
            add = T)
  }
  progress[, 2:3] <- round(progress[, 2:3], digits)
  row.names(progress) <- paste0("cycle", 0:i)
  return(list(trunc = xx, truncation.points = lim, lognormal = lognormal,
              perc.norm = prop, progress = progress))

}
