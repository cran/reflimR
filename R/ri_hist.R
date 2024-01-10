ri_hist <- function(x, lognormal, stats, limits, perc.norm,
                    targets = NULL, remove.extremes = TRUE,
                    main = "reflim", xlab = "x"){
  xx <- na.omit(x)
  if(!is.numeric(xx)){stop("x must be numeric.")}
  if(min(xx) <= 0){stop("x must be a vector of positive numbers.")}
  if(length(stats) != 2){stop("stats must be a vector with length 2 containing mean (or meanlog) and sd (or sdlog).")}
  if(length(limits) != 2){stop("limits must be a vector with length 2.")}
  if(is.numeric(limits)){
    if(limits[1] >= limits[2]){stop("The upper limit must be greater than the lower limit.")}
    if(limits[1] <= 0 | limits[2] <= 0){stop("Only positive limit values allowed.")}
  }
  if(!is.null(targets)){
    if(length(targets) != 2){stop("targets must be a vector with length 2.")}
    if(targets[1] >= targets[2]){stop("The upper target limit must be greater than the lower target limit.")}
    if(targets[1] <= 0 | targets[2] <= 0){stop("Only positive limit values allowed.")}
  }

  digits <- adjust_digits(median(xx))$digits
  n <- length(xx)
  if(n < 40){{stop(paste0("n = ", n, ". The absolute minimum for reference limit estimation is 40."))}}

  if(remove.extremes){xx <- xx[xx <= median(xx) + 8 * IQR(xx)]}
  if(n < 200){breaks <- "Sturges"} else {
    difference <- max(xx) - min(xx)
    from = min(xx) - 0.1 * difference
    if(from < 0) {from <- 0}
    to = max(xx) + 0.1 * difference
    if(to < limits[2]) { to <- limits[2] + 0.1 * difference}
    breaks <- seq(from = from, to = to, by = (limits[2] - limits[1]) / 10)
  }
  d <- density(xx)
  d.max <- max(d$y)

  if(lognormal){
    d1 <- dlnorm(d$x, stats[1], stats[2])
  } else {
    d1 <- dnorm(d$x, stats[1], stats[2])
  }

  tol.lim <- permissible_uncertainty(limits[1], limits[2])
  col.lim <- c(rgb(0.7, 0.7, 0.7, 0.5), rgb(0.7, 0.7, 0.7, 0.5))
  dev.lim <- c(lower.limit = NA, upper.limit = NA)

  if(!is.null(targets)){
    ip <- interpretation(limits, targets)
    tol.lim <- ip$tol.lim
    tol.tar <- ip$tol.tar
    col.lim <- ip$col.lim
    col.tar <- ip$col.tar
    dev.lim <- ip$dev.lim
  }

  hist(xx, freq = FALSE, breaks = breaks, yaxt = "n",
       ylim = c(0, max(d.max, max(d1 * perc.norm / 100))),
       col = "white", border = "grey",
       main = main, xlab = xlab, ylab = "")
  box()
  if(!is.null(targets)){
    rect(tol.tar[1], 0, tol.tar[2], d.max * 0.8, col = col.tar[1], border = NA)
    rect(tol.tar[3], 0, tol.tar[4], d.max * 0.8, col = col.tar[2], border = NA)
  }
  rect(tol.lim[1], 0, tol.lim[2], d.max * 0.8, col = col.lim[1], border = NA)
  rect(tol.lim[3], 0, tol.lim[4], d.max * 0.8, col = col.lim[2], border = NA)

  lines(d, lty = 3)
  lines(d$x, d1 * perc.norm / 100, lwd = 2, col = "blue")
  d2 <- d$y - d1
  d2[d2 < 0] <- 0
  lines(d$x, d2, col = "red", lty = 2)

  lines(rep(limits[1], 2), c(0, d.max * 0.8), lty = 2)
  lines(rep(limits[2], 2), c(0, d.max * 0.8), lty = 2)
  text(limits, rep(d.max * 0.85, 2), round(limits, digits))
  if(!is.null(targets)){
    lines(rep(targets[1], 2), c(0, d.max * 0.8))
    lines(rep(targets[2], 2), c(0, d.max * 0.8))
    text(targets[1 : 2], rep(d.max * 0.92, 2),
         round(targets[1 : 2], digits), col = "grey")
  }
  return(list(lognormal = lognormal, percent_normal = perc.norm, interpretation = dev.lim))
}
