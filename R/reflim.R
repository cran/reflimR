reflim <- function(x, lognormal = NULL, targets = NULL,
                   perc.trunc = 2.5, n.min = 200, apply.rounding = TRUE,
                   plot.it = TRUE, plot.all = FALSE, print.n = TRUE,
                   main = "reference limits", xlab = "x"){

  xx <- na.omit(x)

  result <- list(stats = c(mean = NA, sd = NA, n.total = NA, n.trunc = NA),
                 lognormal = lognormal,
                 limits = c(lower.lim = NA, upper.lim = NA,
                            lower.lim.low = NA, lower.lim.upp = NA,
                            upper.lim.low = NA, upper.lim.upp = NA),
                 targets = c(lower.lim = NA, upper.lim = NA,
                             lower.lim.low = NA, lower.lim.up = NA,
                             upper.lim.low = NA, upper.lim.upp = NA),
                 perc.norm = NA,
                 confidence.int = c(lower.lim.low = NA, lower.lim.upp = NA,
                                    upper.lim.low = NA, upper.lim.upp = NA,
                                    n = NA),
                 interpretation = c(lower.limit = NA, upper.limit = NA),
                 remarks = NA)

  if(!is.numeric(xx)){
      warning("x must be numeric. Non-numeric values removed.")

      xx <- as.numeric(xx)
      xx <- na.omit(xx)

      result$remarks <- "Non-numeric values removed"
    }
  if(min(xx) <= 0){
      warning("Only positive values allowed. values <= 0 removed.")
      xx <- xx[xx > 0]
      result$remarks <- "Values <= 0 removed"
    }
  if(!is.null(targets)){

    targets <- na.omit(as.numeric(targets))

    if(length(targets) != 2){
      warning("targets must be a vector with length 2. NA not allowed. Targets removed.")
      targets = NULL
      result$remarks <- "Unsuitable target values removed"
    }
  }
  if(!is.null(targets)){
    if(is.na(targets[1]) | is.na(targets[2])){
      warning("Targets must be numeric. NA not allowed. Targets removed.")
      targets = NULL
      result$remarks <- "Unsuitable target values removed"
    }
  }
  if(!is.null(targets)){
    if(targets[1] >= targets[2]){
      warning("The upper target limit must be greater than the lower target limit. Targets removed. ")
      targets = NULL
      result$remarks <- "Unsuitable target values removed"
    }
  }
  if(!is.null(targets)){
    if(targets[1] <= 0 | targets[2] <= 0){
      warning("Only positive target values allowed. Targets removed.")
      targets = NULL
      result$remarks <- "Unsuitable target values removed"
    }
  }
  n <- length(xx)
  if(n < 40){
    warning(paste("n = ", n, ". The absolute minimum for reference limit estimation is 40. NAs returned."))
    result$stats[3] <- n
    result$remarks <- "Total n < 40"
    return(result)
    }
  if(n < n.min){
    warning(paste("n =", n, "where a minimum of", n.min, "is required. n.min has been set to 40 at a potential loss of accuracy."))
    result$stats[3] <- n
    result$remarks <- "Attention: low n."
    n.min <- 40
  }

  digits <- adjust_digits(median(xx))$digits
  if(is.null(lognormal)){
    plot.logtype <- TRUE
    lognormal <- lognorm(xx, plot.it = FALSE)$lognormal
  } else {
    plot.logtype <- FALSE
  }

  res.lognorm <- lognorm(xx, plot.it = FALSE)
  res.trunc <- iboxplot(xx, lognormal = lognormal,
                        perc.trunc = perc.trunc,
                        apply.rounding = apply.rounding,
                        plot.it = FALSE)
  n.trunc <- length(res.trunc$trunc)
  if(n.trunc < 40){
    warning(paste("n = ", n.trunc, "after truncation. The absolute minimum for reference limit estimation is 40. NAs returned."))
    result$stats[3] <- n
    result$stats[4] <- n.trunc
    result$remarks <- "n < 40 after truncation."
    return(result)
  }
  if(n.trunc < n.min){
    warning(paste("n.trunc =", n.trunc, "where a minimum of", n.min, "is required. n.min has been set to 40 at a potential loss of accuracy."))
    result$stats[3] <- n
    result$stats[4] <- n.trunc
    result$remarks <- "Low n after truncation."
    n.min <- 40
  }
  res.qq <- truncated_qqplot(res.trunc$trunc, lognormal = lognormal,
                             perc.trunc = perc.trunc, n.min = n.min,
                             apply.rounding = apply.rounding, plot.it = FALSE)$result
  res.ci <- conf_int95(n = n,
                       lower.limit = as.numeric(res.qq[3]),
                       upper.limit = as.numeric(res.qq[4]),
                       lognormal = lognormal, apply.rounding = apply.rounding)
  if(res.qq[3] > 0){
    res.pu <- permissible_uncertainty(lower.limit = as.numeric(res.qq[3]),
                                    upper.limit = as.numeric(res.qq[4]),
                                    apply.rounding = apply.rounding)
  } else {
    warning("Estimated lower limit <= 0. No tolerance limits calculated. No graphics produced.")
    res.pu <- rep(NA, 4)
    targets = NULL
    result$remarks <- "Lower limit <= 0"
  }

  res.lim <- c(as.numeric(res.qq[3 : 4]), as.numeric(res.pu))
  names(res.lim) <- c("lower.lim", "upper.lim", "lower.lim.low", "lower.lim.upp", "upper.lim.low", "upper.lim.upp")
  if(apply.rounding){res.lim <- round(res.lim, digits)}
  res.tar <- c(lower.lim = NA, upper.lim = NA,
               lower.lim.low = NA, lower.lim.up = NA,
               upper.lim.low = NA, upper.lim.upp = NA)
  dev.lim <- c(lower.limit = NA, upper.limit = NA)
  if(!is.null(targets)){
    ip <- interpretation(res.lim[1 : 2], targets)
    res.tar[1 : 2] <- targets
    res.tar[3 : 6] <- ip$tol.tar
    if(apply.rounding){res.tar <- round(res.tar, digits)}
    dev.lim <- ip$dev.lim
  }
  if(res.qq[3] > 0){
    if(plot.all){plot.it <- TRUE}
    if(plot.all){
      oldpar <- par(mfrow = c(2, 2))
      on.exit(par(oldpar))
    }
    if(plot.it){
      rh <- ri_hist(xx, lognormal = lognormal, stats = res.qq[1 : 2],
              limits = res.qq[3 : 4], targets = targets,
              perc.norm = res.trunc$perc.norm,
              main = main, xlab = xlab)
      if(print.n){
        legend("topright", legend = paste("n =", n.trunc, "after truncation"),
             bty = "n", cex = 0.75)
      }
    }
    if(plot.all){
      lognorm(xx, main = "Step 1: Bowley skewness", xlab = "", plot.logtype = plot.logtype)
      iboxplot(xx, lognormal = lognormal, perc.trunc = perc.trunc,
                          apply.rounding = apply.rounding,
               main = "Step 2: iBoxplot", xlab = "")
      truncated_qqplot(res.trunc$trunc, lognormal = lognormal,
                     perc.trunc = perc.trunc, n.min = n.min,
                     apply.rounding = apply.rounding,
                     main = "Step 3: Q-Q plot", xlab = "", ylab = "")
    }
  }

  result$stats = c(res.qq[1 : 2], n.total = n, n.trunc = n.trunc)
  result$lognormal = lognormal
  result$limits = res.lim
  result$targets = res.tar
  result$perc.norm = res.trunc$perc.norm
  result$confidence.int = res.ci[1 : 4]
  result$interpretation = dev.lim

  return(result)
}
