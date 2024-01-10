interpretation <- function(limits, targets){
  if(length(limits) != 2){stop("Limits must be a vector with length 2.")}
  if(limits[1] >= limits[2]){stop("The upper limit must be greater than the lower limit.")}
  if(limits[1] <= 0 | limits[2] <= 0){stop("Only positive limit values allowed.")}
  if(length(targets) != 2){stop("targets must be a vector with length 2.")}
  if(targets[1] >= targets[2]){stop("The upper target limit must be greater than the lower target limit.")}
  if(targets[1] <= 0 | targets[2] <= 0){stop("Only positive target values allowed.")}

  tol.lim <- permissible_uncertainty(limits[1], limits[2])
  col.lim <- c(lower.limit = rgb(0.7, 0.7, 0.7, 0.5), upper.limit = rgb(0.7, 0.7, 0.7, 0.5))
  dev.lim <- c(lower.limit = NA, upper.limit = NA)

  tol.tar <- permissible_uncertainty(targets[1], targets[2])
  col.tar <- col.lim
  if(limits[1] >= tol.tar[1] & limits[1] <= tol.tar[2]){
    col.lim[1] <-  rgb(0, 1, 0, 0.5)
  } else {
    if((tol.lim[1] >= tol.tar[1] & tol.lim[1] <= tol.tar[2]) |
       (tol.lim[2] >= tol.tar[1] & tol.lim[2] <= tol.tar[2])){
      col.lim[1] <- rgb(1, 1, 0, 0.5)
    } else {col.lim[1] <- rgb(1, 0, 0, 0.5)}
  }
  if(limits[2] >= tol.tar[3] & limits[2] <= tol.tar[4]){
    col.lim[2] <-  rgb(0, 1, 0, 0.5)
  } else {
    if((tol.lim[3] >= tol.tar[3] & tol.lim[3] <= tol.tar[4]) |
       (tol.lim[4] >= tol.tar[3] &  tol.lim[4] <= tol.tar[4])){
      col.lim[2] <- rgb(1, 1, 0, 0.5)
    } else {col.lim[2] <- rgb(1, 0, 0, 0.5)}
  }
  dev.lim[1 : 2] <- c("within tolerance", "within tolerance")
  for(i in 1 : 2){
    if(col.lim[i] == rgb(1, 1, 0, 0.5)){
      if(limits[i] < targets[i]){
        dev.lim[i] <- "slightly decreased"
      } else {
        dev.lim[i] <- "slightly increased"
      }
    }
    if(col.lim[i] == rgb(1, 0, 0, 0.5)){
      if(limits[i] < targets[i]){
        dev.lim[i] <- "markedly decreased"
      } else {
        dev.lim[i] <- "markedly increased"
      }
    }
  }
  return(list(tol.lim = tol.lim, tol.tar = tol.tar,
              col.lim = col.lim, col.tar = col.tar,
              dev.lim = dev.lim))
}
