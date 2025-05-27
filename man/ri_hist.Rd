\name{ri_hist}
\alias{ri_hist}
\title{Histogram with density plots and reference limits}
\description{Creates a graphic illustrating the results of the reflim function.}
\usage{
ri_hist(x, lognormal, stats, limits, perc.norm,
  targets = NULL, remove.extremes = TRUE,
  main = "reflim", xlab = "x")
}
\arguments{
\item{x}{vector of positive numbers}
\item{lognormal}{Boolean indicating whether a lognormal distribution should be assumed}
\item{stats}{vector of mean and sd, or meanlog and sdlog, respectively}
\item{limits}{vector of reference limits calculated by the reflim function (or any other suitable algorithm)}
\item{perc.norm}{estimated percentage of non-pathological values (usually provided by the iboxplot function)}
\item{targets}{vector of target reference limits obtained from external sources}
\item{remove.extremes}{Boolean indicating whether extreme values should be removed to improve the graphic}
\item{main, xlab}{title and x label of the graphic}
}
\value{
\item{$lognormal}{assumed distribution model}
\item{$percent_normal}{assumed percentage of non-pathological values}
\item{$interpretation}{text describing the deviation of observed limits from target values}
}
\details{
ri_hist is called by the reflim function, but it can also be used to illustrate the results of other software packages (e. g. refineR), if the required arguments are available (see details).

It creates a graphic, which includes a histogram and a density curve of x, as well as a theoretical density curve of presumably non-pathological values (blue) and a calculated density curve of presumably pathological values (red). Calculated reference limits and target limits are shown as vertical lines, and their tolerance intervals (i. e., the permissible uncertainties) are represented by surrounding boxes. If target values are provided, traffic light colors indicate the deviation between target and actual results.

If the arguments lognormal or perc.norm are unknown, they can be set according to the user's expertise. For example, if the distribution type is unknown, a lognormal distribution can be assumed [1]. If the percentage of non-pathological values has not been provided by a foreign algorithm (e. g. refineR), it can be roughly estimated, if density curves of normal and pathological values are available (the argument perc.norm does not influence the result; its only effect is on the shape of the theoretical density curve).
}

\references{
1. Haeckel R, Wosniok W. Observed unknown distributions of clinical chemical quantities should be considered to be log-normal. Clin Chem Lab Med 2010; 48: 1393-6. \doi{10.1515/CCLM.2010.273}.
}
\examples{
set.seed(123)
x1 <- rlnorm(800, 3, 0.3)
lim <- quantile(x1, c(0.025, 0.975))
ri_hist(x1, lognormal = TRUE, stats = c(3, 0.3), limits = lim, perc.norm = 100)

x2 <- rlnorm(200, 3.5, 0.4)
x <- c(x1, x2)
tar <- quantile(x, c(0.025, 0.975))
ri_hist(x, lognormal = TRUE, stats = c(3, 0.3), limits = lim, targets = tar, perc.norm = 80)
}
