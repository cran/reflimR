\name{iboxplot}
\alias{iboxplot}
\title{Removal of pathological values}
\description{
Iteratively truncates a vector of quantitative laboratory results until no more values outside the specified truncation interval are left.
}
\usage{
iboxplot(x, lognormal = NULL, perc.trunc = 2.5,
  apply.rounding = TRUE, plot.it = TRUE, main = "iBoxplot", xlab = "x")
}
\arguments{
\item{x}{vector of positive numbers}
\item{lognormal}{Boolean indicating whether a lognormal distribution should be assumed (NULL means that the distribution type is defined automatically)}
\item{perc.trunc}{percentage of presumably normal values to be removed from each side. If perc.trunc is increased (e.g. to 3.5 instead of 2.5), more values are removed.}
\item{apply.rounding}{Boolean indicating whether the estimated reference limits should be rounded}
\item{plot.it}{Boolean indicating whether a graphic should be created}
\item{main, xlab}{title and x label of the graphic}
}
\value{
\item{$trunc}{truncated vector x}
\item{$truncation.points}{truncation points, preliminary reference limits}
\item{$lognormal}{Boolean indicating whether a lognormal distribution has been assumed}
\item{$perc.norm}{proportion of the assumed non-pathological values}
\item{$progress}{results of the iterative truncation}
}
\details{
The truncated vector represents the estimated central 95 percent of values, which follow the assumed distribution (normal or lognormal). If the distribution of the reference values is unknown, medical laboratory results should be assumed to be lognormally distributed [2].
}
\references{
1. Klawonn F, Hoffmann G. Using fuzzy cluster analysis to find interesting clusters. In: L.A. Garcia-Escuderoet al. (eds.): Building bridges between soft and statistical methodologies for data science. Springer, Cham (2023), 231-239. \doi{10.1007/978-3-031-15509-3_31}.

2. Haeckel R, Wosniok W. Observed unknown distributions of clinical chemical quantities should be considered to be log-normal. Clin Chem Lab Med 2010; 48: 1393-6. \doi{10.1515/CCLM.2010.273}.
}
\examples{
set.seed(123)
iboxplot(rlnorm(n = 250, meanlog = 3,  sdlog = 0.3))

iboxplot(rnorm(1000, 100, 10), apply.rounding = FALSE, plot.it = FALSE)$truncation.points

alb.trunc <- iboxplot(livertests$ALB, main = "ALB", xlab = "g/L")$trunc
summary(alb.trunc)
}
