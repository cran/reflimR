\name{lognorm}
\alias{lognorm}
\title{Lognormal distribution model}
\description{
Suggests lognormal modelling of a numeric vector x by comparing Bowley's quantile skewness for x and log(x). Lognormality is suggested if bowley(x) - bowley(log(x)) >= cutoff.
}
\usage{
lognorm(x, cutoff = 0.05, alpha = 0.25, digits = 3,
  plot.it = TRUE, plot.logtype = TRUE, main = "Bowley skewness", xlab = "x")
}
\arguments{
\item{x}{numeric vector of positive numbers}
\item{cutoff}{skewness threshold for the suggestion of a lognormal distribution}
\item{alpha}{lower quantile of the range to be regarded (e. g. 0.25 for IQR)}
\item{digits}{number of digits to be displayed for the Bowley skewness}
\item{plot.it}{Boolean indicating whether a graphic should be created}
\item{plot.logtype}{Boolean indicating whether the distribution type should be printed in the graphic}
\item{main, xlab}{title and x label of the graphic}
}
\value{
\item{$lognorm}{Boolean indicating whether a lognormal distribution should be assumed}
\item{$BowleySkewness}{Bowley skewness of the original and the logtransformed values as well as the difference}
}
\details{
If $lognorm is TRUE, a lognormal distribution is suggested for right-skewed density curves (bowley(x) > 0). The decision for a lognormal distribution is based on the difference between the skewness of the original and the logtransformed values (cut-off defaults to 0.05).

In the unusual case of a left-skewed distribution (bowley(x) < 0), a normal distribution is suggested (lognormal = FALSE), assuming that the left skew is caused by pathological low values rather than an unusual distribution of laboratory results.

The plot illustrates the skewness of x and log(x) showing density curves and boxplots with separate x-axes for the original values (bottom axis) and the log-transformed values (top axis). A skewness delta below the cut-off value means that both curves are quite symmetric. In this case, x can be approximated by a normal distribution. If the delta exceeds the cut-off, the density curve of the original values and the respective boxplot are right-skewed and become more symmetric after log-transformation.

The plot.logtype argument is used internally to suppress printing the automated definition of the distribution in case that the type has been set manually.

Extreme values are removed before plotting to improve the graphic.
}
\references{
1. Klawonn F, Hoffmann G, Orth M. Quantitative laboratory results: normal or lognormal distribution? J Lab Med 2020; 44: 143-50. \doi{10.1515/labmed-2020-0005}.
}
\examples{
lognorm(rnorm(n = 1000, mean = 20, sd = 2))
lognorm(rlnorm(n = 1000, meanlog = 3,  sdlog = 0.3))
lognorm(livertests$ALB, main = "albumin", xlab = "g/L")
lognorm(livertests$BIL, main = "bilirubin", xlab = "µmol/L")
}
