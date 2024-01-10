\name{bowley}
\alias{bowley}
\title{Bowley skewness}
\description{
Calculates a robust skewness measure for x based on the interquartile range (or any other quantile range).
}
\usage{
bowley(x, alpha = 0.25)
}
\arguments{
\item{x}{numeric vector}
\item{alpha}{lower quantile of the range to be regarded (e. g. 0.25)}
}
\value{
Bowley's quantile skewness
}
\details{
Bowley's quantile skewness is calculated from (q[1] - 2 * q[2] + q[3]) / (q[3] - q[1]), where q is a vector of quantiles alpha, 0.5, and 1 - alpha. The default value for alpha = 0.25 indicates an interval from the first to the third quartile.
}
\references{
1. Bowley, AL (1920). Elements of Statistics. London : P.S. King & Son, Ltd.

2. Klawonn F, Hoffmann G, Orth M. Quantitative laboratory results: normal or lognormal distribution. J Lab Med 2020; 44: 143–50. \doi{10.1515/labmed-2020-0005}.
}
\examples{
bowley(1 : 100)
bowley(rnorm(1000, 3, 0.2))
bowley(rlnorm(1000, 3, 0.5))
}
