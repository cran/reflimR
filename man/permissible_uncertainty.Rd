\name{permissible_uncertainty}
\alias{permissible_uncertainty}
\title{Tolerance intervals of estimated and target limits}
\details{
The tolerance limits (also called equivalence limits) indicate the permissible uncertainty of a reference limit from a medical point of view (in contrast to the confidence interval, which reflects the statistical point of view). The calculation is based on a recommendation made by the DGKL [1, 2].
}
\usage{
permissible_uncertainty(lower.limit, upper.limit, apply.rounding = TRUE)
}
\arguments{
\item{lower.limit}{positive number indicating the lower limit of the reference interval}
\item{upper.limit}{positive number indicating the upper limit of the reference interval}
\item{apply.rounding}{Boolean indicating whether the tolerance limits should be rounded}
}
\value{
Tolerance intervals for the lower and upper reference limits (ranging from lower.lim.low to lower.lim.upp for the lower reference limit and from upper.lim.low to upper.lim.upp for the upper reference limit)
}
\references{
1. Haeckel R et al. Permissible limits for uncertainty of measurement in laboratory medicine. Clin Chem Lab Med 2015;53:1161–71. \doi{10.1515/cclm-2014-0874}.

2. Haeckel R et al. Equivalence limits of reference intervals for partitioning of population data. J Lab Med 2016; 40: 199-205. \doi{10.1515/labmed-2016-0002}.
}
\examples{
permissible_uncertainty(lower.limit = 10, upper.limit = 50)
permissible_uncertainty(10, 50, FALSE)
}
\description{
Returns the permissible uncertainty of reference limits.
}
