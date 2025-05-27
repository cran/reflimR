\name{conf_int95}
\alias{conf_int95}
\title{Confidence intervals of estimated reference limits}
\description{
Calculates 95 percent confidence intervals for the lower and upper reference limits obtained with the reflim algorithm.
}
\usage{
conf_int95(n, lower.limit, upper.limit, lognormal = TRUE, apply.rounding = TRUE)
}
\arguments{
  \item{n}{number of observations}
  \item{lower.limit}{positive number indicating the lower limit of the reference interval}
  \item{upper.limit}{positive number indicating the upper limit of the reference interval}
  \item{lognormal}{Boolean indicating whether a lognormal distribution should be assumed}
  \item{apply.rounding}{Boolean indicating whether the confidence limits should be rounded}
}
\value{
95 percent confidence limits and total number of observations for the lower and the upper reference limit (ranging from lower.lim.low to lower.lim.upp for the lower reference limit, and from upper.lim.low to upper.lim.upp for the upper reference limit)
}
\details{
The confidence limits depend on the reference range (upper minus lower limit), and are proportional to 1/sqrt(n).

The coefficients used in this function have been determined by 100,000 Monte-Carlo simulations for sample sizes between n = 200 and n = 2,000 based on a standard normal distribution.
}
\examples{
conf_int95(n = 250, lower.limit = 10, upper.limit = 50)
conf_int95(250, 135, 145, FALSE, FALSE)
}
