\name{interpretation}
\alias{interpretation}
\title{Colors and text modules to interprete deviations from given  target values}
\description{Creates traffic light colors green, yellow, and red as well as a textual description such as 'slightly increased'. This function is called by reflim(), if target values are available, and provides the required color information for ri_hist().}
\usage{interpretation(limits, targets)}
\arguments{
\item{limits}{vector of two numbers indicating the reference limits that have been calculated by the reflim function (or any other suitable algorithm)}
\item{targets}{vector of two numbers indicating target reference limits that may have been obtained from external sources}
}
\value{
\item{$tol.lim and $tol.tar}{tolerance limits for the estimated reference limits and the respective target values. If targets are not provided, the latter tolerance limits are returned as NA.}
\item{$col.lim and $col.tar}{hexadecimal rgb values, indicating the traffic light colors green, yellow, and red}
\item{$dev.lim}{short text describing the deviations of the observed limit values from the target values}
}
\details{
This algorithm compares the positions and tolerance intervals of the estimated upper and lower reference limits with the tolerance limits of the respective target values.

If the estimated reference limit is within the tolerance of the target value, the dev.lim text says 'within tolerance' and the color code #00FF0080 for semi-transparent green is returned.

If the position is outside and the two tolerance limits overlap, the dev.lim text says 'slightly increased' or 'slightly decreased' and the color code #FFFF0080 for semi-transparent yellow is returned.

If the tolerance limits do not overlap, the dev.lim text says 'markedly increased' or 'markedly decreased' and the color code #FF000080 for semi transparent red is returned.
}
\examples{
interpretation(limits = c(10, 50), targets = c(11, 49))
interpretation(limits = c(10, 50), targets = c(8, 60))$dev.lim
}
