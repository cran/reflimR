\name{adjust_digits}
\alias{adjust_digits}
\title{Plausible rounding}
\description{
Rounds a quantitative laboratory result to a reasonable number of decimal places.
}
\usage{
adjust_digits(x)
}
\arguments{
\item{x}{numeric}
}
\value{
List containing a rounded value of x and the number of decimal places
}
\examples{
adjust_digits(0.001234)
adjust_digits(0)
adjust_digits(-12.34)
adjust_digits(5.4321)$digits
}
