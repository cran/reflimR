\docType{data}
\name{Dataset: targetvalues}
\alias{targetvalues}
\title{Dataset: target values}
\description{
Test names (analytes), units and reference limits from a textbook.
}
\format{
A data frame with 8 rows and 6 columns:
\describe{
   \item{analyte}{short name of the analyte}
   \item{unit}{measuring unit}
   \item{ll.female, ul.female}{lower and upper reference limits for women}
   \item{ll.male, ul.male}{lower and upper reference limits for men}
}}
\source{Thomas L. Clinical Laboratory Diagnostics, 2023}
\details{The table was created from the data in the textbook (web version 2023, https://www.clinical-laboratory-diagnostics.com). Missing data (i.e. the lower limits for ALT, AST and GGT) were supplemented from the product sheets of the respective tests.}
\usage{targetvalues}
\examples{
targetvalues[, 1 : 4]
reflim(livertests$ALB[livertests$Sex == "m"],
main = targetvalues[1, 1], xlab = targetvalues[1, 2],
targets = targetvalues[1, 5 : 6])
}
