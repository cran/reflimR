\docType{data}
\name{Dataset: livertests}
\alias{livertests}
\title{Dataset: livertests}
\description{
Example data showing eight different biomarkers (laboratory tests), which are frequently measured in healthy controls and patients with liver diseases.
}
\format{
A data frame with 612 rows and 11 columns:
\describe{
   \item{Category}{healthy reference individual or patient}
   \item{Age}{age in years}
   \item{Sex}{sex f = female or m = male}
   \item{ALB}{albumin, g/L}
   \item{ALT}{alanine aminotransferase, U/L}
   \item{AST}{aspartate aminotransferase, U/L}
   \item{BIL}{bilirubin, µmol/l}
   \item{CHE}{choline esterase, kU/L}
   \item{CREA}{creatinine, µmol/L}
   \item{GGT}{gamma-glutamyl transferase, U/L}
   \item{PROT}{total protein, mg/L}
}
}
\source{
<https://archive.ics.uci.edu/dataset/571/hcv+data>
}
\usage{
livertests
}
\examples{
summary(livertests)
pie(table(livertests$Category), labels = c("patients", "controls"))
plot(livertests$Age, livertests$ALB, xlab = "Age [yr]", ylab = "ALB [g/L]")
grid()
abline(lm(livertests$ALB ~ livertests$Age))

che <- livertests$CHE
ref <- livertests$CHE[livertests$Category == "reference"]
pat <- livertests$CHE[livertests$Category == "patient"]

hist(che, breaks = 1 : 20, col = "white", main = "cholinesterase", xlab = "kU/L")
hist(ref, breaks = 1 : 20, col = rgb(0, 1, 0, 0.5), add = TRUE)
hist(pat, breaks = 1 : 20, col = rgb(1, 0, 0, 0.5), add = TRUE)
legend("topright", fill = c(rgb(1,1,1,1), rgb(0,1,0,0.5), rgb(1,0,0,0.5)),
    legend = c("all", "controls", "patients"))

t.test(ref, pat)
var.test(ref, pat)

che.f <- livertests$CHE[livertests$Sex == "f"]
che.m <- livertests$CHE[livertests$Sex == "m"]
plot(density(che.f), xlim = c(0, 20), col = "red",
   main = "cholinesterase", xlab = "kU/L")
lines(density(che.m), col = "blue")
legend("topright", lty = 1, col = c("red", "blue"), legend = c("females", "males"))

reflim(che.m, main = "CHE (m)", xlab = "kU/L")
reflim(livertests$AST[livertests$Sex == "m"], main = "AST (m)", xlab = "U/L")
}
