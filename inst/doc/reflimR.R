## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval = FALSE-------------------------------------------------------------
#  install.packages("reflimR")

## -----------------------------------------------------------------------------
library(reflimR)

## -----------------------------------------------------------------------------
help(package = reflimR)

## ----echo = FALSE-------------------------------------------------------------
print(livertests[c(1, 204, 444, 589),])

## ----echo=FALSE---------------------------------------------------------------
targetvalues

## -----------------------------------------------------------------------------
targetvalues[1, 3 : 4]

## ----fig.width=5, fig.height=5------------------------------------------------
x <- livertests$BIL
reflim(x)

## ----fig.width=5, fig.height=5------------------------------------------------
reflim(x, main = "bilirubin", xlab = "µmol/L", targets = targetvalues[4, 5 : 6])$interpretation

## ----fig.width=5, fig.height=5------------------------------------------------
lognorm(livertests$ALB, main = "albumin", xlab = "g/L")

## ----fig.width=5, fig.height=5------------------------------------------------
lognorm(livertests$ALT, main = "alanine aminotransferase", xlab = "µmol/L")

## ----fig.width=5, fig.height=5------------------------------------------------
x <- livertests$BIL
x1 <- iboxplot(x, main = "bilirubin", xlab = "µmol/L")

## -----------------------------------------------------------------------------
x1$progress

## -----------------------------------------------------------------------------
x1$perc.norm

## -----------------------------------------------------------------------------
x1$truncation.points

## ----fig.width=5, fig.height=5------------------------------------------------
truncated_qqplot(x1$trunc)

## ----fig.width=8, fig.height=8------------------------------------------------
ggt.f <- livertests$GGT[livertests$Sex == "f"] 
reflim(ggt.f, plot.all = TRUE, n.min = 150,
       targets = targetvalues[7, 3 : 4],
       main = "GGT (f)", xlab = "U/L")$interpretation

## ----fig.width=6, fig.height=4------------------------------------------------
ggt.m <- livertests$GGT[livertests$Sex == "m"] 
ln <- lognorm(ggt.m, main = "GGT (m)", xlab = "U/L")
arrows(46, 0.015, 46, 0.02, code = 1, length = 0.1, lwd = 2)

## ----fig.width=6, fig.height=4------------------------------------------------
xtrunc.m <- iboxplot(ggt.m, xlab = "U/L")$trunc 
arrows(55, 0.007, 55, 0.012, code = 1, length = 0.1, lwd = 2)

## ----fig.width=6, fig.height=4------------------------------------------------
qq.m <- truncated_qqplot(xtrunc.m) 
arrows(1.5, log(30), 1.5, log(45), code = 2, length = 0.1, lwd = 2)

