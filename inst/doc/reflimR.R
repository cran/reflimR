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
x <- livertests$PROT
reflim(x, main = "total protein", xlab = "g/L", targets = targetvalues[8, 3 : 4])$interpretation

## ----fig.width=5, fig.height=5------------------------------------------------
lognorm(livertests$ALB, main = "albumin", xlab = "g/L")

## ----fig.width=5, fig.height=5------------------------------------------------
lognorm(livertests$ALT, main = "alanine aminotransferase", xlab = "U/L")

## ----fig.width=5, fig.height=5------------------------------------------------
x <- livertests$BIL
trunc.bil <- iboxplot(x, main = "bilirubin", xlab = "µmol/L")

## -----------------------------------------------------------------------------
trunc.bil$progress

## -----------------------------------------------------------------------------
trunc.bil$perc.norm

## -----------------------------------------------------------------------------
trunc.bil$truncation.points

## ----fig.width=5, fig.height=5------------------------------------------------
truncated_qqplot(trunc.bil$trunc)

## ----fig.width=8, fig.height=8------------------------------------------------
ast.f <-  reflim(livertests$AST[livertests$Sex == "f"], plot.all = TRUE, n.min = 150,
       targets = targetvalues[3, 3 : 4],
       main = "AST (f)", xlab = "U/L")

## ----fig.width=6, fig.height=4------------------------------------------------
prot.f <- livertests$PROT[livertests$Sex == "f"] 
ln <- lognorm(prot.f, main = "PROT (f)", xlab = "g/L")
arrows(76, 0.055, 76, 0.075, code = 1, length = 0.1, lwd = 2)

## ----fig.width=6, fig.height=4------------------------------------------------
xtrunc.f <- iboxplot(prot.f, xlab = "g/L")$trunc 
arrows(77.5, 0.07, 77.5, 0.09, code = 1, length = 0.1, lwd = 2)

## ----fig.width=6, fig.height=4------------------------------------------------
qq.f <- truncated_qqplot(xtrunc.f, n.min = 100) 
arrows(1.4, 80, 1.4, 77.5, code = 2, length = 0.1, lwd = 2)

## ----fig.width=6, fig.height=6------------------------------------------------
reflim(prot.f, perc.trunc = 5, n.min = 100,  plot.all = TRUE, print.n = FALSE, 
       main = "PROT (f)", xlab = "g/L")$limits[1 : 2]

