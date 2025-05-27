# *reflimR*: Reference Limit Estimation Using Routine Laboratory Data

The *reflimR* package provides a simple and robust three-step procedure for the estimation of reference intervals from routine laboratory data that may contain an unknown proportion of pathological results. Reference intervals play a crucial role in the medical interpretation and statistical evaluation of laboratory results.

## Installation

To install the *reflimR* package, open R and enter the following command in the console:

```r
install.packages("reflimR")
```

This command will download the package from CRAN. Once the installation is complete, load the package into your R session using the following command:

```r
library(reflimR)
```

The package will then be ready for use in your R environment (e.g. in RStudio). To see the documentation of the package with all its help files, please enter

```r
help(package = reflimR)
```

## Usage

Here is a basic example demonstrating how to use *reflimR*:

```r
library(reflimR)

x <- c(rnorm(800, 100, 10), rnorm(100, 70, 15), rnorm(100, 125, 15))
reflim(x, targets = qnorm(c(0.025, 0.975), 100, 10))
```

## Publication

*reflimR* has been published in: Hoffmann, G., Klawitter, S., Trulson, I., Adler, J., Holdenrieder, S., & Klawonn, F. (2024). A Novel Tool for the Rapid and Transparent Verification of Reference Intervals in Clinical Laboratories. Journal of clinical medicine, 13(15), 4397. https://doi.org/10.3390/jcm13154397
