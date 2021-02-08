# dfo

<!-- badges: start -->
[![R-CMD-check](https://github.com/dankelley/dfo/workflows/R-CMD-check/badge.svg)](https://github.com/dankelley/dfo/actions)
[![codecov](https://codecov.io/gh/dankelley/dfo/branch/main/graph/badge.svg)](https://codecov.io/gh/dankelley/dfo)
<!-- badges: end -->

## Installation

Since `dfo` is not on CRAN, it must be installed from its github repository, with
```R
remotes::install_github("dankelley/dfo", ref="main")
```
If this fails, first install the `remotes` package with
```R
install.packages("remotes")
```

## Usage

The following reads, summarizes, and plots an IOS-formatted CTD
file that is provided with the package. (Type `?read.ctd.ios` in
an R console for more details).
```R
library(dfo)
file <- system.file("extdata", "2007-019-055.ctd", package="dfo")
ctd <- read.ctd.ios(file)
summary(ctd)
plot(ctd)
```

