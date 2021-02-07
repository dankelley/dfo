## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(dfo)
library(oce)

context("CTD")

test_that("read.ctd.ios.netcdf()", {
  tempFile <- tempfile(fileext=".nc")
  url <- "https://data.cioospacific.ca/erddap/files/IOS_CTD_Profiles/2007/2007-019-0055.ctd.nc"
  download.file(url, tempFile)
  ctd <- expect_silent(read.ctd.ios(tempFile))
  unlink(tempFile)
})

test_that("read.ctd.ios.text()", {
  file <- system.file("extdata", "2007-019-055.ctd", package="dfo")
  ctd <- expect_silent(read.ctd.ios(file))
})

