% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.ctd.ios.R
\name{read.ctd.ios.netcdf}
\alias{read.ctd.ios.netcdf}
\title{Read a CTD file in an IOS netCDF format}
\usage{
read.ctd.ios.netcdf(filename, missingValue = NULL, debug = 0)
}
\arguments{
\item{filename}{character value specifying the file name. This value must end
in \code{".nc"}.}

\item{missingValue}{numeric value that is to be converted to \code{NA}. If this is \code{NULL},
the default, then an attempt to infer a reasonable value is made by
examining information stored in the file.}

\item{debug}{integer value controlling the printing of information
that may help in debugging problems. The default, 0, means to work
silently; positive values mean that some information should be printed.}
}
\value{
\code{\link[=read.ctd.ios]{read.ctd.ios()}} returns an \code{oce} object of class \code{ctd}.
}
\description{
The \code{read.ctd.ios.netcdf()} function reads CTD data in a netCDF format used by
the Institution of Ocean Sciences, of the Department of Fisheries
and Oceans, Canada.  It starts by using \code{\link[oce:read.netcdf]{oce::read.netcdf()}} to
decode the data.  Then, it sets the class to \link[oce:ctd-class]{oce::ctd},
after which it creates new variables in the \code{oce} naming
convention, with
\code{PRESPR01} becoming \code{pressure},
\code{TEMPS901} becoming \code{temperature},
\code{PSALST01} becoming \code{salinity},
\code{DOXYZZ01} becoming oxygen in ml/l, and
\code{DOXMZZ01} becoming oxygen2 in mu*mol/kg.
Note that the new variables are in the \emph{vector} format used in
\code{oce} objects, whereas the variables read from the netCDF
file are in one-column matrix format. To avoid problems in
other work, use something akin to
\verb{as.vector(ctd[["FOO"}]]\verb{, where }"FOO"\verb{is the name of a data item in an object named}ctd`.
}
\examples{
library(dfo)
tempFile <- tempfile(fileext=".nc")
url <- "https://data.cioospacific.ca/erddap/files/IOS_CTD_Profiles/2007/2007-019-0055.ctd.nc"
download.file(url, tempFile, mode="wb")
ctd <- read.ctd.ios(tempFile)
summary(ctd)
# Note that using eos="gsw" in the following plot call will
# cause an error with oce version 1.3.0 (on CRAN as of Feb 2021),
# but it works with the updated oce, installed using
#     remotes::install_github("dankelley/oce", ref="develop")
plot(ctd, eos="unesco")
unlink(tempFile)

}
\references{
\url{https://data.cioospacific.ca/erddap/files/IOS_CTD_Profiles}
}
\author{
Dan Kelley
}
