#' Read a CTD file in an IOS text format
#'
#' The `read.ctd.ios.text()` function reads CTD data in a text format used by
#' the Institution of Ocean Sciences, of the Department of Fisheries
#' and Oceans, Canada.  Lacking a document from IOS describing
#' the format, the code is written based on inspection of
#' a particular file, and for this reason, it cannot be trusted
#' deeply.
#'
#' @param filename character value specifying the file name. This must end
#' in `".ctd"`.
#'
#' @param missingValue numeric value that is to be converted to `NA`. If this is `NULL`,
#' the default, then an attempt to infer a reasonable value is made by
#' examining information stored in the file.
#'
#' @param debug integer value controlling the printing of information
#' that may help in debugging problems. The default, 0, means to work
#' silently; positive values mean that some information should be printed.
#'
#' @section Development note:
#' This function was written on 2021-02-06, based on the examination
#' of a single test file. Since this is a risky way to code,
#' efforts are underway to find official documentation for the
#' file format.  If such documentation becomes available, the
#' function will be changed accordingly.
#'
#' @examples
#' library(dfo)
#' # File provided to author by a colleague.
#' file <- system.file("extdata", "2007-019-055.ctd", package="dfo")
#' ctd <- read.ctd.ios(file)
#' summary(ctd)
#' plot(ctd)
#'
#' @references
#' \url{https://catalogue.cioos.ca/dataset/ios_ctd_profiles}
#'
#' @return [read.ctd.ios.text()] returns an `oce` object of class `ctd`.
#'
#' @author Dan Kelley
#'
#' @importFrom oce processingLogAppend
#' @importFrom methods new
#' @importFrom utils read.table
#'
#' @export
read.ctd.ios.text <- function(filename, missingValue=NULL, debug=0)
{
    if (!grepl(".ctd$", filename))
        stop("filename value must end in \".ctd\"")
    nameMaker <- function(name) {
        res <- switch(name,
            "Pressure"="pressure",
            "Depth"="depth",
            "Temperature:Primary"="temperature",
            "Salinity:T0:C0"="salinity",
            "PAR"="par",
            "PAR:Reference"="parReference",
            "Transmissivity"="transmissivity",
            "Fluorescence:URU:Seapoint"="fluorescence",
            "Oxygen:Dissolved:SBE"="oxygen",
            "Number_of_bin_records"="ndata",
            "Nitrate_plus_nitrite:ISUS"="nitrate_plus_nitrite")
        if (is.null(res)) name else res
    }
    unitMaker <- function(unit) {
        switch(unit,
            "decibar"=list(unit=expression(dbar), scale=""),
            "metres"=list(unit=expression(m), scale=""),
            "'deg C (ITS90)'"=list(unit=expression(degree*C), scale="ITS-90"),
            "%/metre"=list(unit=expression("%"/m), scale=""),
            "mg/m^3"=list(unit=expression(mg/m^3), scale=""),
            "uE/m^2/sec"=list(unit=expression(mu*E/m^2/s), scale=""),
            "PSS-78"=list(unit=expression(), scale="PSS-78"),
            "mL/L"=list(unit=expression(ml/l), scale=""),
            "umol/kg"=list(unit=expression(mu*mol/kg), scale=""),
            "volts"=list(unit=expression(V), scale=""),
            "n/a"=list(unit=expression(), scale=""))
    }
    getBlock <- function(lines, blockName) {
        pattern <- paste0("^\\*", blockName, "$")
        blockStart <- grep(pattern, lines)
        n <- length(blockStart)
        if (n == 0)
            stop("This file does not have a block matching \"", pattern, "\"")
        if (n > 1)
            stop("This file has more than one block matching \"", pattern, "\"")
        blockEnd <- grep("^\\*", lines)
        blockEnd <- blockEnd[blockEnd > blockStart][1]
        lines[seq(blockStart, blockEnd-1L)]
    }

    if (!requireNamespace("oce"))
        stop("The 'oce' package must be installed for read.ctd.ios() to work")
    res <- methods::new("ctd")
    #? res <- new("ctd")

    lines <- readLines(filename, encoding="latin1")
    endLine <- grep("^\\*END OF HEADER$", lines)
    if (0 == length(endLine))
        stop("file \"", filename, "\" does not contain an \"*END OF HEADER\" line")
    headerLines <- lines[seq(1, endLine-1)]
    # LOCATION block (longitude, latitude, station)
    locationBlock <- getBlock(headerLines, "LOCATION")
    tmp <- strsplit(locationBlock[grep("LONGITUDE", locationBlock)], " +")[[1]]
    longitude <- (as.numeric(tmp[4]) + as.numeric(tmp[5])/60) * ifelse(tmp[6] == "W", -1, 1)
    tmp <- strsplit(locationBlock[grep("LATITUDE", locationBlock)], " +")[[1]]
    latitude <- (as.numeric(tmp[4]) + as.numeric(tmp[5])/60) * ifelse(tmp[6] == "S", -1, 1)
    tmp <- strsplit(locationBlock[grep("STATION", locationBlock)], " +")[[1]]
    station <- tmp[4]
    # FILE block (time)
    fileBlock <- getBlock(headerLines, "FILE")
    tmp <- strsplit(fileBlock[grep("START TIME", fileBlock)], " +")[[1]]
    startTime <- as.POSIXct(paste(tmp[6], tmp[7]), tz="UTC")
    # ADMINISTRATION block (cruise, PI and ship)
    administrationBlock <- getBlock(headerLines, "ADMINISTRATION")
    if (debug > 0)
        print(administrationBlock)
    res@metadata$mission <- strsplit(administrationBlock[grep("MISSION", administrationBlock)], "[ ]:[ ]*")[[1]][2]
    res@metadata$agency <- strsplit(administrationBlock[grep("AGENCY", administrationBlock)], "[ ]*:[ ]*")[[1]][2]
    res@metadata$country <- strsplit(administrationBlock[grep("COUNTRY", administrationBlock)], "[ ]*:[ ]*")[[1]][2]
    res@metadata$project <- strsplit(administrationBlock[grep("PROJECT", administrationBlock)], "[ ]*:[ ]*")[[1]][2]
    res@metadata$scientist <- strsplit(administrationBlock[grep("SCIENTIST", administrationBlock)], "[ ]*:[ ]*")[[1]][2]
    res@metadata$platform <- strsplit(administrationBlock[grep("PLATFORM", administrationBlock)], "[ ]*:[ ]*")[[1]][2]

    if (is.null(missingValue)) {
        channelDetailBlockStart <- grep("\\$TABLE: CHANNEL DETAIL", fileBlock)
        missingValue <- as.numeric(strsplit(fileBlock[channelDetailBlockStart + 4L], "[ ]+")[[1]][3])
        if (debug > 0)
            cat("Inferred missingValue as", missingValue, "\n")
    }

    # CHANNELS table, withing FILE block
    channelBlockStart <- grep("\\$TABLE: CHANNELS", fileBlock)
    unitList <- list()
    names <- NULL
    namesOriginal <- list()
    j <- 1 # index to variable names
    for (i in seq(channelBlockStart + 3L, length(fileBlock))) {
        if (length(grep("\\$END", fileBlock[i])))
            break
        thisName <- gsub("^[ ]*[0-9]{1,2}[ ]*([^ ]*) .*$", "\\1", fileBlock[i])
        namesOriginal[[j]] <- thisName
        names[j] <- nameMaker(thisName)
        thisUnit <- gsub("^[ ]*[0-9]{1,2}[ ]*[^ ]*[ ]*([^ ]+|'.*').*$", "\\1", fileBlock[i])
        unitList[[j]] <- unitMaker(thisUnit)
        j <- j + 1
        if (debug > 0) {
            cat(oce::vectorShow(i), msg="  ")
            cat(oce::vectorShow(fileBlock[i], msg="   "))
            cat(oce::vectorShow(thisName, msg="   "))
            cat(oce::vectorShow(thisUnit, msg="   "))
        }
    }
    names <- oce::unduplicateNames(names)
    names(unitList) <- names
    names(namesOriginal) <- names
    dataLines <- lines[seq(endLine+1, length(lines))]
    data <- utils::read.table(filename, skip=1 + endLine, col.names=names)
    data[data == missingValue] <- NA
    res@data <- data
    res@metadata$units <- unitList
    res@metadata$dataNamesOriginal <- namesOriginal
    res@metadata$longitude <- longitude
    res@metadata$latitude <- latitude
    res@metadata$startTime <- startTime
    res@metadata$station <- station
    res@processingLog <- oce::processingLogAppend(res@processingLog,
        paste0("read.ctd.ios.text(\"", filename, "\", missingValue=", missingValue, ", debug=", debug, ")\n"))
    res
}

#' Read a CTD file in an IOS netCDF format
#'
#' The `read.ctd.ios.netcdf()` function reads CTD data in a netCDF format used by
#' the Institution of Ocean Sciences, of the Department of Fisheries
#' and Oceans, Canada.  It starts by using [oce::read.netcdf()] to
#' decode the data.  Then, it sets the class to [oce::ctd-class],
#' after which it creates new variables in the `oce` naming
#' convention, with
#' `PRESPR01` becoming `pressure`,
#' `TEMPS901` becoming `temperature`,
#' `PSALST01` becoming `salinity`,
#' `DOXYZZ01` becoming oxygen in ml/l, and
#' `DOXMZZ01` becoming oxygen2 in mu*mol/kg.
#' Note that the new variables are in the *vector* format used in
#' `oce` objects, whereas the variables read from the netCDF
#' file are in one-column matrix format. To avoid problems in
#' other work, use something akin to
#' `as.vector(ctd[["FOO"`]]`, where `"FOO"` is the name of
#' a data item in an object named `ctd`.
#'
#' @param filename character value specifying the file name. This value must end
#' in `".nc"`.
#'
#' @param missingValue numeric value that is to be converted to `NA`. If this is `NULL`,
#' the default, then an attempt to infer a reasonable value is made by
#' examining information stored in the file.
#'
#' @param debug integer value controlling the printing of information
#' that may help in debugging problems. The default, 0, means to work
#' silently; positive values mean that some information should be printed.
#'
#' @examples
#' library(dfo)
#' tempFile <- tempfile(fileext=".nc")
#' url <- "https://data.cioospacific.ca/erddap/files/IOS_CTD_Profiles/2007/2007-019-0055.ctd.nc"
#' download.file(url, tempFile, mode="wb")
#' ctd <- read.ctd.ios(tempFile)
#' summary(ctd)
#' # Note that using eos="gsw" in the following plot call will
#' # cause an error with oce version 1.3.0 (on CRAN as of Feb 2021),
#' # but it works with the updated oce, installed using
#' #     remotes::install_github("dankelley/oce", ref="develop")
#' plot(ctd, eos="unesco")
#' unlink(tempFile)
#'
#' @references
#' \url{https://data.cioospacific.ca/erddap/files/IOS_CTD_Profiles}
#'
#' @return [read.ctd.ios()] returns an `oce` object of class `ctd`.
#'
#' @author Dan Kelley
#'
#' @importFrom oce numberAsPOSIXct oceSetData processingLogAppend read.netcdf
#' @importFrom methods new
#' @importFrom ncdf4 nc_version
#'
#' @export
read.ctd.ios.netcdf <- function(filename, missingValue=NULL, debug=0)
{
    if (!grepl(".nc$", filename))
        stop("filename value must end in \".nc\"")
    # oce needs ncdf4, but it does not depend on it, because only
    # a very few oce functions need this ability. Therefore, to get
    # automated checks to work with the present package, we need to
    # import from ncdf4.  The following uses a simple function that
    # should not take much time.  It will produce an error if ncdf4
    # cannot be installed, and it will come from dfo, not from oce,
    # so that might be clearer for users.
    if (0 == nchar(ncdf4::nc_version()))
       stop("cannot initialize ncdf4")
    res <- methods::new("ctd")
    tmp <- oce::read.netcdf(filename)
    res@data <- tmp@data
    res@metadata <- tmp@metadata
    res <- oce::oceSetData(res, "pressure",
        as.vector(res[["PRESPR01"]]),
        unit=list(unit=expression(dbar), scale=""))
    res <- oce::oceSetData(res, "temperature",
        as.vector(res[["TEMPS901"]]),
        unit=list(unit=expression(degree*C), scale="ITS-90"))
    res <- oce::oceSetData(res, "salinity",
        as.vector(res[["PSALST01"]]),
        unit=list(unit=expression(), scale="PSS-78"))
    res <- oce::oceSetData(res, "oxygen",
        as.vector(res[["DOXYZZ01"]]),
        unit=list(unit=expression(ml/l), scale=""))
    res <- oce::oceSetData(res, "oxygen2",
        as.vector(res[["DOXMZZ01"]]),
        unit=list(unit=expression(mu*mol/kg), scale=""))
    # Some things entered the data slot, but should be in the
    # metadata slot instead.  Move them, one by one.  (We
    # must convert time to a POSIXt value first.)
    res@data$time <- oce::numberAsPOSIXct(res@data$time)
    res@metadata$startTime <- res@data$time[1]
    for (item in c("filename", "country", "mission_id", "scientist", "project",
            "agency", "platform", "instrument_type", "instrument_model",
            "instrument_serial_number", "latitude", "longitude",
            "geographic_area", "event_number", "profile", "time")) {
        res@metadata[[item]] <- res@data[[item]]
        res@data[[item]] <- NULL
    }
    res@processingLog <- oce::processingLogAppend(res@processingLog,
        paste0("read.ctd.ios.netcdf(\"", filename, "\", missingValue=",
            missingValue, ", debug=", debug, ")\n"))
    res
}

#' Read an IOS-formatted CTD file
#'
#' The `read.ctd.ios()` function reads CTD data in either of two formats used by
#' the Institution of Ocean Sciences, of the Department of Fisheries
#' and Oceans, Canada.  The formats are inferred from the value of the
#' `filename` argument.  If that value ends in `".ctd"`, then [read.ctd.ios.text()] is
#' called to read the data.  If it ends in `".nc"`, then [read.ctd.ios.netcdf()]
#' is called. (No other endings are handled.)
#'
#' @param filename character value specifying the file name.
#'
#' @param missingValue numeric value that is to be converted to `NA`. If this is `NULL`,
#' the default, then an attempt to infer a reasonable value is made by
#' examining information stored in the file.
#'
#' @param debug integer value controlling the printing of information
#' that may help in debugging problems. The default, 0, means to work
#' silently; positive values mean that some information should be printed.
#'
#' @section Development note:
#' 1. Add lines to nameMaker() and unitMaker() as required, to handle
#'    column names that are not yet handled.
#'
#' @references
#' \url{https://catalogue.cioos.ca/dataset/ios_ctd_profiles}
#'
#' @return [read.ctd.ios()] returns an `oce` object of class `ctd`.
#'
#' @author Dan Kelley
#'
#' @export
read.ctd.ios <- function(filename, missingValue=NULL, debug=0)
{
    if (!is.character(filename))
        stop("First argument must be a character value.")
    if (grepl(".nc$", filename))
        read.ctd.ios.netcdf(filename, missingValue, debug)
    else if (grepl(".ctd$", filename))
        read.ctd.ios.text(filename, missingValue, debug)
    else
        stop("filename must end in either \".nc\" or \".ctd\"")
}


