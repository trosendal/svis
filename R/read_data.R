##' sample_data
##'
##' Some dummy sample data to use for a point map (or whatever)
##' @export
##' @importFrom utils read.csv2
##' @return a data.frame
##' @examples
##' \dontrun{
##' # Get the data
##' df <- sample_data()
##' str(df)
##' }
sample_data <- function() {
    path <- system.file("extdata/sample_data_cwd.csv",
                        package = "svis")
    read.csv2(path,
              header = TRUE,
              stringsAsFactors = FALSE,
              encoding = "UTF-8")
}

df <- sample_data()
library(sp)
library(rgdal)

##' sweref99
##'
##' The proj4str of sweref99
##'
##' @return a string
sweref99 <- function() {
    "+init=epsg:3006"
}

##' RT90
##'
##' The proj4str of RT90
##'
##' @return a string
RT90 <- function() {
    "+init=epsg:3021"
}

##' WGS84
##'
##' The proj4str of WGS84
##'
##' @return a string
WGS84 <- function() {
    "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
}
