##' sample_data
##'
##' Get some sample data
##'
##' @return A SpatialPointsDataFrame
##' @export
##' @importFrom utils read.table
##' @examples
##' \dontrun{
##' # Get the data
##' library(sp)
##' pts <- sample_data()
##' plot(pts)
##' }
##' @param dataset The name of the dataset. Either "cwd" or "asf"
sample_data <- function(dataset = c("cwd", "asf")) {
    dataset <- match.arg(dataset)
    cwdfunction <- function(){
        df <- read_sample_data_cwd()
        convert_to_sppts(df,
                         RT90(),
                         WGS84(),
                         long = "Gisy",
                         lat = "Gisx")
    }
    asffunction <- function(){
        df <- read_sample_data_asf()
        convert_to_sppts(df,
                         RT90(),
                         WGS84(),
                         long = "Gisy",
                         lat = "Gisx")
    }
    switch(dataset,
           cwd = cwdfunction(),
           asf = asffunction())
}

##' read_sample_data_cwd
##'
##' Some dummy sample data to use for a point map (or whatever)
##'
##' @importFrom utils read.csv2
##' @return a data.frame
read_sample_data_cwd <- function() {
    path <- system.file("extdata/sample_data_cwd.csv",
                        package = "svis")
    read.csv2(path,
              header = TRUE,
              stringsAsFactors = FALSE,
              encoding = "UTF-8")
}

##' read_sample_data_asf
##'
##' Some dummy sample data to use for a point map (or whatever)
##'
##' @importFrom utils read.csv2
##' @return a data.frame
read_sample_data_asf <- function() {
    path <- system.file("extdata/E18-024_Grundrapport.csv",
                        package = "svis")
    a=read.csv(path,
              header = TRUE,
              stringsAsFactors = FALSE,
              sep="\t",
              encoding = "UTF-8")
}

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
    "+init=epsg:4326"
}

##' convert_to_sppts
##'
##' Convert a data.frame with X, Y points to a spatial point
##' data.frame
##'
##' @param df A data.frame that contains X and Y coordinates
##' @param input_proj The projection of the  input
##' @param output_proj The projection of the output
##' @param long The name of the variable that contains the longitude
##' @param lat The name of the variable that contains the latitude
##' @importFrom sp spTransform
##' @importFrom sp SpatialPoints
##' @importFrom sp SpatialPointsDataFrame
##' @importFrom sp "proj4string<-"
##' @importFrom stats complete.cases
##' @return SpatialPointsDataFrame
convert_to_sppts <- function(df,
                             input_proj,
                             output_proj = WGS84(),
                             long,
                             lat) {
    stopifnot(c(lat, long) %in% names(df))
    missing_coords <- length(which(!complete.cases(df[,c(long,lat)])))
    if(missing_coords > 0){
        warning(paste(missing_coords, "of the submitted points are missing coordinates and will be discarded"))
    }
    df <- df[complete.cases(df[, c(long, lat)]),]
    pts <- SpatialPoints(cbind(df[, long], df[, lat]))
    proj4string(pts) <- input_proj
    pts <- spTransform(pts, CRSobj = output_proj)
    pts <- SpatialPointsDataFrame(pts, df)
    return(pts)
}

##' convert_to_geojson
##'
##' @importFrom rgdal writeOGR
##' @export
##' @param spatial_object A sp::SpatialPointsDataFrame, sp::SpatialLinessDataFrame or sp::SpatialPolygonsDataFrame
##' @return A character vector
convert_to_geojson <- function(spatial_object) {
    stopifnot(class(spatial_object) %in% c("SpatialPointsDataFrame",
                                   "SpatialLinesDataFrame",
                                   "SpatialPolygonsDataFrame"))
    innerfile <- tempfile()
    writeOGR(spatial_object,
             innerfile,
             layer = "main",
             driver = "GeoJSON",
             check_exists = FALSE)
    jsondata <- readLines(innerfile)
    class(jsondata) <- c(class(jsondata), "svis_geojson")
    jsondata
}
