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
    df <- read.csv2(path,
                    header = TRUE,
                    stringsAsFactors = FALSE,
                    encoding = "UTF-8")
    df$Djurslag <- factor(df$Djurslag)
    df
}

##' read_sample_data_asf
##'
##' Some dummy sample data to use for a point map (or whatever)
##'
##' @importFrom utils read.csv
##' @return a data.frame
read_sample_data_asf <- function() {
    path <- system.file("extdata/E18-024_Grundrapport.csv",
                        package = "svis")
    read.csv(path,
             header = TRUE,
             stringsAsFactors = FALSE,
             sep="\t",
             encoding = "UTF-8")
}
##' crs
##'
##' @param x A Spatial object
##' @param ... other arguements
##' @return One of these c("sweref99", "RT90", "WGS84")
crs <- function(x, ...) UseMethod("crs")

##' crs.SpatialPointsDataFrame
##'
##' @param x A SpatialPointsDataFrame
##' @param ... other arguements
##' @return One of these c("sweref99", "RT90", "WGS84")
crs.SpatialPointsDataFrame <- function(x, ...) {
    stopifnot(grepl("+init=epsg", x@proj4string@projargs))
    matches <- regexec("+init=epsg:([0-9]*)", x@proj4string@projargs)
    epsg <- as.numeric(regmatches(x@proj4string@projargs, matches)[[1]][2])
    if(epsg == 4326) return("WGS84")
    if(epsg == 3021) return("RT90")
    if(epsg == 3006) return("sweref99")
    stop('I only know how to handle c("sweref99", "RT90", "WGS84")')
}

##' epsg
##'
##' The epsg of various crs
##'
##' @return numeric value
##' @param x A character
##' @param ... other arguements
epsg <- function(x = c("sweref99", "RT90", "WGS84")) {
    x <- match.arg(x)
    switch(x,
           sweref99 = return(3006),
           RT90 = return(3021),
           WGS84 = return(4326))
}

##' sweref99
##'
##' The proj4str of sweref99
##'
##' @return a string
sweref99 <- function() {
    paste0("+init=epsg:", epsg("sweref99"))
}

##' RT90
##'
##' The proj4str of RT90
##'
##' @return a string
RT90 <- function() {
    paste0("+init=epsg:", epsg("RT90"))
}

##' WGS84
##'
##' The proj4str of WGS84
##'
##' @return a string
WGS84 <- function() {
    paste0("+init=epsg:", epsg("WGS84"))
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
