##' svis_point
##'
##' @param data A data.frame with one row
##' @param id The id of the point
##' @return a svis_point
svis_point <- function(data, id) {
    stopifnot(class(data) == "data.frame",
              nrow(data) == 1,
              "x" %in% names(data),
              "y" %in% names(data))
    ob <- as.list(data)
    attributes(ob)$id <- id
    class(ob) <- c("svis_point", "svis_feature")
    ob
}

##' format.svis_point
##'
##' @param x A svis_point
##' @param ... Other parameters
##' @export
##' @return formated point in json
format.svis_point <- function(x, ...) {
    coords <- paste0("[", paste(x[c("x", "y")], collapse = ", "), "]")
    precoord <- "\"geometry\": {\"type\": \"Point\", \"coordinates\" : "
    postcoord <- "}"
    coords <- paste0(precoord, coords, postcoord, "\n")
    props <- paste(mapply(function(x, y) {
        if(class(y) %in% c("integer", "numeric")) {
            if(is.na(y)) y <- shQuote(y)
            return(paste0(shQuote(x), ": ", y))
        }
        paste0(shQuote(x), ": ", shQuote(y))
    }, names(x)[!names(x) %in% c("x", "y")],
    x[!names(x) %in% c("x", "y")]), collapse = ", ")
    preprop <- "\"properties\": {"
    postprop <- "}"
    props <- paste0(preprop, props, postprop)
    id <- paste0("\"id\": ", attributes(x)$id)
    type <- "\"type\": \"Feature\""
    pt <- paste0("{", paste(c(type, id, props, coords), collapse = ",\n"), "}")
    pt
}

##' print.svis_point
##'
##' @param x A svis_point
##' @param ... other arguments
##' @export
##' @return NULL
print.svis_point <- function(x, ...) {
    cat(format(x, ...), "\n")
}

##' svis_points
##'
##' @param data A dataframe or spatial dataframe
##' @param ... Other arguments
##' @export
##' @return A svis_points object
svis_points <- function(data, ...) UseMethod("svis_points")

##' svis_points.data.frame
##'
##' @param data A data.frame with at least x and y and columns
##' @param crs The crs of the data c("sweref99", "RT90", "WGS84"). If null the function tries to guess the crs.
##' @param ... Other Arguments
##' @export
##' @return A svis_points object
svis_points.data.frame <- function(data, crs = NULL, ...) {
    if(is.null(crs)) {
        crs <- crs(data)
    } else {
        crs <- match.arg(crs, choices = c("sweref99", "RT90", "WGS84"))
    }
    ob <- lapply(seq_len(nrow(data)), function(i){
        svis_point(data[i, , drop = FALSE], id = i)
    })
    attributes(ob)$epsg <- epsg(crs)
    class(ob) <- c("svis_points", "svis_features")
    ob
}

##' svis_points.SpatialPointsDataFrame
##'
##' @param data A spatialPointsDataFrame
##' @param ... Other Arguments
##' @importFrom sp coordinates
##' @export
##' @return A svis_points object
svis_points.SpatialPointsDataFrame  <- function(data, ...) {
    crs <- crs(data)
    df <- data@data
    df$y <- coordinates(data)[,"coords.x2"]
    df$x <- coordinates(data)[,"coords.x1"]
    svis_points(df)
}

##' format.svis_points
##'
##' @param x A svis_points object
##' @param ... Other arguments
##' @export
##' @return formatted object in json
format.svis_points <- function(x, ...) {
    obpts <- paste(lapply(x, format), collapse = ", \n")
    collection <- paste0("{\"type\": \"FeatureCollection\",\n",
                         "\"name\": \"main\",\n",
                         "\"crs\": { \"type\": \"link\", \"properties\": { \"href\": \"https://spatialreference.rg/ref/epsg/",
                         attributes(x)$epsg,
                         "/proj4\", \"type\": \"proj4\" }},\n")
    prepts <- "\"features\": [\n"
    postpts <- "]"
    obpts <- paste0(collection, prepts, obpts, postpts, "}")
    obpts
}

##' print.svis_points
##'
##' @param x A svis_points object
##' @param ... Other arguments
##' @export
##' @return NULL
print.svis_points <- function(x, ...) {
    cat(format(x, ...), "\n")
}

##' crs.data.frame
##'
##' @param data a data.frame with x and y
##' @return one of c("sweref99", "RT90", "WGS84")
crs.data.frame <- function(data) {
    stopifnot(class(data) == "data.frame",
              "x" %in% names(data),
              "y" %in% names(data))
    if(all(data$x <= 180)) return("WGS84")
    if(all(data$y >= 1083427.2970)) return("RT90")
    if(all(data$y < 1083427.2970)) return("SWEREF99")
    stop("Cannot detect crs from data")
}
