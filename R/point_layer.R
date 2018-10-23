##' build a layer
##'
##' @param data Currently a SpatialPointsDataFrame
##' @param ... Arguments passed to specific layer functions for
##'     different classes
##' @return a svis_layer object
##' @export
layer <- function(data, ...) UseMethod("layer")

##' layer
##'
##' @param data The dataset in sp::SpatialPointsDataFrame format
##' @param layer_title The human readable name of the layer in the map
##' @param byvar The variable in the dataframe to use to colour the
##'     points. Will be coerced into a factor. If NULL, one colour
##'     will be used.
##' @param col The colour(s) for the points. If you supply less
##'     colours than the number of levels in 'byvar', the colours
##'     will be reused. You my not supply more colours than levels in
##'     'byvar'.
##' @param ... Other arguments
##' @import hlt
##' @export
##' @return A svis_layer
layer.SpatialPointsDataFrame <- function(data,
                                         layer_title = "layer1",
                                         byvar = NULL,
                                         col = NULL,
                                         ...){
    ## temp vars. These are to be implemented in same way as col is
    ## currently. However radius may need it's own var.
    radius <- 5
    fillColor <- shQuote("#00A9CE")
    color <- shQuote("black")
    weight <- 1
    opacity <- 1
    fillOpacity <- 1

    ## Add the data
    data_name <- paste0("data_", gsub(" ", "_", layer_title))
    layer_name <- paste0("layer_", gsub(" ", "_", layer_title))

    ## Check that the by var is a factor
    if(!is.null(byvar)) {
        data@data[, byvar] <- as.factor(data@data[, byvar])
        ## this could be useful for the legend:
        byvarname <- byvar
        bylabs <- levels(data@data[, byvar])
        byvar <- sort(unique(as.numeric(data@data[, byvar])))
        jsbyvar <- paste0("feature.properties.", byvarname )
        ## now that we have the levels we can convert to numeric we
        ## want in the json:
        data@data[, byvar] <- as.numeric(data@data[, byvar])
    }
    if(is.null(byvar)) {
        jsbyvar <- "feature.id"
    }

    ## The colour of the points
    getcol <- fillColor_js(layername = layer_name,
                           values = byvar,
                           col = col)

    ## Convert the data to json
    jsondata <- convert_to_geojson(data)

    data_load <-  html_script(c(paste(data_name, "="),
                           jsondata))

    ## Define the layer
    layer <- c(paste("var", layer_name, "= new L.LayerGroup();"),
               paste0("L.geoJson(", data_name, ",{"),
               "style: function (feature) {",
               "return feature.properties && feature.properties.style;",
               "},",
               paste(c("onEachFeature: ", onEachFeature(), ","), collapse = ""),
               "pointToLayer: function (feature, latlng) {",
               "return L.circleMarker(latlng, {",
               paste0("radius: ", as.character(radius),  ","),
               paste0("fillColor: ", layer_name, "_getfillColor(", jsbyvar, "),"),
               paste0("color: ", as.character(color), ","),
               paste0("weight: ", as.character(weight), ","),
               paste0("opacity: ", as.character(opacity), ","),
               paste0("fillOpacity: ", as.character(fillOpacity), ","),
               "});",
               "}",
               paste0("}).addTo(", layer_name, ");"))
    script <- list(data_load, getcol, html_script(c(onEachFeature(), layer)))
    object <- list(name = layer_name,
                   title = layer_title,
                   script = script)
    class(object) <- "svis_layer"
    object
}

##' layers
##'
##' @param list_of_layers a list of svis_layer objects
##' @export
##' @return a svis_layers object
layers <- function(list_of_layers) {

    ## check that we have a list
    stopifnot(identical(class(list_of_layers), "list"))

    ## Check that the layers are all the class svis_layer
    lapply(list_of_layers, function(x) {
        stopifnot(identical(class(x), "svis_layer"))
    })

    ## Check that none of th elayers have the same name
    stopifnot(all(!duplicated(lapply(list_of_layers, "[", "name"))))

    class(list_of_layers) <- "svis_layers"
    list_of_layers
}

##' names.svis_layer
##'
##' @param x a svis_layer object
##' @export
##' @return name of the layer
names.svis_layer <- function(x) {
    x$name
}

##' names.svis_layers
##'
##' @param x a svis_layers object
##' @export
##' @return names of the layers
names.svis_layers <- function(x) {
    do.call("c", lapply(x, names.svis_layer))
}

##' Get the scripts
##'
##' @param object A svis_layer or svis_layers object
##' @return a list of hlt_script objects
##' @export
scripts <- function(object) UseMethod("scripts")

##' scripts.svis_layer
##'
##' @param object A svis_layer object
##' @export
##' @return a script
scripts.svis_layer <- function(object) {
    object$script
}

##' scripts.svis_layers
##'
##' @param object a svis_layers object
##' @export
##' @return a list of scripts
scripts.svis_layers <- function(object) {
    do.call("c", lapply(object, scripts.svis_layer))
}
