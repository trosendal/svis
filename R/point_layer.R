##' build a layer
##'
##' @param data svis_points
##' @param ... Arguments passed to specific layer functions for
##'     different classes
##' @return a svis_layer object
##' @export
layer <- function(data, ...) UseMethod("layer")

##' layer
##'
##' @param data The dataset in svis_points format
##' @param layer_title The human readable name of the layer in the map
##' @param byvar The variable in the dataframe to use to colour the
##'     points. Will be coerced into a factor. If NULL, one colour
##'     will be used.
##' @param col The colour(s) for the points. If you supply less
##'     colours than the number of levels in 'byvar', the colours will
##'     be reused. You my not supply more colours than levels in
##'     'byvar'.
##' @param outline_col the Outline colour of th points
##' @param radius The radius of the points
##' @param fillOpacity The fill opacity of the points
##' @param opacity The opacity of the opline of the points
##' @param ... Other arguments
##' @import hlt
##' @export
##' @return A svis_layer
layer.svis_points <- function(data,
                              layer_title = "layer1",
                              byvar = NULL,
                              col = NULL,
                              outline_col = NULL,
                              radius = NULL,
                              fillOpacity = NULL,
                              opacity = NULL,
                              ...) {
    ## Just make the data a list to facilitate adding attributes at
    ## the upper level
    ob <- list(data)

    ## Check the levels of the byvar
    df <- as.data.frame(data)
    if(!is.null(byvar)) {
        stopifnot(byvar %in% names(df))
        df[, byvar] <- as.factor(df[, byvar])
        ## this could be useful for the legend:
        attributes(ob)$bylabs <- levels(df[, byvar])
    }

    ## Add attributes to layer
    attributes(ob)$layer_title = layer_title
    attributes(ob)$byvar <- byvar
    attributes(ob)$fillColor <- col
    attributes(ob)$color <- outline_col
    attributes(ob)$radius <- radius
    attributes(ob)$opacity <- opacity
    attributes(ob)$fillOpacity <- fillOpacity
    class(ob) <- "svis_layer"
    ob
}

##' as.svis_points
##'
##' @return A svis_points object
##' @param x another object to be coerced into a svis_points
##' @param ... Other arguments
as.svis_points <- function(x, ...) UseMethod("as.svis_points")

##' as.svis_points.svis_layer
##'
##' @param x A svis_layer
##' @param ... Other arguments
##' @return svis_points
as.svis_points.svis_layer <- function(x, ...) {
    x[[1]]
}

##' data_name
##'
##' @param x A svis_layer
##' @return name of the data in js
data_name <- function(x) {
    stopifnot(class(x) == "svis_layer")
    paste0("data_", gsub(" ", "_", attributes(x)$layer_title))
}

##' layer_name
##'
##' @param x A svis_layer
##' @return name of the layer in js
layer_name <- function(x) {
    stopifnot(class(x) == "svis_layer")
    paste0("layer_", gsub(" ", "_", attributes(x)$layer_title))
}

##' colfunction
##'
##' @param x A svis_layer
##' @return The name of the getcolour function in the js for the fill colour
colfunction <- function(x) {
    stopifnot(class(x) == "svis_layer")
    paste0(layer_name(x), "_getfillColor")
}

##' jsbyvar
##'
##' @param x A svis_layer
##' @return The name of the byvar in .js
jsbyvar <- function(x) {
    stopifnot(class(x) == "svis_layer")
    if(is.null(attributes(x)$byvar)) return("feature.id")
    paste0("feature.properties.", attributes(x)$byvar)
}

##' radius
##'
##' @param x A svis_layer
##' @return The radius of the points or the name of the variable in
##'     the data to use to determine the radius
radius <- function(x) {
    stopifnot(class(x) == "svis_layer")
    return(5)
}

##' outline_color
##'
##' @param x A svis_layer
##' @return The outline colour of the points or the name of the variable in
##'     the data to use to determine the outline colour
outline_color <- function(x) {
    stopifnot(class(x) == "svis_layer")
    return(shQuote("#000000"))
}

##' outline_opacity
##'
##' @param x A svis_layer
##' @return The outline opacity of the points or the name of the variable in
##'     the data to use to determine the outline opacity
outline_opacity <- function(x) {
    stopifnot(class(x) == "svis_layer")
    return("1")
}

##' opacity
##'
##' @param x A svis_layer
##' @return The opacity of the points of the name of the variable in
##'     the data to use to determine the opacity
opacity <- function(x) {
    stopifnot(class(x) == "svis_layer")
    return("1")
}

##' format.svis_layer
##'
##' @param x svis_layer
##' @param ... Other arguments
##' @export
format.svis_layer <- function(x, ...){

    ## The colour of the points
    getcol <- fillColor_js(layername = layer_name(x),
                           values = attributes(x)$bylabs,
                           col = attributes(x)$col)$content

    data_load <- format(as.svis_points(x), name = data_name(x))

    ## Define the layer
    call_layer_part <- c(paste("var", layer_name(x), "= new L.LayerGroup();"),
                         paste0("L.geoJson(", data_name(x), ",{"),
                         "style: function (feature) {",
                         "return feature.properties && feature.properties.style;",
                         "},",
                         paste(c("onEachFeature: ", onEachFeature(), ","), collapse = ""),
                         "pointToLayer: function (feature, latlng) {",
                         "return L.circleMarker(latlng, {",
                         paste0("radius: ", radius(x),  ","),
                         paste0("fillColor: ", colfunction(x), "(", jsbyvar(x), "),"),
                         paste0("color: ", outline_color(x), ","),
                         paste0("weight: ", "1", ","),
                         paste0("opacity: ", outline_opacity(x), ","),
                         paste0("fillOpacity: ", opacity(x), ","),
                         "});",
                         "}",
                         paste0("}).addTo(", layer_name(x), ");"))

    feature_function <- onEachFeature()

    list(getcol, data_load, feature_function, call_layer_part)
}

##' print.svis_layer
##'
##' @param x A svis_layer
##' @param ... Other arguments
##' @export
##' @return NULL
print.svis_layer <- function(x, ...) {
    lapply(format(x), function(y){
        cat(y)
    })
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
