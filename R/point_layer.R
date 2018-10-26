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
    ob <- list(data)

    ## Check the levels of the byvar
    df <- as.data.frame(data)
    if(!is.null(byvar)) {
        stopifnot(byvar %in% names(df))
        df[, byvar] <- as.factor(df[, byvar])
        ## this could be useful for the legend:
        attributes(ob)$bylabs <- levels(df[, byvar])
    }

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


##' format.svis_layer
##'
##' @param x svis_points
##' @param ... Other arguments
##' @export
format.svis_layer <- function(x, ...){

    ## Add the data
    data_name <- paste0("data_", gsub(" ", "_", attributes(x)$layer_title))
    layer_name <- paste0("layer_", gsub(" ", "_", attributes(x)$layer_title))

    ## Get colour function name in js
    colfunction <- paste0(attributes(x)$layer_name, "_getfillColor")

    ## The colour of the points
    getcol <- fillColor_js(layername = layer_name,
                           values = attributes(x)$bylabs,
                           col = col)

    data_load <-  format(data, name = data_name)

    jsbyvar <-

    jsbyvar <- paste0("feature.properties.", attributes(x)$byvar)
    if(is.null(attributes(x)$byvar)) jsbyvar <- "feature.id"

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
               paste0("fillColor: ", colfunction, "(", jsbyvar, "),"),
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
                   byvar = byvar,
                   bylabs = bylabs,
                   byvarnum = byvarnum,
                   colfunction = colfunction,
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
