##' Overlay functions
##' @param object A svis_layer or svis_layers object
##' @return character the overlay string
overlays <- function(object) UseMethod("overlays")

##' overlays.svis_layer
##'
##' @param object a svis_layer
##' @return chracter the overlay string
overlays.svis_layer <- function(object) {
    object <- paste0("'", object$title, "': ", object$name)
    paste0("var overlays = {", object, "};")
}

##' overlays.svis_layers
##'
##' @param object a svis_layers object
##' @return html snippet
overlays.svis_layers <- function(object) {
    layers <- lapply(object, function(x){
        stopifnot("svis_layer" %in% class(x))
        paste0("'", x$title, "': ", x$name)
    })
    layers <- paste(layers, collapse = ",")
    paste0("var overlays = {", layers, "};")
}

##' map_div
##'
##' @param layers A layer or a list of layers
##' @param layer_states a logical vector indicating the default on/off
##'     state of each layer
##' @param center The center of the map
##' @param zoom The default zoom level
##' @param maxBounds The max navigable bounds of the map. Must be a
##'     list of length 2 with the bottom left corner and top right
##'     corner.
##' @import hlt
##' @export
##' @return A svis_div object
map_div <- function(layers,
                    layer_states = TRUE,
                    center = c(63.0, 17.5),
                    zoom = 5,
                    maxBounds = list(c(54, 4), c(70, 31))) {

    ## Check maxbound format
    stopifnot(identical(class(maxBounds), "list"))
    stopifnot(identical(length(maxBounds), 2L))
    lapply(maxBounds, function(x){stopifnot(identical(class(x), "numeric"))})
    lapply(maxBounds, function(x){stopifnot(identical(length(x), 2L))})

    ## The layer scripts
    layerssnippet <- scripts(layers)

    ## Which layer to turn on and off at the default state
    stopifnot(identical(class(layer_states), "logical"))
    stopifnot(length(layer_states) == 1 | length(layer_states) == length(names(layers)))
    layer_states <- paste(names(layers)[layer_states], collapse = ", ")

    ## Format the center
    center <- paste(center, collapse = ", ")

    ## Format the boundaries
    maxBounds <- paste0("[[", paste(lapply(maxBounds, paste, collapse = ", "), collapse = "], ["), "]]")

    ## Format the map script
    map <- c("var map = L.map('map', {",
             paste0("center: [", center, "],"),
             paste0("zoom: ", zoom, ","),
             paste0("maxBounds: ", maxBounds, ","),
             paste(c("layers: [streets, ", layer_states, "]"), collapse = ""),
             "});",
             "L.control.layers(baseLayers, overlays, {collapsed:false}).addTo(map);")

    ## Bundle these pieces into an html_div
    object <- html_div(c(layerssnippet, list(html_script(c(overlays(layers), map)))), id = "map")
    class(object) <- c(class(object), "svis_div")
    object
}
