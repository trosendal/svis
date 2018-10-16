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
##' @import hlt
##' @return html snippet
overlays.svis_layers <- function(object) {
    layers <- lapply(object, function(x){
        stopifnot("svis_layer" %in% class(x))
        paste0("'", x$title, "': ", x$name)
    })
    layers <- paste(layers, collapse = ",")
    paste0("var overlays = {", layers, "};")
}
