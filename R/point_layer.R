##' point_layer
##'
##' @param jsondata Name of the .js file and the name of the variable
##'     in the file
##' @param layer_title The human readable name of the layer in the map
##' @param radius Radius of the points (maybe a .js function call)
##' @param fillColor The colour to fill the points (maybe a .js
##'     function call)
##' @param color The border colour of the points (maybe a .js function
##'     call)
##' @param weight The weight, actually I don't know what this
##'     parameter does but.. (maybe a .js function call)
##' @param opacity The opacity of the border of the points (maybe a
##'     .js function call)
##' @param fillOpacity The opacity of the fill (maybe a .js function
##'     call)
##' @param onEach The name of the function to call on each feature
##' @import hlt
##' @export
##' @return A svis_layer
point_layer <- function(jsondata,
                        layer_title = "layer1",
                        radius = 5,
                        fillColor = shQuote("#00A9CE"),
                        color = shQuote("black"),
                        weight = 1,
                        opacity = 1,
                        fillOpacity = 1,
                        onEach = onEachFeature()){

    ## Add the data
    data_name <- paste0("data_", gsub(" ", "_", layer_title))
    layer_name <- paste0("layer_", gsub(" ", "_", layer_title))

    data_load <-  html_script(c(paste(data_name, "="),
                           jsondata))

    ## Define the layer
    layer <- c(paste("var", layer_name, "= new L.LayerGroup();"),
               paste0("L.geoJson(", data_name, ",{"),
               "style: function (feature) {",
               "return feature.properties && feature.properties.style;",
               "},",
               paste(c("onEachFeature: ", onEach, ","), collapse = ""),
               "pointToLayer: function (feature, latlng) {",
               "return L.circleMarker(latlng, {",
               paste0("radius: ", as.character(radius),  ","),
               paste0("fillColor: ",as.character(fillColor), ","),
               paste0("color: ", as.character(color), ","),
               paste0("weight: ", as.character(weight), ","),
               paste0("opacity: ", as.character(opacity), ","),
               paste0("fillOpacity: ", as.character(fillOpacity), ","),
               "});",
               "}",
               paste0("}).addTo(", layer_name, ");"))
    script <- list(data_load, html_script(c(onEach, layer)))
    object <- list(name = layer_name,
                   title = layer_title,
                   script = script)
    class(object) <- "svis_layer"
    object
}
