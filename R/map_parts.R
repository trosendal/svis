##' tiles
##'
##' @import hlt
##' @return an hlt::html_script object
##' @param topo The name of the topo layer
##' @param streets The name of the streets layer
##' @param satelite The name of the satelite layer
tiles <- function(topo = "Terr\u00E4ng",
                  streets = "V\u00E4gkarta",
                  satelite = "Flygfoto") {
    sources <- c("var streets = L.tileLayer('https://tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png', {",
                 "    maxZoom: 18,",
                 "    attribution: '&copy; <a href=\"http://www.openstreetmap.org/copyright\">OpenStreetMap</a>'",
                 "}),",
                 "    topo = L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}', {",
                 "	maxZoom: 18,",
                 "	attribution: 'Tiles &copy; Esri &mdash; Esri, DeLorme, NAVTEQ, TomTom, Intermap, iPC, USGS, FAO, NPS, NRCAN, GeoBase, Kadaster NL, Ordnance Survey, Esri Japan, METI, Esri China (Hong Kong), and the GIS User Community'",
                 "    }),",
                 "    Esri_WorldImagery = L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}', {",
                 "	maxZoom: 16,",
                 "	attribution: 'Tiles &copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community'",
                 "    });")
    topo <- paste0(as.character(topo), ": topo,")
    streets <- paste0(as.character(streets), ": streets,")
    satelite <- paste0(as.character(satelite), ": Esri_WorldImagery,")
    map_call <- c("var baseLayers = {",
                  topo, streets, satelite,
                  "};")
    script <- paste(c(sources, map_call), collapse = "\n")
    html_script(script)
}

##' leaflet_css
##'
##' @import hlt
##' @return An html snippet
leaflet_css <- function() {
    html_link(rel = "stylesheet",
              href = "https://unpkg.com/leaflet@1.3.4/dist/leaflet.css",
              integrity = "sha512-puBpdR0798OZvTTbP4A8Ix/l+A4dHDD0DGqYW6RQ+9jxkRFclaxxQb/SJAWZfWAkuyeQUytO7+7N4QKrDh+drA==",
              crossorigin = "")
}

##' leaflet_js
##' @import hlt
##' @return An html snippet
leaflet_js <- function() {
    html_script(content = "",
                src = "https://unpkg.com/leaflet@1.3.4/dist/leaflet.js",
                integrity = "sha512-nMMmRyTVoLYqjP9hrbed9S+FzjZHW5gY1TWCHA5ckwXZBadntCNs8kEqAWdrb9O7rxbCaA4lKTIWjDXZxflOcA==",
                crossorigin = "")
}

##' onEachFeature
##'
##' @param name name of the function
##' @import hlt
##' @return A character vector
onEachFeature <- function(name = "onEachPoint") {
    c(paste0("function ", name, "(feature, layer) {"),
      "var popupContent = feature.properties.popup_text;",
      "layer.bindPopup(popupContent);",
      "}")
}

##' fillColor_js
##'
##' @param layername the name of the layer that the function should
##'     be applied
##' @param values The numeric values of the factor variable to
##'     consider
##' @param col The colours to match with each level
##' @param default_col The colour given to a point that doesn't meet
##'     any of the criteria
##' @importFrom grDevices rainbow
##' @return An hlt script object
fillColor_js <- function(layername,
                         values = NULL,
                         col = NULL,
                         default_col = "#000000") {
    stopifnot(identical(class(layername), "character"))
    ## First if there is no col or value specified, just give back a
    ## function that returns blue
    if(is.null(values) & is.null(col)) {
        js <- html_script(c(paste0("function ", layername, "_getfillColor(x) {"),
                            paste0("return '#00A9CE';}")))
        return(js)
    }
    stopifnot(identical(class(default_col), "character"))
    ## If there is a colour but no values
    if(is.null(values) & !is.null(col)) {
        stopifnot(identical(length(col), 1L))
        js <- html_script(c(paste0("function ", layername, "_getfillColor(x) {"),
                            paste0("return ", shQuote(col), ";}")))
        return(js)
    }
    stopifnot(class(values) %in% c("numeric", "integer"))
    ## if col is NULL generate colours with rainbow()
    if(is.null(col)) {
        col <- rainbow(length(values))
    }
    ## check that there are not more colours than values
    stopifnot(length(values) >= length(col))
    stopifnot(identical(length(default_col), 1L))
    func_name <- c(paste0("function ", layername, "_getfillColor(x) {"),
      "return(")
    parts <- mapply(function(x, y) {
        paste0("x == ", x, " ? ", shQuote(y), " :")
    }, values, col)
    default <- paste0(shQuote(default_col), ");}")
    html_script(c(func_name, parts, default))
}

##' maplegend
##' @param x a layer or layers object
##' @return An hlt script with a legend for the layer
maplegend <- function(object) UseMethod("maplegend")


##' maplegend
##'
##' @param x a svis_layer
##' @return an hlt script
maplegend.svis_layer <- function(x, position = "bottomright") {
    ob <- c(paste0("var ", x$name, "_legend = L.control({position: ", shQuote(position), "});"),
            paste0(x$name, "_legend.onAdd = function(map) {"),
            paste0("var div = L.DomUtil.create('div', 'info legend'),"),
            paste0("grades = ", paste0("[", paste(x$byvarnum, collapse = ","), "],")),
            paste0("labels = ", shQuote(x$title), ","),
            paste0("labs = [", paste(shQuote(x$bylabs), collapse = ","), "];"),
            paste0("var index;"),
            paste0("for (index = 0; index<", length(x$bylabs), "; ++index) {" ),
            paste0("value = grades[index];"),
            paste0("div.innerHTML +="),
            paste0("'<i><div class = \"circle\" style = \"background:'", " + ", x$colfunction, "(grades[index]) + '\"></div></i> ' + labs[index] + '<br>';"),
            paste0("}"),
            paste0("return div;"),
            paste0("};"),
            paste0(x$name, "_legend.addTo(map)"))
    html_script(ob)
}

##' maplegend
##'
##' @param x a svis_layers
##' @return an hlt script
maplegend.svis_layers <- function(x) {
    return("Not implemented")
}
