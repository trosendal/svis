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
    sources <- c("var streets = L.tileLayer('http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png', {",
                 "    maxZoom: 18,",
                 "    attribution: '&copy; <a href=\"http://www.openstreetmap.org/copyright\">OpenStreetMap</a>'",
                 "}),",
                 "    topo = L.tileLayer('http://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}', {",
                 "	maxZoom: 18,",
                 "	attribution: 'Tiles &copy; Esri &mdash; Esri, DeLorme, NAVTEQ, TomTom, Intermap, iPC, USGS, FAO, NPS, NRCAN, GeoBase, Kadaster NL, Ordnance Survey, Esri Japan, METI, Esri China (Hong Kong), and the GIS User Community'",
                 "    }),",
                 "    Esri_WorldImagery = L.tileLayer('http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}', {",
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
