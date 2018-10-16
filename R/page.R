##' page_page
##'
##' @param header an html header
##' @param body an html body
##' @return an hlt page
##' @export
##' @import hlt
##' @examples
##' \dontrun{
##'
##' library(svis)
##'
##' # Get the sample data
##' pts <- sample_data()
##' a <- convert_to_geojson(pts)
##'
##' # Convert to layer objects with 2 different names:
##' layer1 <- point_layer(a)
##' layer2 <- point_layer(a, layer_title = "layer2")
##'
##' # Make a layers object with both layers
##' layersob <- layers(list(layer1, layer2))
##'
##' # Generate the page header
##' header <- page_header()
##'
##' # Map with a single layer:
##' body <- page_body(map_div(layer1))
##' page <- page_page(header, body)
##' map <- tempfile(fileext = ".html")
##' capture.output(page, file = map)
##' browseURL(map)
##'
##' # Map with 2 layers:
##'
##' body <- page_body(map_div(layersob))
##' page <- page_page(header, body)
##' map <- tempfile(fileext = ".html")
##' capture.output(page, file = map)
##' browseURL(map)
##' }

page_page <- function(header, body) {
    html_html(header +
              body)
}

##' page_body
##'
##' @param div a svis_div object
##' @return an hlt body
##' @export
##' @import hlt
page_body <- function(div) {
    stopifnot("svis_div" %in% class(div))
    html_body(div)
}

##' page_header
##'
##' Create some of the content in the header of the webpage with the
##' leaflet map
##'
##' @import hlt
##' @export
##' @return An hlt object with the page header
##' @param title The title of the page
page_header <- function(title = "map") {

    ## Title of the page
    header  <- html_title(as.character(title)) +

    ## The meta tags to make the map work in old IE and declare the
    ## page encoding
    html_meta(charset = "utf-8") +
    html_meta("http-equiv" = "x-ua-compatible") +
    html_meta(content = "IE=edge") +
    html_meta(name="viewport") +
    html_meta(content="width = device-width, initial-scale = 1.0") +

    ## The leaflet library
    leaflet_css() +
    leaflet_js() +

    ## The sva css
    html_link(rel = "stylesheet",
              href = "https://www.sva.se/assets/css/main.css") +

    ## The leaflet maps tiles and stuff
    tiles() +
    css()
    ## Put everyting in a header tag
    html_head(header)
}
