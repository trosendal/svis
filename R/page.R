##' page_header
##'
##' Create some of the content in the header of the webpage with the
##' leaflet map
##'
##' @title page_header
##' @import hlt
##' @export
##' @return An hlt object with the page header
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
    tiles()
    ## Put everyting in a header tag
    html_head(header)
}
