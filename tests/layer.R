library(svis)

## Page with a single layer
pts <- sample_data()
pts@data$Djurslag <- as.factor(pts@data$Djurslag)
pts@data$popup_text <- pts@data$Djurslag
layer1 <- layer(pts, layer_title = "CWD", byvar = "Djurslag")
header <- page_header()
body <- page_body(map_div(layer1))
page <- page_page(header, body)


## Page with two layers

pts2 <- sample_data("asf")
pts2@data$Provtagningsorsak <- as.factor(pts2@data$Provtagningsorsak)
cols <- terrain.colors(nlevels(pts2@data$Provtagningsorsak))
pts2@data$popup_text <- pts2@data$Provtagningsorsak
layer2 <- layer(pts2, layer_title = "ASF", byvar = "Provtagningsorsak", col = cols)
layersob <- layers(list(layer1, layer2))
header <- page_header()
body <- page_body(map_div(layersob))
page <- page_page(header, body)

page_file <- tempfile(fileext = ".html")
capture.output(page, file = page_file)
browseURL(page_file)
