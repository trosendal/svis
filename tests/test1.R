library(svis)

## Just load the sample data
pts <- sample_data()
stopifnot(identical(class(pts), structure("SpatialPointsDataFrame", package = "sp")))
rm(list = ls())

## Assert names check
df <- svis:::read_sample_data_cwd()
foo <- tools::assertError(svis:::convert_to_sppts(df,
                                                  svis:::RT90(),
                                                  svis:::WGS84(),
                                                  long = "foo",
                                                  lat = "bar"))
stopifnot(identical(foo[[1]]$message, "c(lat, long) %in% names(df) are not all TRUE"))
rm(list = ls())

## Assert warning on missing spatial references in convert to sp::object
df <- svis:::read_sample_data_cwd()
df$Gisx[1] <- NA
res <- tools:::assertWarning(svis:::convert_to_sppts(df,
                                                     svis:::RT90(),
                                                     svis:::WGS84(),
                                                     long = "Gisy",
                                                     lat = "Gisx"))
stopifnot(length(grep("1 of the submitted points are missing coordinates and will be discarded",
                     res[[1]]$message)) > 0)
rm(list = ls())

## Create a point layer

pts <- sample_data()
stopifnot(identical(class(layer(pts)), "svis_layer"))

## Make a list of those layers

layer1 <- layer(pts)
layer2 <- layer(pts, layer_title = "layer2")
layers(list(layer1, layer2))

## Assert that you can't submit two layers with the same name
layer3 <- layer(pts, layer_title = "layer2")
foo <- tools::assertError(layers(list(layer2, layer3)))
stopifnot(identical(foo[[1]]$message, "all(!duplicated(lapply(list_of_layers, \"[\", \"name\"))) is not TRUE"))
## Assert that you must submitt a list to layers

foo <- tools::assertError(layers(layer1))
stopifnot(identical(foo[[1]]$message, "identical(class(list_of_layers), \"list\") is not TRUE"))

## Assert that you must submitt only svis_layer class objects to layers

class(layer3) <- "foo"
foo <- tools::assertError(layers(list(layer1, layer3)))
stopifnot(identical(foo[[1]]$message, "identical(class(x), \"svis_layer\") is not TRUE"))
rm(list = ls())

## Check names function of layer and layers

pts <- sample_data()

layer1 <- layer(pts)
layer2 <- layer(pts, layer_title = "layer2")
layersob <- layers(list(layer1, layer2))

stopifnot(identical(names(layer1), "layer_layer1"))
stopifnot(identical(names(layer2), "layer_layer2"))
stopifnot(identical(names(layersob), c("layer_layer1", "layer_layer2")))

## Check the scripts function
stopifnot(length(scripts(layer1)) == 3)
stopifnot(length(scripts(layersob)) == 6)
rm(list = ls())

## Check the building of the overlay call
pts <- sample_data()

layer1 <- layer(pts)
layer2 <- layer(pts, layer_title = "layer2")
layersob <- layers(list(layer1, layer2))

svis:::overlays.svis_layer(layer1)
svis:::overlays.svis_layers(layersob)

## Build a map div with 1 or 2 layers

header <- page_header()
body <- page_body(map_div(layer1))
page <- page_page(header, body)

body <- page_body(map_div(layersob))
page <- page_page(header, body)

## Get the asf data.
str(sample_data("asf"))
