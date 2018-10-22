## Check the fill color function
library(svis)
layer_name <- "foobar"
col_vector <- terrain.colors(3)
values_vector <- 1:3
default_colour  <-  "#000000"
## Expect these to work:
svis:::fillColor_js(layer_name,
                    values_vector,
                    col_vector,
                    default_colour)
svis:::fillColor_js(layer_name,
                    NULL,
                    "#000000",
                    default_colour)
svis:::fillColor_js(layer_name,
                    values_vector,
                    NULL,
                    default_colour)
svis:::fillColor_js(layer_name,
                    NULL,
                    NULL,
                    default_colour)
svis:::fillColor_js(layer_name,
                    NULL,
                    NULL,
                    NULL)
## Expect one colour of you don't give values
ob <- tools::assertError(svis:::fillColor_js(layer_name,
                                             NULL,
                                             col_vector,
                                             default_colour))[[2]]$message
stopifnot(identical(ob, "identical(length(col), 1L) is not TRUE"))
## Expect a layer_name
ob <- tools::assertError(svis:::fillColor_js(NULL,
                                             values_vector,
                                             col_vector,
                                             default_colour))[[2]]$message
stopifnot(identical(ob, "identical(class(layername), \"character\") is not TRUE"))
