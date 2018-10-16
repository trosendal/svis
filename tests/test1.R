library(svis)

## Just load the sample data
pts <- sample_data()
stopifnot(identical(class(pts), structure("SpatialPointsDataFrame", package = "sp")))
rm(list = ls())

## Assert names check
df <- svis:::read_sample_data()
tools::assertError(svis:::convert_to_sppts(df,
                                           svis:::RT90(),
                                           svis:::WGS84(),
                                           long = "foo",
                                           lat = "bar"))
rm(list = ls())

## Assert warning on missing spatial references
df <- svis:::read_sample_data()
df$Gisx[1] <- NA
res <- tools:::assertWarning(svis:::convert_to_sppts(df,
                                                     svis:::RT90(),
                                                     svis:::WGS84(),
                                                     long = "Gisy",
                                                     lat = "Gisx"))
stopifnot(length(grep("1 of the submitted points are missing coordinates and will be discarded",
                     res[[1]]$message)) > 0)
rm(list = ls())
