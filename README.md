# svis

This is an R package that produces a leaflet map. Currently you can
produce a point map with an arbitrary number of point layers.

## Installation

Use the remotes package to install the package and its dependency from github list this:

```{r}
library("remotes")
install_github("trosendal/hlt")
install_github("trosendal/svis")
```

## Usage

Have a look at the vignette like this:

```{r}
library(svis)
vignette("point_map", package = "svis")
```
