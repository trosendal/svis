##' css
##'
##' @import hlt
##' @return An hlt_style element
css <- function() {

    ## Item 1
    i1 <- c("\u0023map {width: 100%; height: 98vh; float: left;}")

    ## Item 2
    i2 <- c(".info {padding: 6px 8px;",
            "font: 14px/16px Arial, Helvetica, sans-serif;",
            "background: rgba(255,255,255,0.8);",
            "box-shadow: 0 0 15px rgba(0,0,0,0.2);",
            "border-radius: 5px;}")

    ## Item 3
    i3 <- c(".info h4 {margin: 0 0 5px; color: #777;}")

    ## Items for legend
    i4 <- c(".legend {",
            "    text-align: left;",
            "    line-height: 18px;",
            "    color: #555;",
            "       }")

    i5 <- c(".legend i {",
            "    width: 18px;",
            "    height: 18px;",
            "    float: left;",
            "    margin-right: 8px;",
            "    opacity: 0.9;",
            "}")

    i6 <- c(".legend .circle {",
            "   border-radius: 50%;",
            "    width: 10px;",
            "    height: 10px;",
            "    margin-top: 8px;",
            "    border: 1px solid black;",
            "}")

    i7 <- c(".icon {",
            "    border-radius: 0%;",
            "    margin-right: -10px;",
            "    margin-left: -12px;",
            "    margin-top: -1px;",
            "    margin-bottom: 15px;",
            "    width: 150%;",
            "    height: 100%;",
            "    text-align: center;",
            "}")
    html_style(c(i1, i2, i3, i4, i5, i6, i7))
}
