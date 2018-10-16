##' sample_data
##'
##' Some dummy sample data to use for a point map (or whatever)
##' @export
##' @importFrom utils read.csv2
##' @return a data.frame
##' @examples
##' \dontrun{
##' # Get the data
##' df <- sample_data()
##' str(df)
##' }
sample_data <- function() {
    path <- system.file("extdata/sample_data_cwd.csv",
                        package = "svis")
    read.csv2(path,
              header = TRUE,
              stringsAsFactors = FALSE,
              encoding = "UTF-8")
}
