#' @title Export the R code of a specific case
#'
#' @description
#' \code{source.case} exports the R code of a given case to output
#'
#' @export
source.case <- function(case, output=NA, documentation=0) {

  filename_case <- paste0(path.package("data.r"), "/", case, ".Rmd")
  if(is.na(output)) {
    filename_out <- paste0(getwd(), "/", case, ".R")
  } else { filename_out <- output }
  knitr::purl(filename_case, filename_out, documentation=documentation)

}
