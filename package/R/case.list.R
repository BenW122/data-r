#' @title Return a data.frame with all currently available cases
#'
#' @description
#' \code{case.list} returns a data.frame with all currently available cases
#' based on the .Rmd files in the /inst directory.
#'
#' @export
case.list <- function() {
  
  cases <- list.files(path.package("data.r"), "*.Rmd")
  case.list <- data.frame(unlist(strsplit(cases, ".Rmd")))
  names(case.list) <- "Case"
  return(case.list)
  
}
