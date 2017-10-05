#' @title Return a data.frame with all currently available code cases
#'
#' @description
#' \code{code.list} returns a data.frame with all currently available code cases
#' based on the .Rmd files in the /inst directory.
#'
#' @export
code.list <- function() {
  
  cases <- list.files(path.package("data.r"), ".*\\.R$")
  code.list <- data.frame(unlist(strsplit(cases, ".R")))
  names(code.list) <- "Code"
  return(code.list)
  
}
