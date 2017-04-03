#' @title Open a specific case
#'
#' @description
#' \code{data.case} opens the Rmd file of a given case within RStudio
#'
#' @export
data.case <- function(case) {
  
  file.edit(paste0(path.package("data.r"), "/", case, ".Rmd"))
  
}