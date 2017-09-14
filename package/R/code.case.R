#' @title Open a specific code case
#'
#' @description
#' \code{code.case} opens the R script of a given code case within RStudio
#'
#' @export
code.case <- function(case) {
  
  filename_case <- paste0(path.package("data.r"), "/", case, ".R")
  if (file.exists(filename_case)) {
    file.edit(filename_case)
  } else {
    warning(paste("Case", case, "is not available!"))
  }
  
}
