#' @title Open a specific case
#'
#' @description
#' \code{data.case} opens the Rmd file of a given case within RStudio
#'
#' @export
data.case <- function(case) {
  
  filename_case <- paste0(path.package("data.r"), "/", case, ".Rmd")
  if (file.exists(filename_case)) {
    file.edit(filename_case)
  } else {
    warning(paste("Case", case, "is not available!"))
  }
  
}
