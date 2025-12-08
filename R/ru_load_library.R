#' Load a list of libraries
#'
#' Given a list of packages, check if installed and generate message, otherwise load package.
#'
#' @param pkgs A list of packages to check if installed and then load.
#'
#' @return No return value, packages are loaded.
#'
#' @author Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com} \cr
#'         Chris Rook, \email{cr883296@gmail.com}
#'
#' @examples
#' library(repfun)
#' repfun::ru_load_library(c("dplyr", "haven", "magrittr", "r2rtf"))
#'
#' @export
#'
ru_load_library <- function(pkgs) {
  for (i in 1:length(pkgs)) {
    if (!requireNamespace(pkgs[i], quietly = TRUE)) {
      stop(paste("Package", pkgs[i], "is not installed. Please install it first."))
    }
    if (!pkgs[i] %in% loadedNamespaces()) {
      library(pkgs[i], character.only = TRUE)
    }
  }
  return(invisible(NULL))
}
