#' Set the current working directory.
#'
#' Set the current working directory so that relative paths work as expected.
#'
#' @param p A path as a string.
#'
#' @return No return value, the current working directory is set.
#'
#' @author Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com} \cr
#'         Chris Rook, \email{cr883296@gmail.com}
#'
#' @examples
#'\dontrun{
#' library(repfun)
#' repfun::setpath()
#'}
#' @export
#'
setpath <- function(p){
tryCatch(
  expr = {setwd(p)},
  error = function(e){tryCatch(expr = {setwd(rprojroot::find_root(rprojroot::is_rstudio_project))},
                               error = function(e){tryCatch(expr = {setwd(rfenv$PATH)},
                                                            error = {
                                                              message('Current Working Directory: ', getwd())
                                                              message('The Current Working Directory Has Not Been Set As Expected (An Issue). It Must Be Set By The User.')})})},
  finally = {
    #message(paste0('Current Working Directory is: ',getwd()))
    }
)
  return(invisible(NULL))
}
