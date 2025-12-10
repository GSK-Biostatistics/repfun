#' Copy package data to the directory specified.
#'
#' Copy all package data into a temporary directory that can be used when running examples.
#'
#' @param p A path as a string.
#'
#' @return 'NULL' because package dataframes are copied to a folder.
#'
#' @author Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com} \cr
#'         Chris Rook, \email{cr883296@gmail.com}
#'
#' @examples
#' library(repfun)
#' copydata(tempdir())
#'
#' @export
#'
copydata <- function(p){

  #============
  # Read data.
  #============
  adae <- repfun::adae
  adsl <- repfun::adsl
  advs <- repfun::advs
  ae <- repfun::ae
  suppae <- repfun::suppae
  dm <- repfun::dm
  suppdm <- repfun::suppdm
  airquality_4test <- repfun::airquality_4test
  airquality_updated <- repfun::airquality_updated
  mtcars_w2lbls <- repfun::mtcars_w2lbls
  formats <- repfun::formats

  #============
  # Copy data.
  #============
  save(adae, file = paste0(p,"/adae.rda"))
  save(adsl, file = paste0(p,"/adsl.rda"))
  save(advs, file = paste0(p,"/advs.rda"))
  save(ae, file = paste0(p,"/ae.rda"))
  save(suppae, file = paste0(p,"/suppae.rda"))
  save(dm, file = paste0(p,"/dm.rda"))
  save(suppdm, file = paste0(p,"/suppdm.rda"))
  save(airquality_4test, file = paste0(p,"/airquality_4test.rda"))
  save(airquality_updated, file = paste0(p,"/airquality_updated.rda"))
  save(mtcars_w2lbls, file = paste0(p,"/mtcars_w2lbls.rda"))
  save(formats, file = paste0(p,"/formats.rda"))

  return(invisible(NULL))
}
