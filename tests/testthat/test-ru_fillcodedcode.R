test_that("populating a decode variable works", {

  suppressMessages(library(repfun))
  suppressMessages(library(dplyr))
  suppressMessages(library(testthat))

  #========================================
  # Create decode for SEX using function.
  #========================================
  fmtlist <- list('SEXS'=list('START'=list('M','F'), 'LABEL'=c('Male','Female')))
  adsl <- repfun::adsl
  prod <- repfun::ru_fillcodedcode(adsl, codedecodevarpairs=c("SEX", "SEXDCD"), varcodelistpairs=c("SEX", "SEXS"), codelistnames=fmtlist) %>% dplyr::arrange(USUBJID)

  #=================================
  # Create decode for SEX manually.
  #=================================
  adsl %>% dplyr::mutate(SEXDCD=ifelse(SEX=='F','Female',ifelse(SEX=='M','Male',NA))) %>% dplyr::arrange(USUBJID) -> qc

  #===========
  # Clean up.
  #===========
  prod <- as.data.frame(prod[,c('SEX','SEXDCD')])
  qc <- as.data.frame(qc[,c('SEX','SEXDCD')])
  qc <- repfun::ru_labels(qc,varlabels=list('SEX'='Sex','SEXDCD'='Sex Decode'))

  attr(prod, '_xportr.df_arg_') <- NULL
  attr(prod, 'label') <- 'Subject Level Analysis'
  attr(qc, '_xportr.df_arg_') <- NULL
  prod <- repfun::ru_labels(prod,varlabels=list('SEX'='Sex','SEXDCD'='Sex Decode'))

  #==========================
  # Compare for differences.
  #==========================
  testthat::expect_equal(prod, qc)
})
