test_that("combining SDTM with its supplemental data works", {

  suppressMessages(library(repfun))
  suppressMessages(library(dplyr))
  suppressMessages(library(tidyr))
  suppressMessages(library(testthat))

  repfun::setpath(paste0(rfenv$PATH,'/tests/testthat'))

  ######
  # DM #
  ######

  #=============================================================
  # Create prod dmsupp data frame using ru_addsupp() function.
  #=============================================================
  sdtmdata <- repfun::ru_libname('../../data')
  sdtm.dm <- sdtmdata$dm()
  sdtm.suppdm <- sdtmdata$suppdm()
  prod_dm <- repfun::ru_addsupp(dsetin=sdtm.dm,dsetinsupp=sdtm.suppdm)

  #=============================================================
  # Create qc dmsupp data frame using independent code.
  #=============================================================
  dm <- repfun::dm

  suppdm <- repfun::suppdm
  qlabs <- suppdm %>% dplyr::distinct(QNAM,QLABEL)
  tlabs <- as.character(qlabs$QLABEL)
  names(tlabs) <- qlabs$QNAM
  suppdm %>% dplyr::select(STUDYID, USUBJID, QNAM, QVAL) %>% tidyr::pivot_wider(names_from=QNAM, values_from=QVAL) -> suppdm_t

  vlabs = sapply(suppdm_t,function(x){attr(x,"label")})
  vlabs <- modifyList(as.list(vlabs),as.list(tlabs))
  suppdm_t <- repfun::ru_labels(suppdm_t,varlabels=vlabs,style='base')
  suppressMessages(dm %>% dplyr::full_join(suppdm_t)) -> qc_dm

  #=======================================
  # Compare prod and qc for SDTM DM data.
  #=======================================
  testthat::expect_equal(as.data.frame(prod_dm), as.data.frame(qc_dm))

  ######
  # AE #
  ######

  #=============================================================
  # Create prod aesupp data frame using ru_addsupp() function.
  #=============================================================
  sdtm.ae <- sdtmdata$ae()
  sdtm.suppae <- sdtmdata$suppae()
  prod_ae <- repfun::ru_addsupp(dsetin=sdtm.ae,dsetinsupp=sdtm.suppae)

  #=============================================================
  # Create qc aesupp data frame using independent code.
  #=============================================================
  ae <- repfun::ae

  suppae <- repfun::suppae
  qlabs <- suppae %>% dplyr::distinct(QNAM,QLABEL)
  tlabs <- as.character(qlabs$QLABEL)
  names(tlabs) <- qlabs$QNAM
  suppae %>% dplyr::select(STUDYID, USUBJID, IDVARVAL, QNAM, QVAL) %>% tidyr::pivot_wider(names_from=QNAM, values_from=QVAL) %>% dplyr::rename(AESEQ=IDVARVAL) %>% dplyr::mutate(AESEQ=as.numeric(AESEQ)) -> suppae_t
  vlabs = sapply(suppae_t,function(x){attr(x,"label")})
  vlabs['AESEQ'] <- 'Sequence Number'
  vlabs <- modifyList(as.list(vlabs),as.list(tlabs))
  suppae_t <- repfun::ru_labels(suppae_t,varlabels=vlabs,style='base')
  suppressMessages(ae %>% dplyr::full_join(suppae_t)) -> qc_ae

  #=======================================
  # Compare prod and qc for SDTM AE data.
  #=======================================
  testthat::expect_equal(as.data.frame(prod_ae), as.data.frame(qc_ae))

})
