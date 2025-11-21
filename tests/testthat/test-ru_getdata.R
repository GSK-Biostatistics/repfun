test_that("get data function works", {

  suppressMessages(library(repfun))
  suppressMessages(library(dplyr))
  suppressMessages(library(testthat))

  repfun::setpath(paste0(rfenv$PATH,'/tests/testthat'))

  #=========================
  # Set up the environment.
  #=========================
  repfun::rs_setup(D_POP="SAFFL",D_POPLBL="Safety",D_POPDATA=repfun::adsl, D_SUBJID=c("STUDYID","USUBJID"), R_ADAMDATA=".")
  rfenv$G_POPDATA %>% dplyr::mutate(TRT01AN=ifelse(TRT01A=='Placebo',1,ifelse(TRT01A=='Xanomeline Low Dose',2,3)),
                       SAFFL=ifelse((row_number() %% 10) == 0,'N',SAFFL)) %>%
    repfun::ru_labels(varlabels=list('TRT01AN'='Actual Treatment for Period 01 (n)','SAFFL'='Safety Population Flag')) -> G_POPDATA

  #==================================
  # Invoke getdata function on ADAE.
  #==================================
  adae <- rfenv$adamdata$adae.rda() %>% dplyr::select(-c('SAFFL','TRT01A')) %>% repfun::ru_getdata(G_POPDATA, c("STUDYID", "USUBJID"), keeppopvars=c("TRT01AN", "TRT01A")) %>%
    dplyr::select(STUDYID,USUBJID,AEBODSYS,AEDECOD,SAFFL,TRT01AN,TRT01A) %>% dplyr::arrange(STUDYID,USUBJID,AEBODSYS,AEDECOD,SAFFL,TRT01AN,TRT01A)

  #================================
  # Manually apply the population.
  #================================
  pop <- G_POPDATA %>% select(STUDYID,USUBJID,TRT01AN,TRT01A,SAFFL) %>% dplyr::filter(SAFFL=='Y')
  adaeqc <- rfenv$adamdata$adae.rda() %>% dplyr::select(-c('SAFFL','TRT01A')) %>% dplyr::inner_join(pop,by=c('STUDYID','USUBJID')) %>%
    dplyr::select(STUDYID,USUBJID,AEBODSYS,AEDECOD,SAFFL,TRT01AN,TRT01A) %>% dplyr::arrange(STUDYID,USUBJID,AEBODSYS,AEDECOD,SAFFL,TRT01AN,TRT01A)
  adaeqc <- as.data.frame(adaeqc)
  attr(adaeqc, '_xportr.df_arg_') <- NULL
  attr(adaeqc,'label') <- NULL

  #======================
  # Compare the results.
  #======================
  testthat::expect_equal(adae,adaeqc)
})
