test_that("adding big N works", {

  suppressMessages(library(repfun))
  suppressMessages(library(dplyr))
  suppressMessages(library(testthat))
  suppressMessages(library(tibble))

  repfun::setpath(paste0(rfenv$PATH,'/tests/testthat'))

  #=======
  # SETUP
  #=======
  suppressMessages(require(dplyr))
  options(dplyr.summarise.inform = FALSE)
  repfun::rs_setup(D_POPDATA=repfun::adsl %>% dplyr::filter(SAFFL =='Y'), D_SUBJID=c("STUDYID","USUBJID"), R_ADAMDATA=".")
  rfenv$G_POPDATA %>% dplyr::mutate(TRT01AN=ifelse(TRT01A=='Placebo',1,ifelse(TRT01A=='Xanomeline Low Dose',2,3))) %>%
    dplyr::select(STUDYID,USUBJID,SAFFL,TRT01AN,TRT01A) -> G_POPDATA
  attr(G_POPDATA$TRT01AN,"label") <- 'Actual Treatment for Period 01 (n)'

  #inadae <- adamdata$adae.rda()
  inadae <- do.call(rfenv$adamdata$adae.rda,list())
  inadae <- tibble::as_tibble(as.data.frame(inadae))
  adae <- inadae %>% dplyr::inner_join(G_POPDATA, by=c('STUDYID','USUBJID','SAFFL','TRT01A')) %>% dplyr::filter(TRTEMFL=='Y')

  #=======
  # PROD
  #=======
  addbign <- repfun::ru_addbignvar(adae,
                           G_POPDATA,
                           groupbyvars=c("TRT01AN", "TRT01A"),
                           countdistinctvars=c("STUDYID", "USUBJID"),
                           totalforvar=c("TRT01AN"),
                           totalid = 99,
                           totaldecode = 'Total',
                           codedecodevarpairs=c("TRT01AN", "TRT01A"),
                           varcodelistpairs=c(""),
                           codelistnames=list(),
                           addbigntovarvalue=TRUE,
                           splitchar="~") %>% dplyr::select(STUDYID, USUBJID, TRT01AN, TRT01A, AEBODSYS, AEDECOD) %>%
                                          dplyr::arrange(STUDYID, USUBJID, TRT01AN, TRT01A, AEBODSYS, AEDECOD)

  #=====
  # QC
  #=====
  qcbign <- G_POPDATA %>% dplyr::group_by(TRT01AN,TRT01A) %>% dplyr::summarize(DENOMCNT=n()) %>% dplyr::ungroup() %>%
    dplyr::select(TRT01AN,TRT01A,DENOMCNT) %>% dplyr::mutate(TRT01A=paste0(TRT01A,'~(N=',as.character(DENOMCNT),')')) %>% dplyr::select(-DENOMCNT)
  qcadae <- adae %>% select(-c('TRT01A')) %>% left_join(qcbign, by=c("TRT01AN"))
  attr(G_POPDATA$TRT01AN,"label") <- 'Actual Treatment for Period 01'
  qcadae <- as.data.frame(qcadae) %>% select(STUDYID, USUBJID, TRT01AN, TRT01A, AEBODSYS, AEDECOD)
  qcadae <- repfun::ru_labels(qcadae,varlabels=lapply(addbign, attr, "label"))
  qcadae <- qcadae %>% dplyr::arrange(STUDYID, USUBJID, TRT01AN, TRT01A, AEBODSYS, AEDECOD)

  #==========
  # Compare
  #==========
  attr(addbign, '_xportr.df_arg_') <- NULL
  attr(qcadae, '_xportr.df_arg_') <- NULL
  attr(addbign, 'label') <- NULL
  attr(qcadae, 'label') <- NULL

  testthat::expect_equal(addbign, qcadae)
})
