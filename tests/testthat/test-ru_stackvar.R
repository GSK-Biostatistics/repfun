test_that("stacking variables works", {

  suppressMessages(library(repfun))
  suppressMessages(library(dplyr))
  suppressMessages(library(tibble))
  suppressMessages(library(testthat))

  #=====================
  # Set up environment.
  #=====================
  rs_setup(D_POPDATA=repfun::adsl %>% dplyr::filter(SAFFL =='Y'), D_SUBJID=c("STUDYID","USUBJID"), R_ADAMDATA=".")
  rfenv$G_POPDATA %>% dplyr::mutate(TRT01AN=ifelse(TRT01A=='Placebo',1,ifelse(TRT01A=='Xanomeline Low Dose',2,3))) %>%
    repfun::ru_labels(varlabels=list('TRT01AN'='Actual Treatment for Period 01 (n)')) -> G_POPDATA
  adae <- tibble::as_tibble(rfenv$adamdata$adae.rda()) %>% dplyr::inner_join(G_POPDATA, by=c('STUDYID','USUBJID','SAFFL','TRT01A'))
  aesum_p <- repfun::ru_freq(adae,
                     dsetindenom=G_POPDATA,
                     countdistinctvars=c('STUDYID','USUBJID'),
                     groupbyvarsnumer=c('TRT01AN','TRT01A','AEBODSYS','AEDECOD'),
                     anyeventvars = c('AEBODSYS','AEDECOD'),
                     anyeventvalues = c('ANY EVENT','ANY EVENT'),
                     groupbyvarsdenom=c('TRT01AN'),
                     resultstyle="NUMERPCT",
                     totalforvar=c('TRT01AN'),
                     totalid=99,
                     totaldecode='Total',
                     codedecodevarpairs=c("TRT01AN", "TRT01A"),
                     varcodelistpairs=c(""),
                     codelistnames=list(),
                     resultpctdps=0) %>% repfun::ru_denorm(varstodenorm=c("tt_result", "PERCENT"),
                                                   groupbyvars=c("tt_summarylevel", "AEBODSYS", "AEDECOD"),
                                                   acrossvar="TRT01AN", acrossvarlabel="TRT01A",
                                                   acrossvarprefix=c("tt_ac", "tt_p")) %>%
                                         dplyr::mutate(ord1=ifelse(tt_summarylevel==0,0,1)) %>% dplyr::rename(ord2=tt_summarylevel) %>%
                                         dplyr::arrange(ord1,AEBODSYS,ord2,AEDECOD) %>% dplyr::select(-c(starts_with('tt_p'),starts_with('ord')))

  #=======
  # PROD
  #=======
  prod <- repfun::ru_stackvar(dsetin=aesum_p,varsin=c('AEBODSYS','AEDECOD'),varout='SYSPREF',varlabel='Body System/Preferred Term')

  #=====
  # QC
  #=====
  qc <- aesum_p %>% dplyr::mutate(SYSPREF=paste0(AEBODSYS,'/','\\line',AEDECOD)) %>% repfun::ru_labels(varlabels=list('SYSPREF'='Body System/Preferred Term')) %>%
        dplyr::relocate(SYSPREF)

  #==========
  # Compare
  #==========
  testthat::expect_equal(prod, qc)

})
