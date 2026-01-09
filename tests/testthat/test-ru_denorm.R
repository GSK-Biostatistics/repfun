test_that("transposing a dataframe works", {

  suppressMessages(library(repfun))
  suppressMessages(library(dplyr))
  suppressMessages(library(tidyr))
  suppressMessages(library(testthat))

  #======================
  #======================
  # Adverse Events Test
  #======================
  #======================

  #=======
  # SETUP
  #=======
  repfun::rs_setup(D_POP="SAFFL",D_POPLBL="Safety",D_POPDATA=repfun::adsl %>% filter(SAFFL =='Y'), D_SUBJID=c("STUDYID","USUBJID"), R_ADAMDATA=".")
  rfenv$G_POPDATA %>% dplyr::mutate(TRT01AN=ifelse(TRT01A=='Placebo',1,ifelse(TRT01A=='Xanomeline Low Dose',2,3))) %>% repfun::ru_labels(varlabels=list('TRT01AN'='Actual Treatment for Period 01 (n)')) -> G_POPDATA
  adae <- rfenv$adamdata$adae.rda() %>% dplyr::select(-SAFFL) %>% repfun::ru_getdata(G_POPDATA, c("STUDYID", "USUBJID"), keeppopvars=c("TRT01AN", "TRT01A"))
  aesum <- repfun::ru_freq(adae,
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
                   resultpctdps=0)

  #=======
  # PROD
  #=======
  aeprod_t <- repfun::ru_denorm(aesum,varstodenorm=c("tt_result", "PERCENT"), groupbyvars=c("tt_summarylevel", "AEBODSYS", "AEDECOD"),
                                acrossvar="TRT01AN", acrossvarlabel="TRT01A", acrossvarprefix=c("tt_ac", "tt_p")) %>%
              dplyr::arrange(tt_summarylevel, AEBODSYS, AEDECOD)

  #=======
  # QC
  #=======
  aeqc_t <- aesum %>% dplyr::select(tt_summarylevel, AEBODSYS, AEDECOD, TRT01AN, tt_result, PERCENT) %>%
    dplyr::group_by(tt_summarylevel, AEBODSYS, AEDECOD) %>% tidyr::pivot_wider(names_from=TRT01AN, values_from=c(tt_result, PERCENT)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(tt_ac01=tt_result_1,tt_ac02=tt_result_2,tt_ac03=tt_result_3,tt_ac99=tt_result_99,
           tt_p01=PERCENT_1,tt_p02=PERCENT_2,tt_p03=PERCENT_3,tt_p99=PERCENT_99) %>% repfun::ru_labels(varlabels=lapply(aeprod_t, attr, "label")) %>%
    dplyr::select(names(aeprod_t)) %>% dplyr::arrange(tt_summarylevel, AEBODSYS, AEDECOD)

  #==================
  # Compare results
  #==================
  testthat::expect_equal(aeprod_t,aeqc_t)

  #=============================================
  #=============================================
  # Baseline Characteristics Summary Stats Test
  #=============================================
  #=============================================

  #=======
  # Setup
  #=======
  rs_setup(D_POP="SAFFL",D_POPLBL="Safety",D_POPDATA=repfun::adsl %>% dplyr::filter(SAFFL =='Y'), D_SUBJID=c("STUDYID","USUBJID"), R_ADAMDATA=".")
  rfenv$G_POPDATA %>% dplyr::mutate(TRT01AN=ifelse(TRT01A=='Placebo',1,ifelse(TRT01A=='Xanomeline Low Dose',2,3))) %>% repfun::ru_labels(varlabels=list('TRT01AN'='Actual Treatment for Period 01 (n)')) -> G_POPDATA
  demstats <- repfun::ru_sumstats(G_POPDATA,
                          analysisvars=c("AGE","TRTDURD"),
                          groupbyvars=c("STUDYID","TRT01AN"),
                          codedecodevarpairs=c("TRT01AN", "TRT01A"),
                          totalforvar="TRT01AN", totalid=99,
                          totaldecode="Total",
                          statsinrowsyn = "Y",
                          analysisvardps=list("AGE"=1,"TRTDURD"=2),
                          statslist=c("n", "mean", "median", "sd", "min", "max"))

  #=======
  # PROD
  #=======
  demprod_t <- repfun::ru_denorm(demstats, varstodenorm=c("tt_result"), groupbyvars=c("tt_avid", "tt_avnm", "tt_svid", "tt_svnm"),
                         acrossvar="TRT01AN", acrossvarlabel="TRT01A", acrossvarprefix=c("tt_ac"))

  #=======
  # QC
  #=======
  demqc_t <- demstats %>% dplyr::select(tt_avid,tt_avnm,tt_svid,tt_svnm,TRT01AN,tt_result) %>% dplyr::mutate(TRT01AN=sprintf("%02d",TRT01AN)) %>%
    dplyr::group_by(tt_avid,tt_avnm,tt_svid,tt_svnm) %>% tidyr::pivot_wider(names_from=TRT01AN, values_from=c(tt_result), names_prefix="tt_ac") %>%
    dplyr::ungroup() %>%
    repfun::ru_labels(varlabels=lapply(demprod_t, attr, "label")) %>% dplyr::select(names(demprod_t)) %>% dplyr::arrange(tt_avid,tt_avnm,tt_svid,tt_svnm)

  #==========
  # Compare
  #==========
  testthat::expect_equal(demprod_t,demqc_t)

})
