test_that("aligning columns works", {

  suppressMessages(library(repfun))
  suppressMessages(library(dplyr))
  suppressMessages(library(tidyr))
  suppressMessages(library(testthat))

  #=================
  # Adverse Events
  #=================
  repfun::rs_setup(D_POP="SAFFL",D_POPLBL="Safety",D_POPDATA=repfun::adsl %>% dplyr::filter(SAFFL =='Y'), D_SUBJID=c("STUDYID","USUBJID"), R_ADAMDATA=".")
  rfenv$G_POPDATA %>% dplyr::mutate(TRT01AN=ifelse(TRT01A=='Placebo',1,ifelse(TRT01A=='Xanomeline Low Dose',2,3))) -> G_POPDATA
  attr(G_POPDATA$TRT01AN,"label") <- 'Actual Treatment for Period 01 (n)'
  adae <- rfenv$adamdata$adae.rda() %>% dplyr::select(-SAFFL) %>% repfun::ru_getdata(G_POPDATA, c("STUDYID", "USUBJID"), keeppopvars=c("TRT01AN", "TRT01A"))
  aesum_t <- repfun::ru_freq(adae,
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
                                                           groupbyvars=c("tt_summarylevel", "AEBODSYS", "AEDECOD"), acrossvar="TRT01AN",
                                                           acrossvarlabel="TRT01A", acrossvarprefix=c("tt_ac", "tt_p"))

  #======
  # Prod
  #======
  aesum_t_a <- repfun::ru_align(aesum_t, "tt_ac:") %>% dplyr::select(-c('tt_p01','tt_p02','tt_p03','tt_p99'))

  #====
  # QC
  #====
  qcaesum_t_a <- aesum_t %>% dplyr::select(-c('tt_p01','tt_p02','tt_p03','tt_p99')) %>%
    dplyr::mutate(tt_ac01=ifelse(nchar(trimws(gsub('^.*\\(','\\(',tt_ac01)))==4,gsub(' ','  ',tt_ac01),tt_ac01),
           tt_ac02=ifelse(nchar(trimws(gsub('^.*\\(','\\(',tt_ac02)))==4,gsub(' ','  ',tt_ac02),tt_ac02),
           tt_ac03=ifelse(nchar(trimws(gsub('^.*\\(','\\(',tt_ac03)))==4,gsub(' ','  ',tt_ac03),tt_ac03),
           tt_ac99=ifelse(nchar(trimws(gsub('^.*\\(','\\(',tt_ac99)))==4,gsub(' ','  ',tt_ac99),tt_ac99),
           l1=nchar(tt_ac01), l2=nchar(tt_ac02), l3=nchar(tt_ac03), l9=nchar(tt_ac99)) %>% {. ->> QCINT} %>%
    dplyr::summarize(m1=max(l1), m2=max(l2), m3=max(l3), m9=max(l9)) %>% tidyr::crossing(QCINT) %>%
    dplyr::mutate(tt_ac01=paste0(strrep(' ',m1-l1),tt_ac01),
           tt_ac02=paste0(strrep(' ',m2-l2),tt_ac02),
           tt_ac03=paste0(strrep(' ',m3-l3),tt_ac03),
           tt_ac99=paste0(strrep(' ',m9-l9),tt_ac99),) %>% dplyr::select(-c(starts_with('m'),starts_with('l'))) %>%
    repfun::ru_labels(varlabels=lapply(aesum_t, attr, "label"))

  testthat::expect_equal(aesum_t_a, qcaesum_t_a)

  #===========================
  # Baseline Characteristics
  #===========================
  rs_setup(D_POP="SAFFL",D_POPLBL="Safety",D_POPDATA=repfun::adsl %>% dplyr::filter(SAFFL =='Y'), D_SUBJID=c("STUDYID","USUBJID"), R_ADAMDATA=".")
  rfenv$G_POPDATA %>% dplyr::mutate(TRT01AN=ifelse(TRT01A=='Placebo',1,ifelse(TRT01A=='Xanomeline Low Dose',2,3))) %>% repfun::ru_labels(varlabels=list('TRT01AN'='Actual Treatment for Period 01 (n)')) -> G_POPDATA
  demstats_t <- repfun::ru_sumstats(G_POPDATA,
                            analysisvars=c("AGE","TRTDURD"),
                            groupbyvars=c("STUDYID","TRT01AN"),
                            codedecodevarpairs=c("TRT01AN", "TRT01A"),
                            totalforvar="TRT01AN", totalid=99,
                            totaldecode="Total",
                            statsinrowsyn = "Y",
                            analysisvardps=list("AGE"=1,"TRTDURD"=2),
                            statslist=c("n", "mean", "median", "sd", "min", "max")) %>%
                repfun::ru_denorm(varstodenorm=c("tt_result"), groupbyvars=c("tt_avid", "tt_avnm", "tt_svid", "tt_svnm"),
                                  acrossvar="TRT01AN", acrossvarlabel="TRT01A", acrossvarprefix=c("tt_ac"))
  #=======
  # PROD
  #=======
  demstats_t_a <- repfun::ru_align(demstats_t, "tt_ac:", ncspaces=10)

  #=======
  # QC
  #=======
  qcdemstats_t_a <- demstats_t %>% dplyr::mutate(b1=3 - nchar(trimws(gsub('\\..*$','',tt_ac01))),
                                          a1=4 - ifelse(grepl('.',tt_ac01,fixed=TRUE),nchar(trimws(gsub('^.*\\.','',tt_ac01))),-1),
                                          tt_ac01=paste0(strrep(' ',b1),trimws(tt_ac01),strrep(' ',a1)),

                                          b2=3 - nchar(trimws(gsub('\\..*$','',tt_ac02))),
                                          a2=4 - ifelse(grepl('.',tt_ac02,fixed=TRUE),nchar(trimws(gsub('^.*\\.','',tt_ac02))),-1),
                                          tt_ac02=paste0(strrep(' ',b2),trimws(tt_ac02),strrep(' ',a2)),

                                          b3=3 - nchar(trimws(gsub('\\..*$','',tt_ac03))),
                                          a3=4 - ifelse(grepl('.',tt_ac03,fixed=TRUE),nchar(trimws(gsub('^.*\\.','',tt_ac03))),-1),
                                          tt_ac03=paste0(strrep(' ',b3),trimws(tt_ac03),strrep(' ',a3)),

                                          b4=3 - nchar(trimws(gsub('\\..*$','',tt_ac99))),
                                          a4=4 - ifelse(grepl('.',tt_ac99,fixed=TRUE),nchar(trimws(gsub('^.*\\.','',tt_ac99))),-1),
                                          tt_ac99=paste0(strrep(' ',b4),trimws(tt_ac99),strrep(' ',a4))) %>%
    dplyr::select(-c(starts_with('a'),starts_with('b'))) %>% repfun::ru_labels(varlabels=lapply(demstats_t, attr, "label"))

  testthat::expect_equal(demstats_t_a,qcdemstats_t_a)

})
