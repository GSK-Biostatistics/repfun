test_that("generating counts and percents works", {

  suppressMessages(library(repfun))
  suppressMessages(library(dplyr))
  suppressMessages(library(tidyr))
  suppressMessages(library(testthat))

  #===============================
  # Set up reporting environment.
  #===============================
  options(dplyr.summarise.inform = FALSE)
  repfun::rs_setup(D_POPDATA=repfun::adsl %>% dplyr::filter(SAFFL =='Y'),
           D_SUBJID=c("STUDYID","USUBJID"),
           R_ADAMDATA=".")

  #===========================================
  # Add numeric variables to ADSL (and ADAE).
  #===========================================
  rfenv$G_POPDATA %>% dplyr::mutate(TRT01AN=ifelse(TRT01A=='Placebo',1,ifelse(TRT01A=='Xanomeline Low Dose',2,3)),
                       SEXN=ifelse(SEX=='F',1,ifelse(SEX=='M',2,NA)),
                       RACEN=ifelse(RACE=='AMERICAN INDIAN OR ALASKA NATIVE',1,ifelse(RACE=='BLACK OR AFRICAN AMERICAN',2,ifelse(RACE=='WHITE',3,NA))),
                       AGEGR1N=ifelse(AGEGR1=='18-64',1,ifelse(AGEGR1=='>64',2,NA))) -> G_POPDATA
  attr(G_POPDATA$TRT01AN,"label") <- 'Actual Treatment for Period 01 (n)'
  attr(G_POPDATA$SEXN,"label") <- 'Sex (n)'
  attr(G_POPDATA$RACEN,"label") <- 'Race (n)'
  attr(G_POPDATA$AGEGR1N,"label") <- 'Pooled Age Group 1 (n)'

  G_POPDATA %>% dplyr::select(STUDYID,USUBJID,SAFFL,TRT01AN,TRT01A) -> G_POPDATA1
  adae <- rfenv$adamdata$adae.rda() %>% dplyr::filter(TRTEMFL=='Y') %>% dplyr::inner_join(G_POPDATA1, by=c('STUDYID','USUBJID','SAFFL','TRT01A'))

  #====================================================================
  # AEs: PROD Counts and percents for Body System and Preferred Term.
  #====================================================================
  aeprod <- repfun::ru_freq(adae,
                    dsetindenom=G_POPDATA1,
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
                    resultpctdps=2) %>%
            dplyr::select(TRT01AN,TRT01A,AEBODSYS,AEDECOD,NUMERCNT,DENOMCNT,PERCENT,tt_result) %>% dplyr::arrange(TRT01AN,TRT01A,AEBODSYS,AEDECOD)

  #=======
  # AE QC
  #=======
  G_POPDATA2 <- G_POPDATA1 %>% {. ->> POP1} %>% dplyr::mutate(TRT01AN=99,TRT01A='Total') %>% rbind(POP1) %>% dplyr::arrange(TRT01AN,TRT01A)
  aes <- adae %>% dplyr::select(STUDYID,USUBJID,TRT01AN,TRT01A,AEBODSYS,AEDECOD) %>% {. ->> ae1} %>% dplyr::mutate(AEDECOD='ANY EVENT') %>% {. ->> ae2} %>%
    dplyr::mutate(AEBODSYS='ANY EVENT') %>% rbind(ae2) %>% rbind(ae1) %>% dplyr::arrange(STUDYID,USUBJID,TRT01AN,TRT01A,AEBODSYS,AEDECOD) %>% dplyr::distinct(STUDYID,USUBJID,TRT01AN,TRT01A,AEBODSYS,AEDECOD)
  aes <- rbind(aes,aes %>% dplyr::mutate(TRT01AN=99,TRT01A='Total'))
  denoms <- G_POPDATA2 %>% dplyr::group_by(TRT01AN,TRT01A) %>% dplyr::summarize(DENOMCNT=n()) %>% dplyr::ungroup()
  numers <- aes %>% dplyr::group_by(TRT01AN,TRT01A,AEBODSYS,AEDECOD) %>% dplyr::summarize(NUMERCNT=n()) %>% dplyr::ungroup()
  master <- G_POPDATA2 %>% dplyr::distinct(TRT01AN,TRT01A) %>% tidyr::crossing(aes %>% dplyr::select(AEBODSYS,AEDECOD))
  numers <- master %>% dplyr::left_join(numers, by=c('TRT01AN','TRT01A','AEBODSYS','AEDECOD')) %>% dplyr::mutate(NUMERCNT=ifelse(is.na(NUMERCNT),0,NUMERCNT))
  aeqc   <- numers %>% dplyr::left_join(denoms, by=c('TRT01AN','TRT01A')) %>% dplyr::mutate(PERCENT=100*NUMERCNT/DENOMCNT,
                                                                                            tt_result=paste0(as.character(NUMERCNT),' (',trimws(sprintf('%.2f',round(PERCENT,2))),'%)')) %>%
                       dplyr::arrange(TRT01AN,TRT01A,AEBODSYS,AEDECOD)
  for (v in names(aeprod)){
    attr(aeqc[[v]],"label") <- attr(aeprod[[v]],"label")
  }
  aeqc <- as.data.frame(aeqc)

  testthat::expect_equal(aeprod, aeqc)

  #=========================================================
  # AEs: PROD Counts and percents for Preferred Terms only.
  #=========================================================
  aeprod2 <- repfun::ru_freq(adae,
                     dsetindenom=G_POPDATA1,
                     countdistinctvars=c('STUDYID','USUBJID'),
                     groupbyvarsnumer=c('TRT01AN','TRT01A','AEDECOD'),
                     anyeventvars = c('AEDECOD'),
                     anyeventvalues = c('ANY EVENT'),
                     groupbyvarsdenom=c('TRT01AN'),
                     resultstyle="NUMERPCT",
                     totalforvar=c('TRT01AN'),
                     totalid=99,
                     totaldecode='Total',
                     codedecodevarpairs=c("TRT01AN", "TRT01A"),
                     varcodelistpairs=c(""),
                     codelistnames=list(),
                     resultpctdps=2) %>% dplyr::select(TRT01AN,TRT01A,AEDECOD,NUMERCNT,DENOMCNT,PERCENT,tt_result) %>% dplyr::arrange(TRT01AN,TRT01A,AEDECOD)

  #=======
  # AE QC
  #=======
  G_POPDATA2 <- G_POPDATA1 %>% {. ->> POP1} %>% dplyr::mutate(TRT01AN=99,TRT01A='Total') %>% rbind(POP1) %>% dplyr::arrange(TRT01AN,TRT01A)
  aes <- adae %>% dplyr::select(STUDYID,USUBJID,TRT01AN,TRT01A,AEDECOD) %>% {. ->> ae1} %>% dplyr::mutate(AEDECOD='ANY EVENT') %>% {. ->> ae2} %>%
    rbind(ae1) %>% dplyr::arrange(STUDYID,USUBJID,TRT01AN,TRT01A,AEDECOD) %>% dplyr::distinct(STUDYID,USUBJID,TRT01AN,TRT01A,AEDECOD)
  aes <- rbind(aes,aes %>% dplyr::mutate(TRT01AN=99,TRT01A='Total'))
  denoms <- G_POPDATA2 %>% dplyr::group_by(TRT01AN,TRT01A) %>% dplyr::summarize(DENOMCNT=n()) %>% dplyr::ungroup()
  numers <- aes %>% dplyr::group_by(TRT01AN,TRT01A,AEDECOD) %>% dplyr::summarize(NUMERCNT=n()) %>% dplyr::ungroup()
  master <- G_POPDATA2 %>% dplyr::distinct(TRT01AN,TRT01A) %>% tidyr::crossing(aes %>% select(AEDECOD))
  numers <- master %>% dplyr::left_join(numers, by=c('TRT01AN','TRT01A','AEDECOD')) %>% dplyr::mutate(NUMERCNT=ifelse(is.na(NUMERCNT),0,NUMERCNT))
  aeqc2   <- numers %>% dplyr::left_join(denoms, by=c('TRT01AN','TRT01A')) %>% dplyr::mutate(PERCENT=100*NUMERCNT/DENOMCNT,
                                                                                             tt_result=paste0(as.character(NUMERCNT),' (',trimws(sprintf('%.2f',round(PERCENT,2))),'%)')) %>%
                        dplyr::arrange(TRT01AN,TRT01A,AEDECOD)
  for (v in names(aeprod)){
    attr(aeqc2[[v]],"label") <- attr(aeprod2[[v]],"label")
  }
  aeqc2 <- as.data.frame(aeqc2)

  testthat::expect_equal(aeprod2, aeqc2)

  #=====================================================
  # Baseline Characteristics: PROD counts and percents.
  #=====================================================
  G_POPDATA %>% dplyr::select(STUDYID,USUBJID,SAFFL,TRT01AN,TRT01A,SEXN,SEX,RACEN,RACE,AGEGR1N,AGEGR1) -> G_POPDATA2
  dflst <- list()
  basechar <- data.frame()
  i <- 1
  for(v in c('AGEGR1','SEX','RACE')){
    lbl <- attr(G_POPDATA[[v]],'label')
    dflst[[v]] <- repfun::ru_freq(G_POPDATA2, dsetindenom=G_POPDATA2, countdistinctvars=c("STUDYID", "USUBJID"),
                          groupbyvarsnumer=c("STUDYID", "TRT01AN", paste0(v,'N')),
                          anyeventvars = NULL, anyeventvalues = NULL, groupminmaxvar=NULL,
                          totalforvar=c("TRT01AN"), totalid = 99, totaldecode = 'Total',
                          groupbyvarsdenom=c("STUDYID", "TRT01AN"), resultstyle="NUMERPCT", codedecodevarpairs=c("TRT01AN", "TRT01A", paste0(v,'N'), v),
                          varcodelistpairs=c(""), codelistnames=list(), resultpctdps=0) %>% {. ->> LBLS} %>%
      dplyr::mutate(tt_avid=i, tt_avnm=lbl) %>%
      dplyr::rename(tt_svid=as.name(paste0(v,'N')), tt_svnm=as.name(v)) %>%
      dplyr::arrange(tt_avid,tt_svid) %>%
      dplyr::select(TRT01AN,TRT01A,tt_avid,tt_avnm,tt_svid,tt_svnm,NUMERCNT,DENOMCNT,PERCENT,tt_result) %>%
      dplyr::mutate(tt_avnm=paste0(tt_avnm,', n (%)'))
    row.names(dflst[[v]]) <- NULL
    basechar <- bind_rows(basechar,dflst[[v]])
    i <- i+1
  }
  basechar <- basechar %>% dplyr::arrange(TRT01AN,TRT01A,tt_avid,tt_avnm,tt_svid,tt_svnm)
  for (v in names(basechar)){
    if (v %in% names(LBLS)){
      attr(basechar[[v]],"label") <- attr(LBLS[[v]],"label")
    }
  }
  attr(basechar$tt_svid,"label") <- "Variable Order"
  attr(basechar$tt_svnm,"label") <- "Variable Name"
  attr(basechar$tt_avid,"label") <- "Value Order"
  attr(basechar$tt_avnm,"label") <- "Value Name"

  #=============
  # Basechar QC
  #=============
  G_POPDATA2 <- G_POPDATA2 %>% {. ->> POP1} %>% dplyr::mutate(TRT01AN=99,TRT01A='Total') %>% rbind(POP1) %>% dplyr::arrange(TRT01AN,TRT01A)
  denoms <- G_POPDATA2 %>% dplyr::group_by(TRT01AN,TRT01A) %>% dplyr::summarize(DENOMCNT=n()) %>% dplyr::ungroup()

  numers1 <- G_POPDATA2 %>% dplyr::group_by(TRT01AN,TRT01A,AGEGR1N,AGEGR1) %>% dplyr::summarize(NUMERCNT=n()) %>% dplyr::ungroup() %>% dplyr::mutate(tt_avid=1,tt_avnm=paste0(attr(G_POPDATA2$AGEGR1,"label"),', n (%)')) %>% dplyr::rename(tt_svid=AGEGR1N,tt_svnm=AGEGR1)
  numers2 <- G_POPDATA2 %>% dplyr::group_by(TRT01AN,TRT01A,SEXN,SEX) %>% dplyr::summarize(NUMERCNT=n()) %>% dplyr::ungroup() %>% dplyr::mutate(tt_avid=2,tt_avnm=paste0(attr(G_POPDATA2$SEX,"label"),', n (%)')) %>% dplyr::rename(tt_svid=SEXN,tt_svnm=SEX)
  numers3 <- G_POPDATA2 %>% dplyr::group_by(TRT01AN,TRT01A,RACEN,RACE) %>% dplyr::summarize(NUMERCNT=n()) %>% dplyr::ungroup() %>% dplyr::mutate(tt_avid=3,tt_avnm=paste0(attr(G_POPDATA2$RACE,"label"),', n (%)')) %>% dplyr::rename(tt_svid=RACEN,tt_svnm=RACE)
  numers  <- bind_rows(numers1,numers2,numers3)

  master  <- G_POPDATA2 %>% distinct(TRT01AN,TRT01A) %>% tidyr::crossing(numers %>% distinct(tt_avid,tt_avnm,tt_svid,tt_svnm))
  numers  <- master %>% dplyr::left_join(numers, by=c('TRT01AN','TRT01A','tt_avid','tt_avnm','tt_svid','tt_svnm')) %>% dplyr::mutate(NUMERCNT=ifelse(is.na(NUMERCNT),0,NUMERCNT))

  bcqc    <- numers %>% dplyr::left_join(denoms, by=c('TRT01AN','TRT01A')) %>% dplyr::mutate(PERCENT=100*NUMERCNT/DENOMCNT,
                                                                               tt_result=paste0(as.character(NUMERCNT),' (',trimws(sprintf('%.0f',round(PERCENT,0))),'%)')) %>%
    dplyr::arrange(TRT01AN,TRT01A,tt_avid,tt_avnm,tt_svid,tt_svnm) %>% dplyr::select(TRT01AN,TRT01A,tt_avid,tt_avnm,tt_svid,tt_svnm,NUMERCNT,DENOMCNT,PERCENT,tt_result)
  for (v in names(basechar)){
    attr(bcqc[[v]],"label") <- attr(basechar[[v]],"label")
  }
  bcqc <- as.data.frame(bcqc)

  testthat::expect_equal(basechar, bcqc)

  #=====================================================
  # Vital Sign Timepoints: PROD counts and percents.
  #=====================================================
  advs <- repfun::advs %>%
    dplyr::mutate(TRT01AN=ifelse(TRT01A=='Placebo',1,ifelse(TRT01A=='Xanomeline Low Dose',2,3))) %>%
    filter(SAFFL=='Y' & !is.na(ATPTN) & !is.na(AVISITN) & !is.na(TRT01AN)) %>%
    ru_labels(varlabels=list('TRT01AN'='Actual Treatment for Period 01 (n)'))

  #=======
  # PROD
  #=======
  prod_advs <- repfun::ru_freq(advs, dsetindenom=advs, countdistinctvars=c("STUDYID", "USUBJID"),
                               groupbyvarsnumer=c("PARAMCD", "TRT01AN", "AVISITN", "ATPTN"),
                               anyeventvars = NULL, anyeventvalues =  NULL, groupminmaxvar="max(ATPTN)",
                               totalforvar="TRT01AN(2, 3)", totalid = 9, totaldecode = 'All Xano',
                               groupbyvarsdenom=c("PARAMCD", "TRT01AN", "AVISITN"), resultstyle="NUMERPCT",
                               codedecodevarpairs=c("TRT01AN", "TRT01A", "AVISITN", "AVISIT", "PARAMCD", "PARAM", "ATPTN", "ATPT"),
                               varcodelistpairs=c(""), codelistnames=list(), resultpctdps=0) %>% select(-tt_summarylevel) %>%
    arrange(PARAMCD,TRT01AN,AVISITN,ATPTN)


  #=====
  # QC
  #=====
  advs_tot <- advs %>% mutate(TRT01AN=ifelse(TRT01AN %in% c(2,3),9,TRT01AN),
                              TRT01A=ifelse(TRT01AN==9,'All Xano',TRT01A))

  advs2 <- advs %>% rbind(advs_tot)

  trt <- advs2 %>% distinct(TRT01AN,TRT01A)
  vis <- advs2 %>% distinct(AVISITN,AVISIT)
  prm <- advs2 %>% distinct(PARAMCD,PARAM)
  tim <- advs2 %>% distinct(ATPTN,ATPT)
  mfile <- trt %>% cross_join(vis) %>% cross_join(prm) %>% cross_join(tim) %>% mutate(NUMERCNT=0)

  #PROD <- vs_cnts %>% distinct(TRT01AN,TRT01A,AVISITN,AVISIT,PARAMCD,PARAM,ATPTN,ATPT)
  #QC <- mfile %>% distinct(TRT01AN,TRT01A,AVISITN,AVISIT,PARAMCD,PARAM,ATPTN,ATPT)
  #freq(PROD,c('TRT01AN','TRT01A','PARAMCD','PARAM'))
  #freq(QC,c('TRT01AN','TRT01A','PARAMCD','PARAM'))
  #PROD %>% arrange(PARAMCD,AVISITN,AVISIT) %>% distinct(PARAMCD,AVISITN,AVISIT)
  #QC %>% arrange(PARAMCD,AVISITN,AVISIT) %>% distinct(PARAMCD,AVISITN,AVISIT)
  #testthat::expect_equal(PROD, QC)

  qc_numer <- advs2 %>%
    distinct(STUDYID,USUBJID,TRT01AN,TRT01A,PARAMCD,PARAM,AVISITN,AVISIT,ATPTN,ATPT) %>%
    arrange(STUDYID,USUBJID,TRT01AN,TRT01A,PARAMCD,PARAM,AVISITN,AVISIT,ATPTN,ATPT) %>%
    group_by(STUDYID,USUBJID,TRT01AN,TRT01A,PARAMCD,PARAM,AVISITN,AVISIT) %>%
    filter(row_number()==n()) %>%
    ungroup() %>%
    group_by(TRT01AN,TRT01A,PARAMCD,PARAM,AVISITN,AVISIT,ATPTN,ATPT) %>%
    summarize(COUNT=n()) %>%
    ungroup()

  qc_denom <- advs2 %>%
    distinct(STUDYID,USUBJID,TRT01AN,TRT01A,PARAMCD,PARAM,AVISITN,AVISIT) %>%
    group_by(TRT01AN,TRT01A,PARAMCD,PARAM,AVISITN,AVISIT) %>%
    summarize(DENOMCNT=n()) %>%
    ungroup()

  qc_advs <- mfile %>%
    left_join(qc_numer,by=c('TRT01AN','TRT01A','PARAMCD','PARAM','AVISITN','AVISIT','ATPTN','ATPT')) %>%
    left_join(qc_denom,by=c('TRT01AN','TRT01A','PARAMCD','PARAM','AVISITN','AVISIT')) %>%
    mutate(NUMERCNT=ifelse(!is.na(COUNT),COUNT,NUMERCNT),
           PERCENT=100*NUMERCNT/DENOMCNT,
           tt_result=paste0(NUMERCNT,' (',round(PERCENT,0),'%)')) %>% select(names(prod_advs)) %>%
    arrange(PARAMCD,TRT01AN,AVISITN,ATPTN)

  #=========================================
  # Temporary step, remove all attributes.
  #=========================================
  qc_advs[] <- lapply(qc_advs, function(x) {
    attributes(x) <- NULL
    x
  })

  prod_advs[] <- lapply(prod_advs, function(x) {
    attributes(x) <- NULL
    x
  })

  attr(qc_advs, '_xportr.df_arg_') <- NULL
  attr(qc_advs, 'label') <- NULL
  attr(prod_advs, '_xportr.df_arg_') <- NULL
  attr(prod_advs, 'label') <- NULL

  testthat::expect_equal(prod_advs, qc_advs)

})
