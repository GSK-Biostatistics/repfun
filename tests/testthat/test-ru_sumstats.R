test_that("generating summary statistics works", {

  suppressMessages(library(repfun))
  suppressMessages(library(dplyr))
  suppressMessages(library(tidyr))
  suppressMessages(library(tibble))
  suppressMessages(library(testthat))

  options(dplyr.summarise.inform = FALSE)

  repfun::setpath(paste0(rfenv$PATH,'/tests/testthat'))

  #====================
  # Environment setup.
  #====================
  rs_setup(D_POPDATA=repfun::adsl %>% dplyr::filter(SAFFL =='Y'),
           D_SUBJID=c("STUDYID","USUBJID"),
           R_ADAMDATA=".")

  #=================================
  # Baseline characteristics: PROD
  #=================================
  rfenv$G_POPDATA %>% dplyr::mutate(TRT01AN=ifelse(TRT01A=='Placebo',1,ifelse(TRT01A=='Xanomeline Low Dose',2,3))) -> G_POPDATA
  attr(G_POPDATA$TRT01AN,"label") <- 'Actual Treatment for Period 01 (n)'
  repfun::ru_sumstats(G_POPDATA,
              analysisvars=c("AGE","TRTDURD"),
              groupbyvars=c("STUDYID","TRT01AN"),
              codedecodevarpairs=c("TRT01AN", "TRT01A"),
              totalforvar="TRT01AN", totalid=99,
              totaldecode="Total",
              statsinrowsyn = "Y",
              analysisvardps=list("AGE"=1,"TRTDURD"=2),
              statslist=c("n", "mean", "median", "sd", "min", "max")) %>%
    dplyr::arrange(tt_avid, TRT01AN,tt_svid) %>%
    dplyr::select(-c('tt_svid','tt_avid','tt_result_num')) -> basechars
  attr(basechars$tt_result,"label") <- 'Result'

  #=================================
  # Baseline characteristics: QC
  #=================================
  G_POPDATA %>% dplyr::select(STUDYID,TRT01AN,TRT01A,AGE) %>% {. ->> pop1} %>% base::rbind(pop1 %>% dplyr::mutate(TRT01AN=99, TRT01A='Total')) %>% dplyr::filter(!is.na(AGE)) %>%
    dplyr::group_by(STUDYID,TRT01AN,TRT01A) %>% dplyr::summarize(n=as.character(round(n(),0)),
                                                          Mean=format(round(mean(AGE, na.rm=TRUE),1+1),nsmall=1+1),
                                                          Median=format(round(stats::median(AGE, na.rm=TRUE),1+1),nsmall=1+1),
                                                          SD=format(round(stats::sd(AGE, na.rm=TRUE),1+2),nsmall=1+2),
                                                          Min=format(round(min(AGE, na.rm=TRUE),1),nsmall=1),
                                                          Max=format(round(max(AGE, na.rm=TRUE),1),nsmall=1)) %>%
    tidyr::pivot_longer(!c(STUDYID,TRT01AN,TRT01A), names_to="tt_svnm", values_to="tt_result") %>%
    dplyr::ungroup() %>% select(STUDYID, TRT01AN, TRT01A, tt_result, tt_svnm) %>%
    dplyr::mutate(tt_avnm='AGE' ) -> qc_basechars_age
  G_POPDATA %>% dplyr::select(STUDYID,TRT01AN,TRT01A,TRTDURD) %>% {. ->> pop1} %>%
    base::rbind(pop1 %>% dplyr::mutate(TRT01AN=99, TRT01A='Total')) %>%
    dplyr::filter(!is.na(TRTDURD)) %>%
    dplyr::group_by(STUDYID,TRT01AN,TRT01A) %>% dplyr::summarize(n=as.character(round(n(),0)),
                                                          Mean=format(round(mean(TRTDURD, na.rm=TRUE),2+1),nsmall=2+1),
                                                          Median=format(round(stats::median(TRTDURD, na.rm=TRUE),2+1),nsmall=2+1),
                                                          SD=format(round(stats::sd(TRTDURD, na.rm=TRUE),2+2),nsmall=2+2),
                                                          Min=format(round(min(TRTDURD, na.rm=TRUE),2),nsmall=2),
                                                          Max=format(round(max(TRTDURD, na.rm=TRUE),2),nsmall=2)) %>%
    tidyr::pivot_longer(!c(STUDYID,TRT01AN,TRT01A), names_to="tt_svnm", values_to="tt_result") %>% dplyr::ungroup() %>%
    dplyr::select(STUDYID, TRT01AN, TRT01A, tt_result, tt_svnm) %>%
    dplyr::mutate(tt_avnm='TRTDURD') -> qc_basechars_dur
  qc_basechars <- as.data.frame(base::rbind(qc_basechars_age,qc_basechars_dur))
  attr(qc_basechars$tt_svnm,"label") <- 'Statistical Parameter Name'
  attr(qc_basechars$tt_result,"label") <- 'Result'
  attr(qc_basechars$tt_avnm,"label") <- 'Analysis Variable Name'

  testthat::expect_equal(basechars, qc_basechars)     # <---------------------------------------------------------------------------

  #==============================================================
  # Vital signs part 1: Precision constant across PARAMCD/PARAM.
  #==============================================================
  tibble::as_tibble(rfenv$adamdata$advs.rda()) %>% dplyr::filter(ANL01FL=='Y') %>%
    dplyr::mutate(TRT01AN=ifelse(TRT01A=='Placebo',1,ifelse(TRT01A=='Xanomeline Low Dose',2,3))) %>%
    dplyr::filter(!is.na(AVISITN) & (DTYPE=='AVERAGE')) -> advs2
  attr(advs2$TRT01AN,"label") <- 'Actual Treatment for Period 01 (n)'
  ru_sumstats(advs2,
              analysisvars=c("AVAL"),
              groupbyvars=c("STUDYID", "TRT01AN", "PARAMCD", "AVISITN"),
              codedecodevarpairs=c("TRT01AN", "TRT01A", "PARAMCD", "PARAM", "AVISITN", "AVISIT"),
              totalforvar="TRT01AN",
              totalid=99,
              totaldecode="Total",
              statsinrowsyn = "Y",
              analysisvardps=1,
              statslist=c("n", "mean", "median", "sd", "min", "max")) %>%
    dplyr::arrange(TRT01AN,PARAMCD,AVISITN, tt_svid) %>%
    dplyr::select(-contains("tt_av"),-tt_svid,-tt_result_num) -> vtlsigns
  attr(vtlsigns$tt_result,"label") <- 'Result'

  #========================
  # Vital Signs part 1: QC
  #========================
  advs2 %>% dplyr::select(STUDYID,TRT01AN,TRT01A,PARAMCD,PARAM,AVISITN,AVISIT,AVAL) %>% {. ->> pop1} %>% base::rbind(pop1 %>% dplyr::mutate(TRT01AN=99, TRT01A='Total')) %>%
    dplyr::filter(!is.na(AVAL)) %>%
    group_by(STUDYID,TRT01AN,TRT01A,PARAMCD,PARAM,AVISITN,AVISIT) %>% dplyr::summarize(n=as.character(round(n(),0)),
                                                                                       Mean=format(round(mean(AVAL, na.rm=TRUE),1+1),nsmall=1+1),
                                                                                       Median=format(round(stats::median(AVAL, na.rm=TRUE),1+1),nsmall=1+1),
                                                                                       SD=format(round(stats::sd(AVAL, na.rm=TRUE),1+2),nsmall=1+2),
                                                                                       Min=format(round(min(AVAL, na.rm=TRUE),1),nsmall=1),
                                                                                       Max=format(round(max(AVAL, na.rm=TRUE),1),nsmall=1)) %>%
    tidyr::pivot_longer(!c(STUDYID,TRT01AN,TRT01A,PARAMCD,PARAM,AVISITN,AVISIT), names_to="tt_svnm", values_to="tt_result") %>% dplyr::ungroup() %>%
    dplyr::select(STUDYID, TRT01AN, TRT01A, PARAMCD, PARAM, AVISITN, AVISIT, tt_result, tt_svnm) -> qc_vtlsigns
  qc_vtlsigns <- as.data.frame(qc_vtlsigns)
  attr(qc_vtlsigns$tt_svnm,"label") <- 'Statistical Parameter Name'
  attr(qc_vtlsigns$tt_result,"label") <- 'Result'

  testthat::expect_equal(vtlsigns, qc_vtlsigns)     # <---------------------------------------------------------------------------

  #=========================================================
  # Vital signs part 2: Precision differs by PARAMCD/PARAM.
  #=========================================================
  decodes <- advs2 %>% distinct(PARAMCD, PARAM)
  dcodelst <- split(decodes$PARAM, decodes$PARAMCD)
  advs2 %>% dplyr::select(STUDYID,USUBJID,TRT01AN,TRT01A,PARAMCD,AVISITN,AVISIT,AVAL) %>% dplyr::arrange(USUBJID,TRT01AN,TRT01A,AVISITN,AVISIT) %>%
    dplyr::group_by(USUBJID,TRT01AN,TRT01A,AVISITN,AVISIT) %>% tidyr::pivot_wider(names_from=PARAMCD, values_from=AVAL) -> advs2_t
  advs2_t <- ru_labels(advs2_t,varlabels=dcodelst)
  vtlsigns_t <- repfun::ru_sumstats(advs2_t,
                            analysisvars=c("BMI", "BSA", "DIABP", "MAP", "PULSE", "SYSBP", "TEMP", "WEIGHT"),
                            groupbyvars=c("STUDYID","TRT01AN","AVISITN"),
                            codedecodevarpairs=c("TRT01AN", "TRT01A", "AVISITN", "AVISIT"),
                            totalforvar="TRT01AN", totalid=99,
                            totaldecode="Total",
                            statsinrowsyn = "Y",
                            analysisvardps=list("BMI"=1, "BSA"=2, "DIABP"=3, "MAP"=4, "PULSE"=1, "SYSBP"=2, "TEMP"=3, "WEIGHT"=4),
                            statslist=c("n", "mean", "median", "sd", "min", "max")) %>%
    dplyr::left_join(decodes %>% mutate(tt_avnm=PARAMCD),by='tt_avnm') %>%
    dplyr::arrange(tt_avid, TRT01AN, AVISITN, tt_svid) %>%
    dplyr::select(-c('tt_svid','tt_avid','tt_avnm','tt_result_num')) %>%
    dplyr::arrange(PARAMCD,TRT01AN,AVISITN)
  attr(vtlsigns_t$tt_result,"label") <- 'Result'

  #========================
  # Vital Signs part 2: QC
  #========================
  advs2 %>% dplyr::select(STUDYID,TRT01AN,TRT01A,PARAMCD,PARAM,AVISITN,AVISIT,AVAL) %>% {. ->> pop1} %>% base::rbind(pop1 %>% dplyr::mutate(TRT01AN=99, TRT01A='Total')) %>%
    dplyr::filter(!is.na(AVAL)) %>% dplyr::mutate(prec=1,
                                    prec=ifelse(grepl("(BSA|SYSBP)",PARAMCD),prec+1,prec),
                                    prec=ifelse(grepl("(DIABP|TEMP)",PARAMCD),prec+2,prec),
                                    prec=ifelse(grepl("(MAP|WEIGHT)",PARAMCD),prec+3,prec)) %>%
    dplyr::group_by(STUDYID,TRT01AN,TRT01A,prec,PARAMCD,PARAM,AVISITN,AVISIT) %>% dplyr::summarize(n=n(),
                                                                                            Mean=mean(AVAL, na.rm=TRUE),
                                                                                            Median=stats::median(AVAL, na.rm=TRUE),
                                                                                            SD=stats::sd(AVAL, na.rm=TRUE),
                                                                                            Min=min(AVAL, na.rm=TRUE),
                                                                                            Max=max(AVAL, na.rm=TRUE)) %>% ungroup() %>%
    dplyr::mutate(n=as.character(n),
           Mean=sprintf(paste0('%.',prec+1,'f'),round(Mean,prec+1)),
           Median=sprintf(paste0('%.',prec+1,'f'),round(Median,prec+1)),
           SD=sprintf(paste0('%.',prec+2,'f'),round(SD,prec+2)),
           Min=sprintf(paste0('%.',prec,'f'),round(Min,prec)),
           Max=sprintf(paste0('%.',prec,'f'),round(Max,prec))) %>% select(-prec) %>%
    dplyr::group_by(STUDYID,TRT01AN,TRT01A,PARAMCD,PARAM,AVISITN,AVISIT) %>%
    tidyr::pivot_longer(!c(STUDYID,TRT01AN,TRT01A,PARAMCD,PARAM,AVISITN,AVISIT), names_to="tt_svnm", values_to="tt_result") %>% dplyr::ungroup() %>%
    dplyr::select(STUDYID, TRT01AN, TRT01A, PARAMCD, PARAM, AVISITN, AVISIT, tt_result, tt_svnm) %>% dplyr::arrange(PARAMCD,TRT01AN,AVISITN) %>% dplyr::select(names(vtlsigns_t)) -> qc_vtlsigns_t
  qc_vtlsigns_t <- as.data.frame(qc_vtlsigns_t)
  attr(qc_vtlsigns_t$tt_svnm,"label") <- 'Statistical Parameter Name'
  attr(qc_vtlsigns_t$tt_result,"label") <- 'Result'

  testthat::expect_equal(vtlsigns_t, qc_vtlsigns_t)     # <---------------------------------------------------------------------------

})
