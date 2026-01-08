test_that("producing RTFs works", {

  suppressMessages(library(repfun))
  suppressMessages(library(dplyr))
  suppressMessages(library(tidyr))
  suppressMessages(library(haven))
  suppressMessages(library(testthat))

  repfun::setpath(paste0(rfenv$PATH,'/tests/testthat'))

  #===================================
  # Set up the reporting environment.
  #===================================
  tmpdr <- tempdir()
  setup <- function(tlfid){
    repfun::rs_setup(
      D_DATADATE=Sys.Date(),
      D_DSPLYNUM=tlfid,
      D_FOOT1='1.) Only treatment emergent events related to lipids are displayed.',
      D_FOOT2='2.) Subjects are only counted once within each body system and preferred term.',
      D_KEEPPOPVARS=c('STUDYID','USUBJID','SAFFL'),
      D_OUTFILE=paste0(tmpdr,"/t_ru_list_",tlfid,".rtf"),
      D_PGMPTH=".",
      D_STUDYID='ABCXYZPDQ',
      D_POP="SAFFL",
      D_POPDATA=repfun::adsl %>% dplyr::filter(SAFFL =='Y') %>% dplyr::mutate(TRT01AN=ifelse(TRT01A=='Placebo',1,ifelse(TRT01A=='Xanomeline Low Dose',2,3))) %>%
        repfun::ru_labels(varlabels=list('TRT01AN'='Actual Treatment for Period 01 (n)')),
      D_POPLBL="Safety",
      D_SUBJID=c("STUDYID","USUBJID"),
      D_TITLE1=paste0('Table ',tlfid,': Summary of Treatment Emergent Adverse Events'),
      R_DDDATA=paste0(tmpdr,'/t_ru_list_',tlfid,'.rds'),
      R_ADAMDATA=".")
  }

  #============================================
  # Process ADAE - derive counts and percents.
  #============================================
  setup(1)
  aesum <- repfun::ru_freq(rfenv$adamdata$adae.rda() %>% dplyr::select(-SAFFL) %>% repfun::ru_getdata(rfenv$G_POPDATA, c("STUDYID", "USUBJID"), keeppopvars=c("TRT01AN", "TRT01A")),
                           dsetindenom=rfenv$G_POPDATA,
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
                           resultpctdps=0) %>% dplyr::arrange(TRT01AN,TRT01A,AEBODSYS,tt_summarylevel,AEDECOD,NUMERCNT,DENOMCNT) %>% repfun::ru_align("tt_result")

  #==========================================================
  # Prod Table 1:  Summary of Adverse Events - DDDATA only.
  #==========================================================
  repfun::ru_list(aesum,
                  display='Y',
                  columns=c('AEBODSYS','AEDECOD','tt_01','tt_02','tt_03','tt_99'),
                  nowidowvar='AEBODSYS',
                  widths=c(5.5,4.5,1.75,1.9,1.9,1.75),
                  skipvars=c('AEBODSYS'),
                  centrevars=c('tt_01','tt_02','tt_03','tt_99'),
                  ordervars=c('AEBODSYS','tt_summarylevel','AEDECOD'),
                  noprintvars=c('tt_summarylevel'),
                  denormyn='Y',
                  varsToDenorm=c('tt_result'),
                  groupByVars=c('AEBODSYS','tt_summarylevel','AEDECOD'),
                  acrossVar="TRT01AN",
                  acrossVarLabel="TRT01A",
                  acrossColVarPrefix='tt_',
                  dddatasetlabel=paste0('DD Dataframe for AE Table ',rfenv$G_DSPLYNUM),
                  lpp=23,
                  rpp=50)

  #======================
  # Read in PROD DDDATA.
  #======================
  #prod1 <- haven::read_xpt(gsub('rds','xpt',rfenv$G_DDDATA))
  prod <- readRDS(rfenv$G_DDDATA)
  prod1 <- prod %>% mutate_at(vars(AEBODSYS,AEDECOD,tt_01,tt_02,tt_03,tt_99),as.character) %>% ru_labels(.,varlabels=lapply(prod,attr,"label"))

  #====================
  # Produce QC DDDATA.
  #====================
  qc1 <- aesum %>% dplyr::mutate(TRT01A=paste0(TRT01A,'\\line (N=',DENOMCNT,')\\line n (%)')) %>%
    repfun::ru_denorm(varstodenorm=c("tt_result", "PERCENT"), groupbyvars=c("tt_summarylevel", "AEBODSYS", "AEDECOD"), acrossvar="TRT01AN",
                      acrossvarlabel="TRT01A", acrossvarprefix=c("tt_", "tt_p")) %>% {lapply(., attr, "label") ->> LBLS; .} %>% dplyr::arrange(AEBODSYS,tt_summarylevel,AEDECOD) %>%
    dplyr::select(-starts_with('tt_p'),-tt_summarylevel) %>% dplyr::group_by(AEBODSYS) %>% dplyr::group_modify(~ add_row(.x,.after=Inf)) %>%
    dplyr::ungroup() %>% dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(.x, ""))) %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    repfun::ru_labels(varlabels=LBLS,style='base') %>%
    repfun::ru_addpage(grpvars=c('AEBODSYS'),rowsprbdy=20)

  #=============================
  # Fix dangling group headers.
  #=============================
  qc1 %>% group_by(AEBODSYS) %>% mutate(lastrow=ifelse(row_number()==n(),TRUE,FALSE),
                                        frstpage=ifelse(lastrow & !PAGEVAR==lag(PAGEVAR),TRUE,FALSE)) %>% filter(!(lastrow & frstpage)) %>% select(-c(lastrow,frstpage)) -> qc1

  qc1 <- as.data.frame(qc1)

  attr(qc1,'label') <- 'DD Dataframe for AE Table 1'

  #==================
  # Test 1: Compare.
  #==================
  testthat::expect_equal(prod1, qc1)

  #==========================================================
  # Prod Table 3:  Summary of Adverse Events - DDDATA only.
  #==========================================================
  setup(3)
  aesum <- repfun::ru_freq(rfenv$adamdata$adae.rda() %>% dplyr::select(-SAFFL) %>%
                             repfun::ru_getdata(rfenv$G_POPDATA, c("STUDYID", "USUBJID"), keeppopvars=c("TRT01AN", "TRT01A")),
                           dsetindenom=rfenv$G_POPDATA,
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
                           resultpctdps=0) %>% dplyr::arrange(TRT01AN,TRT01A,AEBODSYS,tt_summarylevel,AEDECOD,NUMERCNT,DENOMCNT) %>% repfun::ru_align("tt_result")

  #=========
  #=========
  # PROD 3
  #=========
  #=========
  aetbls <- list('Gender: Male'=aesum,
                 'Gender: Female'=aesum)

  repfun::ru_list(aetbls,
                  display='Y',
                  columns=c('AEBODSYS','AEDECOD','tt_01','tt_02','tt_03','tt_99'),
                  nowidowvar='AEBODSYS',
                  widths=c(5.5,4.5,1.75,1.9,1.9,1.75),
                  skipvars=c('AEBODSYS'),
                  centrevars=c('tt_01','tt_02','tt_03','tt_99'),
                  ordervars=c('AEBODSYS','tt_summarylevel','AEDECOD'),
                  noprintvars=c('tt_summarylevel'),
                  denormyn='Y',
                  varsToDenorm=c('tt_result'),
                  groupByVars=c('AEBODSYS','tt_summarylevel','AEDECOD'),
                  acrossVar="TRT01AN",
                  acrossVarLabel="TRT01A",
                  acrossColVarPrefix='tt_',
                  dddatasetlabel=paste0('DD Dataframe for AE Table ',rfenv$G_DSPLYNUM),
                  lpp=23,
                  rpp=50)

  prod3 <- readRDS(paste0(tmpdr,'/t_ru_list_3.rds'))
  prod3[] <- lapply(prod3, as.character)
  attr(prod3, 'label') <- NULL

  #=======
  #=======
  # QC 3
  #=======
  #=======
  qc3 <- aesum %>% dplyr::mutate(TRT01A=paste0(TRT01A,'\\line (N=',DENOMCNT,')\\line n (%)')) %>%
    repfun::ru_denorm(varstodenorm=c("tt_result", "PERCENT"), groupbyvars=c("tt_summarylevel", "AEBODSYS", "AEDECOD"), acrossvar="TRT01AN",
                      acrossvarlabel="TRT01A", acrossvarprefix=c("tt_", "tt_p")) %>% {lapply(., attr, "label") ->> LBLS; .} %>% dplyr::arrange(AEBODSYS,tt_summarylevel,AEDECOD) %>%
    dplyr::select(-starts_with('tt_p'),-tt_summarylevel) %>% dplyr::group_by(AEBODSYS) %>% dplyr::group_modify(~ add_row(.x,.after=Inf)) %>%
    dplyr::ungroup() %>% dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(.x, ""))) %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    repfun::ru_labels(varlabels=LBLS,style='base') %>%
    repfun::ru_addpage(grpvars=c('AEBODSYS'),rowsprbdy=19)

  qc3 %>% group_by(AEBODSYS) %>% mutate(lastrow=ifelse(row_number()==n(),TRUE,FALSE),
                                        frstpage=ifelse(lastrow & !PAGEVAR==lag(PAGEVAR),TRUE,FALSE)) %>% filter(!(lastrow & frstpage)) %>% select(-c(lastrow,frstpage)) -> qc3

  qc3 <- as.data.frame(qc3)
  qc3 <- rbind(qc3 %>% mutate(PAGBYCAT='Gender: Male'),qc3 %>% mutate(PAGBYCAT='Gender: Female'))
  attr(qc3$PAGEVAR, "label") <- NULL

  attr(qc3,'label') <- 'DD Dataframe for AE Table 3'
  qc3[] <- lapply(qc3, as.character)
  attr(qc3, 'label') <- NULL

  #==================
  # Test 3: Compare.
  #==================
  testthat::expect_equal(prod3, qc3)

  #=======================================================
  # Generate 2nd DD data set and compare with QC version.
  #=======================================================
  setup(2)
  SOCterms <- aesum %>% dplyr::distinct(AEBODSYS,AEDECOD)
  SOCcnts <- table(SOCterms$AEBODSYS)
  repfun::ru_list(aesum %>% dplyr::filter(!(AEBODSYS %in% names(SOCcnts[SOCcnts>=20]))),
                  display='Y',
                  columns=c('AEBODSYS','AEDECOD','tt_01','tt_02','tt_03','tt_99'),
                  nowidowvar='AEBODSYS',
                  widths=c(5.5,4.5,1.75,1.9,1.9,1.75),
                  skipvars=c('AEBODSYS'),
                  centrevars=c('tt_01','tt_02','tt_03','tt_99'),
                  ordervars=c('AEBODSYS','tt_summarylevel','AEDECOD'),
                  noprintvars=c('tt_summarylevel'),
                  denormyn='Y',
                  varsToDenorm=c('tt_result'),
                  groupByVars=c('AEBODSYS','tt_summarylevel','AEDECOD'),
                  acrossVar="TRT01AN",
                  acrossVarLabel="TRT01A",
                  acrossColVarPrefix='tt_',
                  dddatasetlabel=paste0('DD Dataframe for AE Table ',rfenv$G_DSPLYNUM),
                  lpp=24,
                  rpp=50)

  #======================
  # Read in PROD DDDATA.
  #======================
  #prod2 <- haven::read_xpt(gsub('rds','xpt',rfenv$G_DDDATA))
  prod <- readRDS(rfenv$G_DDDATA)
  prod2 <- prod %>% mutate_at(vars(AEBODSYS,AEDECOD,tt_01,tt_02,tt_03,tt_99),as.character) %>% ru_labels(.,varlabels=lapply(prod,attr,"label"))

  #====================
  # Produce QC DDDATA.
  #====================
  qc2 <- aesum %>% dplyr::filter(!(AEBODSYS %in% names(SOCcnts[SOCcnts>=20]))) %>% dplyr::mutate(TRT01A=paste0(TRT01A,'\\line (N=',DENOMCNT,')\\line n (%)')) %>%
    repfun::ru_denorm(varstodenorm=c("tt_result", "PERCENT"), groupbyvars=c("tt_summarylevel", "AEBODSYS", "AEDECOD"), acrossvar="TRT01AN",
                      acrossvarlabel="TRT01A", acrossvarprefix=c("tt_", "tt_p")) %>% {lapply(., attr, "label") ->> LBLS; .} %>% dplyr::arrange(AEBODSYS,tt_summarylevel,AEDECOD) %>%
    dplyr::select(-starts_with('tt_p'),-tt_summarylevel) %>% dplyr::group_by(AEBODSYS) %>% dplyr::group_modify(~ add_row(.x,.after=Inf)) %>%
    dplyr::ungroup() %>% dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(.x, ""))) %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>% repfun::ru_labels(varlabels=LBLS,style='base') %>%
    repfun::ru_addpage(grpvars=c('AEBODSYS'),rowsprbdy=21,,nosplitvars=TRUE)

  qc2 <- as.data.frame(qc2)
  attr(qc2,'label') <- 'DD Dataframe for AE Table 2'

  #==================
  # Test 2: Compare.
  #==================
  testthat::expect_equal(prod2, qc2)

  unlink(tmpdr, recursive = TRUE)

})
