test_that("environment setup works", {

  suppressMessages(library(repfun))
  suppressMessages(library(dplyr))
  suppressMessages(library(tibble))
  suppressMessages(library(testthat))

  #globs <- paste0(tempdir(),'/GLOBALS2.txt')
  #write(getwd(),file=globs,append=TRUE)
  #write(testthat::test_path(),file=globs,append=TRUE)
  #write(paste0(getwd(),'/',testthat::test_path()),file=globs,append=TRUE)
  #Sys.sleep(300)

  #repfun::setpath(paste0(rfenv$PATH,'/tests/testthat'))
  repfun::setpath(paste0(rfenv$PATH,'/tests/testthat'))
  #print(getwd())
  #Sys.sleep(20)

  #======================
  # Invoke setup macro.
  #======================
  tmpdr <- tempdir()
  suppressMessages(
    repfun::rs_setup(
      D_CENTID="SITEID",
      D_DATADATE=Sys.Date(),
      D_DSPLYNUM=1,
      D_DSPLYTYP=T,
      D_FONTSIZE=10,
      D_FOOT1='1.) Only treatment emergent events related to lipids are displayed.',
      D_FOOT2='2.) Subjects are only counted once within each body system and preferred term.',
      D_KEEPPOPVARS=c('STUDYID','USUBJID','SAFFL'),
      D_OUTFILE=paste0(tmpdr,"/t_ru_list_1.rtf"),
      D_PGMPTH="./test-rs_setup.R",
      D_STUDYID='ABCXYZPDQ',
      D_POP="SAFFL",
      D_POPDATA=repfun::adsl %>% dplyr::filter(SAFFL=='Y'),
      D_POPLBL="Safety",
      D_SUBJID=c("STUDYID","USUBJID"),
      D_TITLE1=paste0('Table 1: Summary of Treatment Emergent Adverse Events'),
      D_USERID=Sys.getenv("DOMINO_USER_NAME"),
      R_DICTION="../../inst/formats",
      R_MACDIRS=NULL,
      R_DDDATA=paste0(tmpdr,'/t_ru_list_1.rds'),
      R_OTHERDATA=".",
      R_INPUTDATA=".",
      R_RAWDATA=".",
      R_SDTMDATA=".",
      R_ADAMDATA=".",
      D_RTFYN="Y",
      D_DEBUG=0)
  )

  #print(getwd())
  #print(adamdata)

  #=========================================================
  # Read ADSL directly, subset and compare with G_POPDATA.
  #=========================================================
  #indata <- adamdata$adsl.rda()
  indata <- do.call(rfenv$adamdata$adsl.rda,list())
  indata <- tibble::as_tibble(indata)
  indata %>% dplyr::filter(SAFFL=='Y') -> C_POPDATA

  #========
  # Checks
  #========
  testthat::expect_equal(rfenv$G_POPDATA, C_POPDATA)
  testthat::expect_equal(rfenv$G_STUDYID, 'ABCXYZPDQ')

})
