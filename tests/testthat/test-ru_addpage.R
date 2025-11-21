test_that("adding page numbers works", {

  suppressMessages(library(repfun))
  suppressMessages(library(dplyr))
  suppressMessages(library(testthat))

  repfun::setpath(paste0(rfenv$PATH,'/tests/testthat'))

  #==================================================
  # Setup reporting environment and read in AE data.
  #==================================================
  rs_setup(D_POPDATA=repfun::adsl %>% dplyr::filter(SAFFL =='Y'), D_SUBJID=c("STUDYID","USUBJID"), R_ADAMDATA=".")
  rfenv$G_POPDATA %>% dplyr::mutate(TRT01AN=ifelse(TRT01A=='Placebo',1,ifelse(TRT01A=='Xanomeline Low Dose',2,3))) %>% repfun::ru_labels(varlabels=list('TRT01AN'='Actual Treatment for Period 01 (n)')) -> G_POPDATA
  adae <- rfenv$adamdata$adae.rda() %>% dplyr::inner_join(G_POPDATA, by=c('STUDYID','USUBJID','SAFFL','TRT01A'))
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
                                                           groupbyvars=c("tt_summarylevel", "AEBODSYS", "AEDECOD"),
                                                           acrossvar="TRT01AN",
                                                           acrossvarlabel="TRT01A",
                                                           acrossvarprefix=c("tt_ac", "tt_p")) %>%
                                         dplyr::mutate(ord1=ifelse(tt_summarylevel==0,0,1)) %>%
                                         dplyr::rename(ord2=tt_summarylevel) %>%
                                         dplyr::arrange(ord1,AEBODSYS,ord2,AEDECOD) %>%
                                         dplyr::select(-c(starts_with('tt_p'),starts_with('ord')))

  #==========================
  # Add page using function.
  #==========================
  prod <- repfun::ru_addpage(aesum_t,grpvars=c('AEBODSYS'),rowsprbdy=30)
  prod <- as.data.frame(prod)

  #====================
  # Add page manually.
  #====================
  qc <- dplyr::mutate(aesum_t, rownum=row_number())
  for (p in 1:10){
    qc[((p-1)*30 < qc$rownum) & (qc$rownum <= p*30),c('PAGEVAR')] <- p
  }
  qc$rownum <- NULL
  attr(qc$PAGEVAR,"label") <- "Page Variable"
  qc <- repfun::ru_labels(qc,varlabels=sapply(qc,function(x){attr(x,"label")}))
  qc <- as.data.frame(qc)

  #======================
  # Compare the results.
  #======================
  testthat::expect_equal(prod, qc)
})
