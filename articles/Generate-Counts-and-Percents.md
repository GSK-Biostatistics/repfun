# Generate-Counts-and-Percents

## Vignette Build Datetime

``` r
message(paste0('Datetime: ',Sys.Date(),':',Sys.time()))
#> Datetime: 2026-01-07:2026-01-07 18:39:18.719788
```

## Load Libraries

``` r
library(repfun)
library(dplyr)
library(kableExtra)
```

## Set Up the Reporting Environment

``` r
tmpdr <- tempdir()
datdir <- file.path(gsub("\\","/",tmpdr,fixed=TRUE),"datdir")
dir.create(datdir,showWarnings=FALSE)
repfun::copydata(datdir)
repfun::rs_setup(D_POPDATA=repfun::adsl %>% dplyr::filter(SAFFL =='Y'), 
         D_SUBJID=c("STUDYID","USUBJID"),
         R_ADAMDATA=datdir)
```

## Update ADSL and ADAE

``` r
repfun:::rfenv$G_POPDATA %>% dplyr::mutate(TRT01AN=ifelse(TRT01A=='Placebo',1,ifelse(TRT01A=='Xanomeline Low Dose',2,3)),
                     SEXN=ifelse(SEX=='F',1,ifelse(SEX=='M',2,NA)),
                     RACEN=ifelse(RACE=='AMERICAN INDIAN OR ALASKA NATIVE',1,
                                  ifelse(RACE=='BLACK OR AFRICAN AMERICAN',2,
                                         ifelse(RACE=='WHITE',3,NA))),
                     AGEGR1N=ifelse(AGEGR1=='18-64',1,ifelse(AGEGR1=='>64',2,NA))) %>% 
              repfun::ru_labels(varlabels=list('TRT01AN'='Actual Treatment for Period 01 (n)','SEXN'='Sex (n)',
                                       'RACEN'='Race (n)','AGEGR1N'='Pooled Age Group 1 (n)')) -> G_POPDATA

G_POPDATA %>% dplyr::select(STUDYID,USUBJID,SAFFL,TRT01AN,TRT01A) -> G_POPDATA1
adae <- repfun:::rfenv$adamdata$adae.rda() %>% dplyr::filter(TRTEMFL=='Y') %>% 
  dplyr::inner_join(G_POPDATA1, by=c('STUDYID','USUBJID','SAFFL','TRT01A'))
```

## Generate Counts and Percents for AE Body System and Preferred Term

``` r
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
                  resultpctdps=2) %>% dplyr::select(TRT01AN,TRT01A,AEBODSYS,AEDECOD,NUMERCNT,
                                             DENOMCNT,PERCENT,tt_result) %>% 
                                      dplyr::arrange(TRT01AN,TRT01A,AEBODSYS,AEDECOD)
```

## Display the Results for AE Body System and Preferred Term

``` r
lbls <- sapply(aeprod,function(x){attr(x,"label")})
knitr::kable(head(aeprod,10), col.names=paste(names(lbls),lbls,sep=": "), 
             caption = "Counts and Percents for AEs by Body System and Preferred Term") %>%
    kable_styling(full_width = T) %>% column_spec(c(3,4,8), width_min = c('2in','2in','2in'))
```

| TRT01AN: Actual Treatment for Period 01 (n) | TRT01A: Actual Treatment for Period 01 | AEBODSYS: Body System or Organ Class | AEDECOD: Dictionary-Derived Term     | NUMERCNT: Numerator Count | DENOMCNT: Denominator Count | PERCENT: Percent | tt_result: Result |
|--------------------------------------------:|:---------------------------------------|:-------------------------------------|:-------------------------------------|--------------------------:|----------------------------:|-----------------:|:------------------|
|                                           1 | Placebo                                | ANY EVENT                            | ANY EVENT                            |                        65 |                          86 |        75.581395 | 65 (75.58%)       |
|                                           1 | Placebo                                | CARDIAC DISORDERS                    | ANY EVENT                            |                        12 |                          86 |        13.953488 | 12 (13.95%)       |
|                                           1 | Placebo                                | CARDIAC DISORDERS                    | ATRIAL FIBRILLATION                  |                         1 |                          86 |         1.162791 | 1 (1.16%)         |
|                                           1 | Placebo                                | CARDIAC DISORDERS                    | ATRIAL FLUTTER                       |                         0 |                          86 |         0.000000 | 0 (0.00%)         |
|                                           1 | Placebo                                | CARDIAC DISORDERS                    | ATRIAL HYPERTROPHY                   |                         1 |                          86 |         1.162791 | 1 (1.16%)         |
|                                           1 | Placebo                                | CARDIAC DISORDERS                    | ATRIOVENTRICULAR BLOCK FIRST DEGREE  |                         1 |                          86 |         1.162791 | 1 (1.16%)         |
|                                           1 | Placebo                                | CARDIAC DISORDERS                    | ATRIOVENTRICULAR BLOCK SECOND DEGREE |                         1 |                          86 |         1.162791 | 1 (1.16%)         |
|                                           1 | Placebo                                | CARDIAC DISORDERS                    | BRADYCARDIA                          |                         1 |                          86 |         1.162791 | 1 (1.16%)         |
|                                           1 | Placebo                                | CARDIAC DISORDERS                    | BUNDLE BRANCH BLOCK LEFT             |                         1 |                          86 |         1.162791 | 1 (1.16%)         |
|                                           1 | Placebo                                | CARDIAC DISORDERS                    | BUNDLE BRANCH BLOCK RIGHT            |                         1 |                          86 |         1.162791 | 1 (1.16%)         |

Counts and Percents for AEs by Body System and Preferred Term

## Generate Counts and Percents for AE Preferred Term Only

``` r
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
                   resultpctdps=2) %>% dplyr::select(TRT01AN,TRT01A,AEDECOD,NUMERCNT,
                                              DENOMCNT,PERCENT,tt_result) %>% 
                                       dplyr::arrange(TRT01AN,TRT01A,AEDECOD)
```

## Display the Results for AE Preferred Term Counts and Percents

``` r
lbls <- sapply(aeprod2,function(x){attr(x,"label")})
knitr::kable(head(aeprod2,10), col.names=paste(names(lbls),lbls,sep=": "), 
             caption = "Counts and Percents for AEs by Preferred Term Only") %>%
  kable_styling(full_width = T) %>% column_spec(c(3,7), width_min = c('2in','2in'))
```

| TRT01AN: Actual Treatment for Period 01 (n) | TRT01A: Actual Treatment for Period 01 | AEDECOD: Dictionary-Derived Term | NUMERCNT: Numerator Count | DENOMCNT: Denominator Count | PERCENT: Percent | tt_result: Result |
|--------------------------------------------:|:---------------------------------------|:---------------------------------|--------------------------:|----------------------------:|-----------------:|:------------------|
|                                           1 | Placebo                                | ABDOMINAL DISCOMFORT             |                         0 |                          86 |         0.000000 | 0 (0.00%)         |
|                                           1 | Placebo                                | ABDOMINAL PAIN                   |                         1 |                          86 |         1.162791 | 1 (1.16%)         |
|                                           1 | Placebo                                | ACROCHORDON EXCISION             |                         0 |                          86 |         0.000000 | 0 (0.00%)         |
|                                           1 | Placebo                                | ACTINIC KERATOSIS                |                         0 |                          86 |         0.000000 | 0 (0.00%)         |
|                                           1 | Placebo                                | AGITATION                        |                         2 |                          86 |         2.325581 | 2 (2.33%)         |
|                                           1 | Placebo                                | ALCOHOL USE                      |                         0 |                          86 |         0.000000 | 0 (0.00%)         |
|                                           1 | Placebo                                | ALLERGIC GRANULOMATOUS ANGIITIS  |                         0 |                          86 |         0.000000 | 0 (0.00%)         |
|                                           1 | Placebo                                | ALOPECIA                         |                         1 |                          86 |         1.162791 | 1 (1.16%)         |
|                                           1 | Placebo                                | AMNESIA                          |                         0 |                          86 |         0.000000 | 0 (0.00%)         |
|                                           1 | Placebo                                | ANXIETY                          |                         0 |                          86 |         0.000000 | 0 (0.00%)         |

Counts and Percents for AEs by Preferred Term Only

## Generate Counts and Percents for Demographic Data

``` r
G_POPDATA %>% dplyr::select(STUDYID,USUBJID,SAFFL,TRT01AN,TRT01A,SEXN,SEX,
                     RACEN,RACE,AGEGR1N,AGEGR1) -> G_POPDATA2
dflst <- list()
basechar <- data.frame()
i <- 1
for(v in c('AGEGR1','SEX','RACE')){
  lbl <- attr(G_POPDATA[[v]],'label')
  dflst[[v]] <- repfun::ru_freq(G_POPDATA2, dsetindenom=G_POPDATA2, 
                        countdistinctvars=c("STUDYID", "USUBJID"),
                        groupbyvarsnumer=c("STUDYID", "TRT01AN", paste0(v,'N')),
                        anyeventvars = NULL, anyeventvalues = NULL, groupminmaxvar=NULL,
                        totalforvar=c("TRT01AN"), totalid = 99, totaldecode = 'Total',
                        groupbyvarsdenom=c("STUDYID", "TRT01AN"), resultstyle="NUMERPCT", 
                        codedecodevarpairs=c("TRT01AN", "TRT01A", paste0(v,'N'), v),
                        varcodelistpairs=c(""), codelistnames=list(), resultpctdps=0) %>% 
    {. ->> LBLS} %>%
    dplyr::mutate(tt_avid=i, tt_avnm=lbl) %>%
    rename(tt_svid=as.name(paste0(v,'N')), tt_svnm=as.name(v)) %>%
    dplyr::select(tt_avid,tt_avnm,TRT01AN,TRT01A,tt_svid,tt_svnm,NUMERCNT,
           DENOMCNT,PERCENT,tt_result) %>%
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
  basechar <- repfun::ru_labels(basechar,varlabels=list('tt_svid'='Variable Order',
                                                'tt_svnm'='Variable Name',
                                                'tt_avid'='Value Order',
                                                'tt_avnm'='Value Name')) %>% 
              dplyr::arrange(tt_avid,tt_svid)
```

## Display the Results for Counts and Percents of Demographic Data

``` r
lbls <- sapply(basechar,function(x){attr(x,"label")})
knitr::kable(head(basechar,10), col.names=paste(names(lbls),lbls,sep=": "), 
             caption = "Counts and Percents for Demographic Data") %>%
  kable_styling(full_width = T) %>% 
  column_spec(c(2,4,6,10), width_min = c('2in','2in','3in','2in'))
```

| tt_avid: Value Order | tt_avnm: Value Name       | TRT01AN: Actual Treatment for Period 01 (n) | TRT01A: Actual Treatment for Period 01 | tt_svid: Variable Order | tt_svnm: Variable Name | NUMERCNT: Numerator Count | DENOMCNT: Denominator Count | PERCENT: Percent | tt_result: Result |
|---------------------:|:--------------------------|--------------------------------------------:|:---------------------------------------|------------------------:|:-----------------------|--------------------------:|----------------------------:|-----------------:|:------------------|
|                    1 | Pooled Age Group 1, n (%) |                                           1 | Placebo                                |                       1 | 18-64                  |                        14 |                          86 |        16.279070 | 14 (16%)          |
|                    1 | Pooled Age Group 1, n (%) |                                           2 | Xanomeline Low Dose                    |                       1 | 18-64                  |                         8 |                          96 |         8.333333 | 8 (8%)            |
|                    1 | Pooled Age Group 1, n (%) |                                           3 | Xanomeline High Dose                   |                       1 | 18-64                  |                        11 |                          72 |        15.277778 | 11 (15%)          |
|                    1 | Pooled Age Group 1, n (%) |                                          99 | Total                                  |                       1 | 18-64                  |                        33 |                         254 |        12.992126 | 33 (13%)          |
|                    1 | Pooled Age Group 1, n (%) |                                           1 | Placebo                                |                       2 | \>64                   |                        72 |                          86 |        83.720930 | 72 (84%)          |
|                    1 | Pooled Age Group 1, n (%) |                                           2 | Xanomeline Low Dose                    |                       2 | \>64                   |                        88 |                          96 |        91.666667 | 88 (92%)          |
|                    1 | Pooled Age Group 1, n (%) |                                           3 | Xanomeline High Dose                   |                       2 | \>64                   |                        61 |                          72 |        84.722222 | 61 (85%)          |
|                    1 | Pooled Age Group 1, n (%) |                                          99 | Total                                  |                       2 | \>64                   |                       221 |                         254 |        87.007874 | 221 (87%)         |
|                    2 | Sex, n (%)                |                                           1 | Placebo                                |                       1 | F                      |                        53 |                          86 |        61.627907 | 53 (62%)          |
|                    2 | Sex, n (%)                |                                           2 | Xanomeline Low Dose                    |                       1 | F                      |                        55 |                          96 |        57.291667 | 55 (57%)          |

Counts and Percents for Demographic Data
