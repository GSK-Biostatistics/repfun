# Global-Reporting-Setup

## Vignette Build Datetime

``` r
message(paste0('Datetime: ',Sys.Date(),':',Sys.time()))
#> Datetime: 2025-12-02:2025-12-02 19:33:23.850642
```

## Load Libraries

``` r
library(repfun)
library(dplyr)
```

## Set Up the Global Environment

``` r
tmpdr <- tempdir()
datdir <- file.path(gsub("\\","/",tmpdr,fixed=TRUE),"datdir")
dir.create(datdir,showWarnings=FALSE)
repfun::copydata(datdir)
outdir <- file.path(gsub("\\","/",tmpdr,fixed=TRUE),"outdir")
dir.create(outdir,showWarnings=FALSE)
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
    D_OUTFILE=paste0(outdir,"/t_ru_list_1.rtf"),
    D_PGMPTH="./Global-Reporting-Setup.Rmd",
    D_STUDYID='ABCXYZPDQ',
    D_POP="SAFFL",
    D_POPDATA= repfun::adsl %>% dplyr::filter(SAFFL=='Y'),
    D_POPLBL="Safety",
    D_SUBJID=c("STUDYID","USUBJID"),
    D_TITLE1=paste0('Table 1: Summary of Treatment Emergent Adverse Events'),
    D_USERID=Sys.getenv("DOMINO_USER_NAME"),
    R_DICTION="../inst/formats",
    R_MACDIRS=c("."),
    R_DDDATA=paste0(tmpdr,'/t_ru_list_1.rds'),
    R_OTHERDATA=datdir,
    R_INPUTDATA=datdir,
    R_RAWDATA=datdir,
    R_SDTMDATA=datdir,
    R_ADAMDATA=datdir,
    D_RTFYN="Y",
    D_DEBUG=0)
    )
```

## Use Libname to Access and Subset Data

``` r
adae <- repfun:::rfenv$adamdata$adae.rda() %>% dplyr::filter(TRTEMFL=='Y') -> adsl1
```

## Display the Result, add Study ID to Title

``` r
knitr::kable(head(adae[,c(1:10)],10), caption = paste0("TEAEs for Study: ", repfun:::rfenv$G_STUDYID))
```

| STUDYID      | DOMAIN | USUBJID     | AESEQ | AESPID | AETERM                               | AELLT                     | AELLTCD | AEDECOD                              | AEPTCD |
|:-------------|:-------|:------------|------:|:-------|:-------------------------------------|:--------------------------|--------:|:-------------------------------------|-------:|
| CDISCPILOT01 | AE     | 01-701-1015 |     1 | E07    | APPLICATION SITE ERYTHEMA            | APPLICATION SITE REDNESS  |      NA | APPLICATION SITE ERYTHEMA            |     NA |
| CDISCPILOT01 | AE     | 01-701-1015 |     2 | E08    | APPLICATION SITE PRURITUS            | APPLICATION SITE ITCHING  |      NA | APPLICATION SITE PRURITUS            |     NA |
| CDISCPILOT01 | AE     | 01-701-1015 |     3 | E06    | DIARRHOEA                            | DIARRHEA                  |      NA | DIARRHOEA                            |     NA |
| CDISCPILOT01 | AE     | 01-701-1023 |     2 | E09    | ERYTHEMA                             | LOCALIZED ERYTHEMA        |      NA | ERYTHEMA                             |     NA |
| CDISCPILOT01 | AE     | 01-701-1023 |     1 | E08    | ERYTHEMA                             | ERYTHEMA                  |      NA | ERYTHEMA                             |     NA |
| CDISCPILOT01 | AE     | 01-701-1023 |     4 | E08    | ERYTHEMA                             | ERYTHEMA                  |      NA | ERYTHEMA                             |     NA |
| CDISCPILOT01 | AE     | 01-701-1023 |     3 | E10    | ATRIOVENTRICULAR BLOCK SECOND DEGREE | AV BLOCK SECOND DEGREE    |      NA | ATRIOVENTRICULAR BLOCK SECOND DEGREE |     NA |
| CDISCPILOT01 | AE     | 01-701-1028 |     1 | E04    | APPLICATION SITE ERYTHEMA            | APPLICATION SITE ERYTHEMA |      NA | APPLICATION SITE ERYTHEMA            |     NA |
| CDISCPILOT01 | AE     | 01-701-1028 |     2 | E05    | APPLICATION SITE PRURITUS            | APPLICATION SITE ITCHING  |      NA | APPLICATION SITE PRURITUS            |     NA |
| CDISCPILOT01 | AE     | 01-701-1034 |     1 | E08    | APPLICATION SITE PRURITUS            | APPLICATION SITE ITCHING  |      NA | APPLICATION SITE PRURITUS            |     NA |

TEAEs for Study: ABCXYZPDQ
