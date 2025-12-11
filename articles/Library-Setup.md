# Library-Setup

## Vignette Build Datetime

``` r
message(paste0('Datetime: ',Sys.Date(),':',Sys.time()))
#> Datetime: 2025-12-11:2025-12-11 19:02:07.127053
```

## Load Libraries

``` r
library(repfun)
library(dplyr)
```

## Define Libnames

``` r
datdir <- file.path(gsub("\\","/",tempdir(),fixed=TRUE),"datdir")
dir.create(datdir,showWarnings=FALSE)
repfun::copydata(datdir)
adamdata <- repfun::ru_libname(datdir)
```

## Use Libname to Access and Subset Data

``` r
adsl <- adamdata$adsl.rda() %>% dplyr::filter(SAFFL=='Y') -> adsl1
```

## Display the Result

``` r
knitr::kable(head(adsl[,c(1:10)],10), caption = "Libname Style Access to ADSL")
```

| STUDYID      | USUBJID     | SUBJID | RFSTDTC    | RFENDTC    | RFXSTDTC   | RFXENDTC   | RFICDTC | RFPENDTC         | DTHDTC |
|:-------------|:------------|:-------|:-----------|:-----------|:-----------|:-----------|:--------|:-----------------|:-------|
| CDISCPILOT01 | 01-701-1015 | 1015   | 2014-01-02 | 2014-07-02 | 2014-01-02 | 2014-07-02 | NA      | 2014-07-02T11:45 | NA     |
| CDISCPILOT01 | 01-701-1023 | 1023   | 2012-08-05 | 2012-09-02 | 2012-08-05 | 2012-09-01 | NA      | 2013-02-18       | NA     |
| CDISCPILOT01 | 01-701-1028 | 1028   | 2013-07-19 | 2014-01-14 | 2013-07-19 | 2014-01-14 | NA      | 2014-01-14T11:10 | NA     |
| CDISCPILOT01 | 01-701-1033 | 1033   | 2014-03-18 | 2014-04-14 | 2014-03-18 | 2014-03-31 | NA      | 2014-09-15       | NA     |
| CDISCPILOT01 | 01-701-1034 | 1034   | 2014-07-01 | 2014-12-30 | 2014-07-01 | 2014-12-30 | NA      | 2014-12-30T09:50 | NA     |
| CDISCPILOT01 | 01-701-1047 | 1047   | 2013-02-12 | 2013-03-29 | 2013-02-12 | 2013-03-09 | NA      | 2013-07-28       | NA     |
| CDISCPILOT01 | 01-701-1097 | 1097   | 2014-01-01 | 2014-07-09 | 2014-01-01 | 2014-07-09 | NA      | 2014-07-09T09:39 | NA     |
| CDISCPILOT01 | 01-701-1111 | 1111   | 2012-09-07 | 2012-09-17 | 2012-09-07 | 2012-09-16 | NA      | 2013-02-22       | NA     |
| CDISCPILOT01 | 01-701-1115 | 1115   | 2012-11-30 | 2013-01-23 | 2012-11-30 | 2013-01-23 | NA      | 2013-05-20       | NA     |
| CDISCPILOT01 | 01-701-1118 | 1118   | 2014-03-12 | 2014-09-09 | 2014-03-12 | 2014-09-09 | NA      | 2014-09-09T13:28 | NA     |

Libname Style Access to ADSL
