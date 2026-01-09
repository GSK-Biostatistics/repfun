# Convert-Strings-to-Dates

## Vignette Build Datetime

``` r
message(paste0('Datetime: ',Sys.Date(),':',Sys.time()))
#> Datetime: 2026-01-09:2026-01-09 19:53:43.775715
```

## Load Libraries

``` r
library(repfun)
library(dplyr)
```

## Define library as a libname

``` r
tmpdr <- tempdir()
datdir <- file.path(gsub("\\","/",tmpdr,fixed=TRUE),"datdir")
dir.create(datdir,showWarnings=FALSE)
repfun::copydata(datdir)
sdtmdata <- repfun::ru_libname(datdir)
```

## Use Libname to Access SDTM.AE Data

``` r
sdtm.ae <- sdtmdata$ae.rda() 
```

## Add times to the character dates

``` r
sdtm.ae %>% mutate(AEDTC=ifelse(nchar(AEDTC)==10,paste0(AEDTC,'T12:30:50'),AEDTC),
                   AESTDTC=ifelse(nchar(AESTDTC)==10,paste0(AESTDTC,'T00:00:01'),AESTDTC),
                   AEENDTC=ifelse(nchar(AEENDTC)==10,paste0(AEENDTC,'T00:00:02'),AEENDTC)) -> sdtm.ae2
```

## Convert character dates to formatted R date variables

``` r
sdtm.ae3 <- repfun::ru_datetime(sdtm.ae2)
```

## Display the results

``` r
knitr::kable(head(sdtm.ae3[,grepl('(DT$|TM$|DTC$)',names(sdtm.ae3))],10), 
             caption = "SDTM.AE with Character Dates/Times Converted")
```

| AEDTC               | AESTDTC             | AEENDTC             | AEDT       |     AETM | AEDTM               | AESTDT     |   AESTTM | AESTDTM             | AEENDT     |   AEENTM | AEENDTM             |
|:--------------------|:--------------------|:--------------------|:-----------|---------:|:--------------------|:-----------|---------:|:--------------------|:-----------|---------:|:--------------------|
| 2014-01-16T12:30:50 | 2014-01-03T00:00:01 | NA                  | 2014-01-16 | 12:30:50 | 2014-01-16 12:30:50 | 2014-01-03 | 00:00:01 | 2014-01-03 00:00:01 | NA         |       NA | NA                  |
| 2014-01-16T12:30:50 | 2014-01-03T00:00:01 | NA                  | 2014-01-16 | 12:30:50 | 2014-01-16 12:30:50 | 2014-01-03 | 00:00:01 | 2014-01-03 00:00:01 | NA         |       NA | NA                  |
| 2014-01-16T12:30:50 | 2014-01-09T00:00:01 | 2014-01-11T00:00:02 | 2014-01-16 | 12:30:50 | 2014-01-16 12:30:50 | 2014-01-09 | 00:00:01 | 2014-01-09 00:00:01 | 2014-01-11 | 00:00:02 | 2014-01-11 00:00:02 |
| 2012-08-27T12:30:50 | 2012-08-26T00:00:01 | NA                  | 2012-08-27 | 12:30:50 | 2012-08-27 12:30:50 | 2012-08-26 | 00:00:01 | 2012-08-26 00:00:01 | NA         |       NA | NA                  |
| 2012-08-27T12:30:50 | 2012-08-07T00:00:01 | 2012-08-30T00:00:02 | 2012-08-27 | 12:30:50 | 2012-08-27 12:30:50 | 2012-08-07 | 00:00:01 | 2012-08-07 00:00:01 | 2012-08-30 | 00:00:02 | 2012-08-30 00:00:02 |
| 2012-08-27T12:30:50 | 2012-08-07T00:00:01 | NA                  | 2012-08-27 | 12:30:50 | 2012-08-27 12:30:50 | 2012-08-07 | 00:00:01 | 2012-08-07 00:00:01 | NA         |       NA | NA                  |
| 2012-09-02T12:30:50 | 2012-08-07T00:00:01 | 2012-08-30T00:00:02 | 2012-09-02 | 12:30:50 | 2012-09-02 12:30:50 | 2012-08-07 | 00:00:01 | 2012-08-07 00:00:01 | 2012-08-30 | 00:00:02 | 2012-08-30 00:00:02 |
| 2013-08-01T12:30:50 | 2013-07-21T00:00:01 | NA                  | 2013-08-01 | 12:30:50 | 2013-08-01 12:30:50 | 2013-07-21 | 00:00:01 | 2013-07-21 00:00:01 | NA         |       NA | NA                  |
| 2013-08-14T12:30:50 | 2013-08-08T00:00:01 | NA                  | 2013-08-14 | 12:30:50 | 2013-08-14 12:30:50 | 2013-08-08 | 00:00:01 | 2013-08-08 00:00:01 | NA         |       NA | NA                  |
| 2014-09-25T12:30:50 | 2014-08-27T00:00:01 | NA                  | 2014-09-25 | 12:30:50 | 2014-09-25 12:30:50 | 2014-08-27 | 00:00:01 | 2014-08-27 00:00:01 | NA         |       NA | NA                  |

SDTM.AE with Character Dates/Times Converted
