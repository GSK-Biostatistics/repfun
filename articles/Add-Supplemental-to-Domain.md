# Add-Supplemental-to-Domain

## Vignette Build Datetime

``` r
message(paste0('Datetime: ',Sys.Date(),':',Sys.time()))
#> Datetime: 2025-12-11:2025-12-11 19:00:38.26475
```

## Load Libraries

``` r
library(repfun)
library(dplyr)
```

## Define data library

``` r
tmpdr <- tempdir()
datdir <- file.path(gsub("\\","/",tmpdr,fixed=TRUE),"datdir")
dir.create(datdir,showWarnings=FALSE)
repfun::copydata(datdir)
sdtmdata <- repfun::ru_libname(datdir)
```

## Read SDTM DM and SUPPDM domains

``` r
sdtm.dm <- sdtmdata$dm()
sdtm.suppdm <- sdtmdata$suppdm()
```

## Show Supplemental Data to Append

``` r
sdtm.suppdm %>% distinct(QNAM,QLABEL) -> usupp
knitr::kable(usupp, caption = "Supplemental Variables to be Added")
```

| QNAM     | QLABEL                                |
|:---------|:--------------------------------------|
| COMPLT16 | Completers of Week 16 Population Flag |
| COMPLT24 | Completers of Week 24 Population Flag |
| COMPLT8  | Completers of Week 8 Population Flag  |
| EFFICACY | Efficacy Population Flag              |
| ITT      | Intent to Treat Population Flag       |
| SAFETY   | Safety Population Flag                |

Supplemental Variables to be Added

## Combine DM and SUPPDM data sets

``` r
suppae <- repfun::ru_addsupp(dsetin=sdtm.dm,dsetinsupp=sdtm.suppdm)
```

## Display the Results

``` r
lbls <- sapply(suppae,function(x){attr(x,"label")})
knitr::kable(head(suppae,10), col.names=paste(names(lbls),lbls,sep=" "), 
             caption = "Add Supplemental Data to DM Domain")
```

| STUDYID Study Identifier | DOMAIN Domain Abbreviation | USUBJID Unique Subject Identifier | SUBJID Subject Identifier for the Study | RFSTDTC Subject Reference Start Date/Time | RFENDTC Subject Reference End Date/Time | RFXSTDTC Date/Time of First Study Treatment | RFXENDTC Date/Time of Last Study Treatment | RFICDTC Date/Time of Informed Consent | RFPENDTC Date/Time of End of Participation | DTHDTC Date/Time of Death | DTHFL Subject Death Flag | SITEID Study Site Identifier | AGE Age | AGEU Age Units | SEX Sex | RACE Race | ETHNIC Ethnicity       | ARMCD Planned Arm Code | ARM Description of Planned Arm | ACTARMCD Actual Arm Code | ACTARM Description of Actual Arm | COUNTRY Country | DMDTC Date/Time of Collection | DMDY Study Day of Collection | COMPLT16 Completers of Week 16 Population Flag | COMPLT24 Completers of Week 24 Population Flag | COMPLT8 Completers of Week 8 Population Flag | EFFICACY Efficacy Population Flag | ITT Intent to Treat Population Flag | SAFETY Safety Population Flag |
|:-------------------------|:---------------------------|:----------------------------------|:----------------------------------------|:------------------------------------------|:----------------------------------------|:--------------------------------------------|:-------------------------------------------|:--------------------------------------|:-------------------------------------------|:--------------------------|:-------------------------|:-----------------------------|--------:|:---------------|:--------|:----------|:-----------------------|:-----------------------|:-------------------------------|:-------------------------|:---------------------------------|:----------------|:------------------------------|-----------------------------:|:-----------------------------------------------|:-----------------------------------------------|:---------------------------------------------|:----------------------------------|:------------------------------------|:------------------------------|
| CDISCPILOT01             | DM                         | 01-701-1015                       | 1015                                    | 2014-01-02                                | 2014-07-02                              | 2014-01-02                                  | 2014-07-02                                 | NA                                    | 2014-07-02T11:45                           | NA                        | NA                       | 701                          |      63 | YEARS          | F       | WHITE     | HISPANIC OR LATINO     | Pbo                    | Placebo                        | Pbo                      | Placebo                          | USA             | 2013-12-26                    |                           -7 | Y                                              | Y                                              | Y                                            | Y                                 | Y                                   | Y                             |
| CDISCPILOT01             | DM                         | 01-701-1023                       | 1023                                    | 2012-08-05                                | 2012-09-02                              | 2012-08-05                                  | 2012-09-01                                 | NA                                    | 2013-02-18                                 | NA                        | NA                       | 701                          |      64 | YEARS          | M       | WHITE     | HISPANIC OR LATINO     | Pbo                    | Placebo                        | Pbo                      | Placebo                          | USA             | 2012-07-22                    |                          -14 | NA                                             | NA                                             | NA                                           | Y                                 | Y                                   | Y                             |
| CDISCPILOT01             | DM                         | 01-701-1028                       | 1028                                    | 2013-07-19                                | 2014-01-14                              | 2013-07-19                                  | 2014-01-14                                 | NA                                    | 2014-01-14T11:10                           | NA                        | NA                       | 701                          |      71 | YEARS          | M       | WHITE     | NOT HISPANIC OR LATINO | Xan_Hi                 | Xanomeline High Dose           | Xan_Hi                   | Xanomeline High Dose             | USA             | 2013-07-11                    |                           -8 | Y                                              | Y                                              | Y                                            | Y                                 | Y                                   | Y                             |
| CDISCPILOT01             | DM                         | 01-701-1033                       | 1033                                    | 2014-03-18                                | 2014-04-14                              | 2014-03-18                                  | 2014-03-31                                 | NA                                    | 2014-09-15                                 | NA                        | NA                       | 701                          |      74 | YEARS          | M       | WHITE     | NOT HISPANIC OR LATINO | Xan_Lo                 | Xanomeline Low Dose            | Xan_Lo                   | Xanomeline Low Dose              | USA             | 2014-03-10                    |                           -8 | NA                                             | NA                                             | NA                                           | Y                                 | Y                                   | Y                             |
| CDISCPILOT01             | DM                         | 01-701-1034                       | 1034                                    | 2014-07-01                                | 2014-12-30                              | 2014-07-01                                  | 2014-12-30                                 | NA                                    | 2014-12-30T09:50                           | NA                        | NA                       | 701                          |      77 | YEARS          | F       | WHITE     | NOT HISPANIC OR LATINO | Xan_Hi                 | Xanomeline High Dose           | Xan_Hi                   | Xanomeline High Dose             | USA             | 2014-06-24                    |                           -7 | Y                                              | Y                                              | Y                                            | Y                                 | Y                                   | Y                             |
| CDISCPILOT01             | DM                         | 01-701-1047                       | 1047                                    | 2013-02-12                                | 2013-03-29                              | 2013-02-12                                  | 2013-03-09                                 | NA                                    | 2013-07-28                                 | NA                        | NA                       | 701                          |      85 | YEARS          | F       | WHITE     | NOT HISPANIC OR LATINO | Pbo                    | Placebo                        | Pbo                      | Placebo                          | USA             | 2013-01-22                    |                          -21 | NA                                             | NA                                             | NA                                           | Y                                 | Y                                   | Y                             |
| CDISCPILOT01             | DM                         | 01-701-1057                       | 1057                                    | NA                                        | NA                                      | NA                                          | NA                                         | NA                                    | 2013-12-27                                 | NA                        | NA                       | 701                          |      59 | YEARS          | F       | WHITE     | HISPANIC OR LATINO     | Scrnfail               | Screen Failure                 | Scrnfail                 | Screen Failure                   | USA             | 2013-12-20                    |                           NA | NA                                             | NA                                             | NA                                           | NA                                | NA                                  | NA                            |
| CDISCPILOT01             | DM                         | 01-701-1097                       | 1097                                    | 2014-01-01                                | 2014-07-09                              | 2014-01-01                                  | 2014-07-09                                 | NA                                    | 2014-07-09T09:39                           | NA                        | NA                       | 701                          |      68 | YEARS          | M       | WHITE     | NOT HISPANIC OR LATINO | Xan_Lo                 | Xanomeline Low Dose            | Xan_Lo                   | Xanomeline Low Dose              | USA             | 2013-12-23                    |                           -9 | Y                                              | Y                                              | Y                                            | Y                                 | Y                                   | Y                             |
| CDISCPILOT01             | DM                         | 01-701-1111                       | 1111                                    | 2012-09-07                                | 2012-09-17                              | 2012-09-07                                  | 2012-09-16                                 | NA                                    | 2013-02-22                                 | NA                        | NA                       | 701                          |      81 | YEARS          | F       | WHITE     | NOT HISPANIC OR LATINO | Xan_Lo                 | Xanomeline Low Dose            | Xan_Lo                   | Xanomeline Low Dose              | USA             | 2012-08-25                    |                          -13 | NA                                             | NA                                             | NA                                           | Y                                 | Y                                   | Y                             |
| CDISCPILOT01             | DM                         | 01-701-1115                       | 1115                                    | 2012-11-30                                | 2013-01-23                              | 2012-11-30                                  | 2013-01-23                                 | NA                                    | 2013-05-20                                 | NA                        | NA                       | 701                          |      84 | YEARS          | M       | WHITE     | NOT HISPANIC OR LATINO | Xan_Lo                 | Xanomeline Low Dose            | Xan_Lo                   | Xanomeline Low Dose              | USA             | 2012-11-23                    |                           -7 | NA                                             | NA                                             | Y                                            | Y                                 | Y                                   | Y                             |

Add Supplemental Data to DM Domain
