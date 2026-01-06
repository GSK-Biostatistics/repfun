# Denormalize-A-DataFrame

## Vignette Build Datetime

``` r
message(paste0('Datetime: ',Sys.Date(),':',Sys.time()))
#> Datetime: 2026-01-06:2026-01-06 21:02:16.489728
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
repfun::rs_setup(D_POP="SAFFL",D_POPLBL="Safety",
         D_POPDATA=repfun::adsl %>% dplyr::filter(SAFFL =='Y'), 
         D_SUBJID=c("STUDYID","USUBJID"), R_ADAMDATA=datdir)
repfun:::rfenv$G_POPDATA %>% dplyr::mutate(TRT01AN=ifelse(TRT01A=='Placebo',1,ifelse(TRT01A=='Xanomeline Low Dose',2,3))) %>% repfun::ru_labels(varlabels=list('TRT01AN'='Actual Treatment for Period 01 (n)')) -> G_POPDATA
```

## Read in ADAE and Apply Population

``` r
adae <- repfun:::rfenv$adamdata$adae.rda() %>% select(-SAFFL) %>% 
  repfun::ru_getdata(G_POPDATA, c("STUDYID", "USUBJID"), keeppopvars=c("TRT01AN", "TRT01A"))
```

## Generate Counts and Percents for AE Body System and Preferred Term

``` r
aesum <- repfun::ru_freq(adae,
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
                 resultpctdps=0)
```

## Denormalize the AE Counts and Percents Data Set

``` r
aesum_t <- repfun::ru_denorm(aesum,varstodenorm=c("tt_result", "PERCENT"), 
                     groupbyvars=c("tt_summarylevel", "AEBODSYS", "AEDECOD"), 
                     acrossvar="TRT01AN", acrossvarlabel="TRT01A", 
                     acrossvarprefix=c("tt_ac", "tt_p")) %>% 
  dplyr::arrange(tt_summarylevel, AEBODSYS, AEDECOD)
```

## Display the Denormalized AE Counts and Percents Data Set

``` r
lbls <- sapply(aesum_t,function(x){attr(x,"label")})
knitr::kable(head(aesum_t,10), col.names=paste(names(lbls),lbls,sep=": "), 
             caption = "Denormalized Data Set for Counts and Percents") %>%  
  kable_styling(full_width = T) %>% column_spec(c(2,3), width_min = c('2in','2in'))
```

| tt_summarylevel: Summary Level | AEBODSYS: Body System or Organ Class                 | AEDECOD: Dictionary-Derived Term | tt_ac01: Placebo | tt_p01: Placebo | tt_ac02: Xanomeline Low Dose | tt_p02: Xanomeline Low Dose | tt_ac03: Xanomeline High Dose | tt_p03: Xanomeline High Dose | tt_ac99: Total | tt_p99: Total |
|-------------------------------:|:-----------------------------------------------------|:---------------------------------|:-----------------|----------------:|:-----------------------------|----------------------------:|:------------------------------|-----------------------------:|:---------------|--------------:|
|                              0 | ANY EVENT                                            | ANY EVENT                        | 69 (80%)         |       80.232558 | 86 (90%)                     |                   89.583333 | 70 (97%)                      |                    97.222222 | 225 (89%)      |    88.5826772 |
|                              1 | CARDIAC DISORDERS                                    | ANY EVENT                        | 13 (15%)         |       15.116279 | 16 (17%)                     |                   16.666667 | 15 (21%)                      |                    20.833333 | 44 (17%)       |    17.3228346 |
|                              1 | CONGENITAL, FAMILIAL AND GENETIC DISORDERS           | ANY EVENT                        | 0 (0%)           |        0.000000 | 1 (1%)                       |                    1.041667 | 2 (3%)                        |                     2.777778 | 3 (1%)         |     1.1811024 |
|                              1 | EAR AND LABYRINTH DISORDERS                          | ANY EVENT                        | 1 (1%)           |        1.162791 | 2 (2%)                       |                    2.083333 | 1 (1%)                        |                     1.388889 | 4 (2%)         |     1.5748031 |
|                              1 | EYE DISORDERS                                        | ANY EVENT                        | 4 (5%)           |        4.651163 | 2 (2%)                       |                    2.083333 | 1 (1%)                        |                     1.388889 | 7 (3%)         |     2.7559055 |
|                              1 | GASTROINTESTINAL DISORDERS                           | ANY EVENT                        | 17 (20%)         |       19.767442 | 16 (17%)                     |                   16.666667 | 20 (28%)                      |                    27.777778 | 53 (21%)       |    20.8661417 |
|                              1 | GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS | ANY EVENT                        | 21 (24%)         |       24.418605 | 51 (53%)                     |                   53.125000 | 36 (50%)                      |                    50.000000 | 108 (43%)      |    42.5196850 |
|                              1 | HEPATOBILIARY DISORDERS                              | ANY EVENT                        | 1 (1%)           |        1.162791 | 0 (0%)                       |                    0.000000 | 0 (0%)                        |                     0.000000 | 1 (0%)         |     0.3937008 |
|                              1 | IMMUNE SYSTEM DISORDERS                              | ANY EVENT                        | 0 (0%)           |        0.000000 | 1 (1%)                       |                    1.041667 | 1 (1%)                        |                     1.388889 | 2 (1%)         |     0.7874016 |
|                              1 | INFECTIONS AND INFESTATIONS                          | ANY EVENT                        | 16 (19%)         |       18.604651 | 10 (10%)                     |                   10.416667 | 13 (18%)                      |                    18.055556 | 39 (15%)       |    15.3543307 |

Denormalized Data Set for Counts and Percents

## Derive Summary Statistics for Baseline Characteristics Data

``` r
demstats <- repfun::ru_sumstats(G_POPDATA,
                        analysisvars=c("AGE","TRTDURD"),
                        groupbyvars=c("STUDYID","TRT01AN"),
                        codedecodevarpairs=c("TRT01AN", "TRT01A"),
                        totalforvar="TRT01AN", totalid=99,
                        totaldecode="Total",
                        statsinrowsyn = "Y",
                        analysisvardps=list("AGE"=1,"TRTDURD"=2),
                        statslist=c("n", "mean", "median", "sd", "min", "max"))
```

## Denormalize the Baseline Characteristics Summary Statistics Data Set

``` r
demprod_t <- repfun::ru_denorm(demstats, varstodenorm=c("tt_result"), 
                       groupbyvars=c("tt_avid", "tt_avnm", "tt_svid", "tt_svnm"),
                       acrossvar="TRT01AN", acrossvarlabel="TRT01A", 
                       acrossvarprefix=c("tt_ac"))
```

## Display the Denormalized Baseline Characteristics Summary Statistics Data Set

``` r
lbls <- sapply(demprod_t,function(x){attr(x,"label")})
knitr::kable(head(demprod_t,10), col.names=paste(names(lbls),lbls,sep=": "), 
             caption = "Denormalized Data Set for Baseline Characteristics Summary Statistics") %>%   
  kable_styling(full_width = T) %>% column_spec(c(2), width_min = c('3in'))
```

| tt_avid: Analysis Variable ID | tt_avnm: Analysis Variable Name | tt_svid: Statistical Parameter ID | tt_svnm: Statistical Parameter Name | tt_ac01: Placebo | tt_ac02: Xanomeline Low Dose | tt_ac03: Xanomeline High Dose | tt_ac99: Total |
|------------------------------:|:--------------------------------|----------------------------------:|:------------------------------------|:-----------------|:-----------------------------|:------------------------------|:---------------|
|                             1 | AGE                             |                                 1 | n                                   | 86               | 96                           | 72                            | 254            |
|                             1 | AGE                             |                                 2 | Mean                                | 75.21            | 75.96                        | 73.78                         | 75.09          |
|                             1 | AGE                             |                                 3 | Median                              | 76.00            | 78.00                        | 75.50                         | 77.00          |
|                             1 | AGE                             |                                 4 | SD                                  | 8.590            | 8.114                        | 7.944                         | 8.246          |
|                             1 | AGE                             |                                 5 | Min                                 | 52.0             | 51.0                         | 56.0                          | 51.0           |
|                             1 | AGE                             |                                 6 | Max                                 | 89.0             | 88.0                         | 88.0                          | 89.0           |
|                             2 | TRTDURD                         |                                 1 | n                                   | 85               | 95                           | 72                            | 252            |
|                             2 | TRTDURD                         |                                 2 | Mean                                | 149.541          | 86.811                       | 112.222                       | 115.230        |
|                             2 | TRTDURD                         |                                 3 | Median                              | 182.000          | 63.000                       | 96.500                        | 132.000        |
|                             2 | TRTDURD                         |                                 4 | SD                                  | 60.3544          | 70.4737                      | 65.5233                       | 70.7137        |

Denormalized Data Set for Baseline Characteristics Summary Statistics
