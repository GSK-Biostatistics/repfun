# Generate-Summary-Statistics

## Vignette Build Datetime

``` r
message(paste0('Datetime: ',Sys.Date(),':',Sys.time()))
#> Datetime: 2026-01-02:2026-01-02 10:24:36.350738
```

## Load Libraries

``` r
library(repfun)
library(dplyr)
```

## Set Up the Reporting Environment

``` r
tmpdr <- tempdir()
datdir <- file.path(gsub("\\","/",tmpdr,fixed=TRUE),"datdir")
dir.create(datdir,showWarnings=FALSE)
repfun::copydata(datdir)
repfun::rs_setup(D_POPDATA=repfun::adsl %>% 
           dplyr::filter(SAFFL =='Y'),
         D_SUBJID=c("STUDYID","USUBJID"),
         R_ADAMDATA=datdir)
```

## Update ADSL and ADVS

``` r
repfun:::rfenv$G_POPDATA %>% dplyr::mutate(TRT01AN=ifelse(TRT01A=='Placebo',1,ifelse(TRT01A=='Xanomeline Low Dose',2,3))) -> G_POPDATA
attr(G_POPDATA$TRT01AN,"label") <- 'Actual Treatment for Period 01 (n)'

repfun:::rfenv$adamdata$advs.rda() %>% dplyr::filter(ANL01FL=='Y') %>% 
  dplyr::mutate(TRT01AN=ifelse(TRT01A=='Placebo',1,ifelse(TRT01A=='Xanomeline Low Dose',2,3))) %>%
  dplyr::filter(!is.na(AVISITN) & (DTYPE=='AVERAGE')) -> advs2
attr(advs2$TRT01AN,"label") <- 'Actual Treatment for Period 01 (n)'
```

## Generate Summary Statistics for Baseline Characteristics

``` r
repfun::ru_sumstats(G_POPDATA,
            analysisvars=c("AGE","TRTDURD"),
            groupbyvars=c("STUDYID","TRT01AN"),
            codedecodevarpairs=c("TRT01AN", "TRT01A"),
            totalforvar="TRT01AN", totalid=99,
            totaldecode="Total",
            statsinrowsyn = "Y",
            analysisvardps=list("AGE"=1,"TRTDURD"=2),
            statslist=c("n", "mean", "median", "sd", "min", "max")) %>% 
   dplyr::arrange(tt_avid, TRT01AN,tt_svid) %>% dplyr::select(-tt_result_num) -> basechars
```

## Display the Results for Baseline Characteristics

``` r
lbls <- sapply(basechars,function(x){attr(x,"label")})
knitr::kable(head(basechars,10), col.names=paste(names(lbls),lbls,sep=" "), 
             caption = "Summary Statistics for Baseline Characteristics")
```

| STUDYID Study Identifier | TRT01AN Actual Treatment for Period 01 (n) | TRT01A Actual Treatment for Period 01 | tt_result Result | tt_svid Statistical Parameter ID | tt_svnm Statistical Parameter Name | tt_avid Analysis Variable ID | tt_avnm Analysis Variable Name |
|:-------------------------|-------------------------------------------:|:--------------------------------------|:-----------------|---------------------------------:|:-----------------------------------|-----------------------------:|:-------------------------------|
| CDISCPILOT01             |                                          1 | Placebo                               | 86               |                                1 | n                                  |                            1 | AGE                            |
| CDISCPILOT01             |                                          1 | Placebo                               | 75.21            |                                2 | Mean                               |                            1 | AGE                            |
| CDISCPILOT01             |                                          1 | Placebo                               | 76.00            |                                3 | Median                             |                            1 | AGE                            |
| CDISCPILOT01             |                                          1 | Placebo                               | 8.590            |                                4 | SD                                 |                            1 | AGE                            |
| CDISCPILOT01             |                                          1 | Placebo                               | 52.0             |                                5 | Min                                |                            1 | AGE                            |
| CDISCPILOT01             |                                          1 | Placebo                               | 89.0             |                                6 | Max                                |                            1 | AGE                            |
| CDISCPILOT01             |                                          2 | Xanomeline Low Dose                   | 96               |                                1 | n                                  |                            1 | AGE                            |
| CDISCPILOT01             |                                          2 | Xanomeline Low Dose                   | 75.96            |                                2 | Mean                               |                            1 | AGE                            |
| CDISCPILOT01             |                                          2 | Xanomeline Low Dose                   | 78.00            |                                3 | Median                             |                            1 | AGE                            |
| CDISCPILOT01             |                                          2 | Xanomeline Low Dose                   | 8.114            |                                4 | SD                                 |                            1 | AGE                            |

Summary Statistics for Baseline Characteristics

## Generate Summary Statistics for Vital Signs with Constant Precision

``` r
repfun::ru_sumstats(advs2,
            analysisvars=c("AVAL"),
            groupbyvars=c("STUDYID","TRT01AN","PARAMCD","AVISITN"),
            codedecodevarpairs=c("TRT01AN","TRT01A","PARAMCD","PARAM","AVISITN","AVISIT"),
            totalforvar="TRT01AN",
            totalid=99,
            totaldecode="Total",
            statsinrowsyn = "Y",
            analysisvardps=1,
            statslist=c("n","mean","median","sd","min","max")) %>% 
            dplyr::arrange(TRT01AN,PARAMCD,AVISITN, tt_svid) %>%     
            dplyr::select(-c('tt_avnm','tt_avid','tt_result_num')) -> vtlsigns
```

## Display the Results for Vital Signs with Constant Precision

``` r
lbls <- sapply(vtlsigns,function(x){attr(x,"label")})
knitr::kable(head(vtlsigns,10), col.names=paste(names(lbls),lbls,sep=" "), 
             caption = "Summary Statistics for Vital Signs with Constant Precision")
```

| STUDYID Study Identifier | TRT01AN Actual Treatment for Period 01 (n) | TRT01A Actual Treatment for Period 01 | PARAMCD Parameter Code | PARAM Parameter         | AVISITN Analysis Visit (N) | AVISIT Analysis Visit | tt_result Result | tt_svid Statistical Parameter ID | tt_svnm Statistical Parameter Name |
|:-------------------------|-------------------------------------------:|:--------------------------------------|:-----------------------|:------------------------|---------------------------:|:----------------------|:-----------------|---------------------------------:|:-----------------------------------|
| CDISCPILOT01             |                                          1 | Placebo                               | BMI                    | Body Mass Index(kg/m^2) |                          2 | Week 2                | 63               |                                1 | n                                  |
| CDISCPILOT01             |                                          1 | Placebo                               | BMI                    | Body Mass Index(kg/m^2) |                          2 | Week 2                | 23.29            |                                2 | Mean                               |
| CDISCPILOT01             |                                          1 | Placebo                               | BMI                    | Body Mass Index(kg/m^2) |                          2 | Week 2                | 23.04            |                                3 | Median                             |
| CDISCPILOT01             |                                          1 | Placebo                               | BMI                    | Body Mass Index(kg/m^2) |                          2 | Week 2                | 3.595            |                                4 | SD                                 |
| CDISCPILOT01             |                                          1 | Placebo                               | BMI                    | Body Mass Index(kg/m^2) |                          2 | Week 2                | 15.3             |                                5 | Min                                |
| CDISCPILOT01             |                                          1 | Placebo                               | BMI                    | Body Mass Index(kg/m^2) |                          2 | Week 2                | 33.6             |                                6 | Max                                |
| CDISCPILOT01             |                                          1 | Placebo                               | BMI                    | Body Mass Index(kg/m^2) |                          4 | Week 4                | 59               |                                1 | n                                  |
| CDISCPILOT01             |                                          1 | Placebo                               | BMI                    | Body Mass Index(kg/m^2) |                          4 | Week 4                | 23.17            |                                2 | Mean                               |
| CDISCPILOT01             |                                          1 | Placebo                               | BMI                    | Body Mass Index(kg/m^2) |                          4 | Week 4                | 23.01            |                                3 | Median                             |
| CDISCPILOT01             |                                          1 | Placebo                               | BMI                    | Body Mass Index(kg/m^2) |                          4 | Week 4                | 3.401            |                                4 | SD                                 |

Summary Statistics for Vital Signs with Constant Precision

## Generate Summary Statistics for Vital Signs with Varying Precision

``` r
decodes <- advs2 %>% distinct(PARAMCD, PARAM)
dcodelst <- split(decodes$PARAM, decodes$PARAMCD)
advs2 %>% dplyr::select(STUDYID,USUBJID,TRT01AN,TRT01A,PARAMCD,AVISITN,AVISIT,AVAL) %>% 
  dplyr::arrange(USUBJID,TRT01AN,TRT01A,AVISITN,AVISIT) %>%
  dplyr::group_by(USUBJID,TRT01AN,TRT01A,AVISITN,AVISIT) %>% 
  tidyr::pivot_wider(names_from=PARAMCD, values_from=AVAL) -> advs2_t
advs2_t <- repfun::ru_labels(advs2_t,varlabels=dcodelst)
repfun::ru_sumstats(advs2_t,
            analysisvars=c("BMI","BSA","DIABP","MAP","PULSE","SYSBP","TEMP","WEIGHT"),
            groupbyvars=c("STUDYID","TRT01AN","AVISITN"),
            codedecodevarpairs=c("TRT01AN","TRT01A","AVISITN","AVISIT"),
            totalforvar="TRT01AN", totalid=99,
            totaldecode="Total",
            statsinrowsyn = "Y",
            analysisvardps=list("BMI"=1,"BSA"=2,"DIABP"=3,"MAP"=4,"PULSE"=1,"SYSBP"=2,
                                "TEMP"=3,"WEIGHT"=4),
            statslist=c("n","mean","median","sd","min","max")) %>% 
  dplyr::left_join(decodes %>% dplyr::mutate(tt_avnm=PARAMCD),by='tt_avnm') %>% 
  dplyr::arrange(tt_avid, TRT01AN, AVISITN, tt_svid) %>%
  dplyr::select(-c('tt_result_num','tt_avnm')) %>% 
  dplyr::arrange(PARAMCD,TRT01AN,AVISITN) -> vtlsigns_t
```

## Display the Results for Vital Signs with Constant Precision

``` r
lbls <- sapply(vtlsigns_t,function(x){attr(x,"label")})
knitr::kable(head(vtlsigns_t,10), col.names=paste(names(lbls),lbls,sep=" "), 
             caption = "Summary Statistics for Vital Signs with Varying Precision")
```

| STUDYID Study Identifier | TRT01AN Actual Treatment for Period 01 (n) | TRT01A Actual Treatment for Period 01 | AVISITN Analysis Visit (N) | AVISIT Analysis Visit | tt_result Result | tt_svid Statistical Parameter ID | tt_svnm Statistical Parameter Name | tt_avid Analysis Variable ID | PARAMCD Parameter Code | PARAM Parameter         |
|:-------------------------|-------------------------------------------:|:--------------------------------------|---------------------------:|:----------------------|:-----------------|---------------------------------:|:-----------------------------------|-----------------------------:|:-----------------------|:------------------------|
| CDISCPILOT01             |                                          1 | Placebo                               |                          2 | Week 2                | 63               |                                1 | n                                  |                            1 | BMI                    | Body Mass Index(kg/m^2) |
| CDISCPILOT01             |                                          1 | Placebo                               |                          2 | Week 2                | 23.29            |                                2 | Mean                               |                            1 | BMI                    | Body Mass Index(kg/m^2) |
| CDISCPILOT01             |                                          1 | Placebo                               |                          2 | Week 2                | 23.04            |                                3 | Median                             |                            1 | BMI                    | Body Mass Index(kg/m^2) |
| CDISCPILOT01             |                                          1 | Placebo                               |                          2 | Week 2                | 3.595            |                                4 | SD                                 |                            1 | BMI                    | Body Mass Index(kg/m^2) |
| CDISCPILOT01             |                                          1 | Placebo                               |                          2 | Week 2                | 15.3             |                                5 | Min                                |                            1 | BMI                    | Body Mass Index(kg/m^2) |
| CDISCPILOT01             |                                          1 | Placebo                               |                          2 | Week 2                | 33.6             |                                6 | Max                                |                            1 | BMI                    | Body Mass Index(kg/m^2) |
| CDISCPILOT01             |                                          1 | Placebo                               |                          4 | Week 4                | 59               |                                1 | n                                  |                            1 | BMI                    | Body Mass Index(kg/m^2) |
| CDISCPILOT01             |                                          1 | Placebo                               |                          4 | Week 4                | 23.17            |                                2 | Mean                               |                            1 | BMI                    | Body Mass Index(kg/m^2) |
| CDISCPILOT01             |                                          1 | Placebo                               |                          4 | Week 4                | 23.01            |                                3 | Median                             |                            1 | BMI                    | Body Mass Index(kg/m^2) |
| CDISCPILOT01             |                                          1 | Placebo                               |                          4 | Week 4                | 3.401            |                                4 | SD                                 |                            1 | BMI                    | Body Mass Index(kg/m^2) |

Summary Statistics for Vital Signs with Varying Precision
