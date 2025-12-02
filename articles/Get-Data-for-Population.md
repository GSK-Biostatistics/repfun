# Get-Data-for-Population

## Vignette Build Datetime

``` r
message(paste0('Datetime: ',Sys.Date(),':',Sys.time()))
#> Datetime: 2025-12-02:2025-12-02 19:33:19.995786
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
repfun::rs_setup(D_POP="SAFFL",D_POPLBL="Safety",D_POPDATA=repfun::adsl, 
         D_SUBJID=c("STUDYID","USUBJID"), R_ADAMDATA=datdir)
repfun:::rfenv$G_POPDATA %>% dplyr::mutate(TRT01AN=ifelse(TRT01A=='Placebo',1,ifelse(TRT01A=='Xanomeline Low Dose',2,3)),
                     SAFFL=ifelse((row_number() %% 10) == 0,'N',SAFFL)) %>% 
              repfun::ru_labels(varlabels=list('TRT01AN'='Actual Treatment for Period 01 (n)',
                                       'SAFFL'='Safety Population Flag')) -> G_POPDATA
```

## Read in ADAE, Restrict to Population and Add Population Variables

``` r
adae <- repfun:::rfenv$adamdata$adae.rda() %>% dplyr::select(-c('SAFFL','TRT01A')) %>% 
  repfun::ru_getdata(G_POPDATA, c("STUDYID", "USUBJID"), 
             keeppopvars=c("TRT01AN", "TRT01A")) %>%
 dplyr::select(STUDYID,USUBJID,AEBODSYS,AEDECOD,SAFFL,TRT01AN,TRT01A) %>% 
  dplyr::arrange(STUDYID,USUBJID,AEBODSYS,AEDECOD,SAFFL,TRT01AN,TRT01A)
```

## Display the Results for AE Body System and Preferred Term

``` r
lbls <- sapply(adae,function(x){attr(x,"label")})
knitr::kable(head(adae,10), col.names=paste(names(lbls),lbls,sep=": "), 
             caption = "Results of Restricting to Population and Adding Population Variables") 
```

| STUDYID: Study Identifier | USUBJID: Unique Subject Identifier | AEBODSYS: Body System or Organ Class                 | AEDECOD: Dictionary-Derived Term     | SAFFL: Safety Population Flag | TRT01AN: Actual Treatment for Period 01 (n) | TRT01A: Actual Treatment for Period 01 |
|:--------------------------|:-----------------------------------|:-----------------------------------------------------|:-------------------------------------|:------------------------------|--------------------------------------------:|:---------------------------------------|
| CDISCPILOT01              | 01-701-1015                        | GASTROINTESTINAL DISORDERS                           | DIARRHOEA                            | Y                             |                                           1 | Placebo                                |
| CDISCPILOT01              | 01-701-1015                        | GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS | APPLICATION SITE ERYTHEMA            | Y                             |                                           1 | Placebo                                |
| CDISCPILOT01              | 01-701-1015                        | GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS | APPLICATION SITE PRURITUS            | Y                             |                                           1 | Placebo                                |
| CDISCPILOT01              | 01-701-1023                        | CARDIAC DISORDERS                                    | ATRIOVENTRICULAR BLOCK SECOND DEGREE | Y                             |                                           1 | Placebo                                |
| CDISCPILOT01              | 01-701-1023                        | SKIN AND SUBCUTANEOUS TISSUE DISORDERS               | ERYTHEMA                             | Y                             |                                           1 | Placebo                                |
| CDISCPILOT01              | 01-701-1023                        | SKIN AND SUBCUTANEOUS TISSUE DISORDERS               | ERYTHEMA                             | Y                             |                                           1 | Placebo                                |
| CDISCPILOT01              | 01-701-1023                        | SKIN AND SUBCUTANEOUS TISSUE DISORDERS               | ERYTHEMA                             | Y                             |                                           1 | Placebo                                |
| CDISCPILOT01              | 01-701-1028                        | GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS | APPLICATION SITE ERYTHEMA            | Y                             |                                           3 | Xanomeline High Dose                   |
| CDISCPILOT01              | 01-701-1028                        | GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS | APPLICATION SITE PRURITUS            | Y                             |                                           3 | Xanomeline High Dose                   |
| CDISCPILOT01              | 01-701-1034                        | GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS | APPLICATION SITE PRURITUS            | Y                             |                                           3 | Xanomeline High Dose                   |

Results of Restricting to Population and Adding Population Variables
