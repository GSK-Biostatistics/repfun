# Add-Big-N-to-DataFrame

## Vignette Build Datetime

``` r
message(paste0('Datetime: ',Sys.Date(),':',Sys.time()))
#> Datetime: 2025-12-02:2025-12-02 20:24:48.260548
```

## Load Libraries

``` r
library(repfun)
suppressMessages(library(dplyr))
library(kableExtra)
```

## Set Up the Reporting Environment

``` r
tmpdr <- tempdir()
datdir <- file.path(gsub("\\","/",tmpdr,fixed=TRUE),"datdir")
dir.create(datdir,showWarnings=FALSE)
repfun::copydata(datdir)

repfun::rs_setup(D_POP='SAFFL',
         D_POPLBL='Safety',
         D_POPDATA=repfun::adsl %>%
           dplyr::filter(SAFFL =='Y'), D_SUBJID=c("STUDYID","USUBJID"),
         R_ADAMDATA=datdir)

G_POPDATA <- repfun:::rfenv$G_POPDATA %>%
  mutate(TRT01AN=ifelse(TRT01A=='Placebo',1,ifelse(TRT01A=='Xanomeline Low Dose',2,3))) %>%
  select(STUDYID,USUBJID,SAFFL,TRT01AN,TRT01A) %>%
  repfun::ru_labels(varlabels=list('TRT01AN'='Actual Treatment for Period 01 (n)'))

adae <- repfun:::rfenv$adamdata$adae.rda() %>% filter(TRTEMFL=='Y') %>%
        repfun::ru_getdata(G_POPDATA, c("STUDYID", "USUBJID"))
```

## Add Big N to ADAE

``` r
addbign <- repfun::ru_addbignvar(adae,
                         G_POPDATA,
                         groupbyvars=c("TRT01AN", "TRT01A"),
                         countdistinctvars=c("STUDYID", "USUBJID"),
                         totalforvar=c("TRT01AN"),
                         totalid = 99,
                         totaldecode = 'Total',
                         codedecodevarpairs=c("TRT01AN", "TRT01A"),
                         varcodelistpairs=c(""),
                         codelistnames=list(),
                         addbigntovarvalue=TRUE,
                         splitchar="~") %>% select(STUDYID,USUBJID,TRT01AN,TRT01A,
                                                   AEBODSYS,AEDECOD) %>% 
           arrange(STUDYID, USUBJID, TRT01AN, TRT01A, AEBODSYS, AEDECOD)
```

## Display the Results of Adding Big N to ADAE

``` r
lbls <- sapply(addbign,function(x){attr(x,"label")})
knitr::kable(
  head(addbign,10), col.names=paste(names(lbls),lbls,sep=": "), 
  caption = "Big N Added to ADAE (Attached to TRT01A - The last groupbyvars column)") %>% 
  kable_styling(full_width = T) %>% column_spec(c(2,4,5,6), width_min = '2in')
```

| STUDYID: Study Identifier | USUBJID: Unique Subject Identifier | TRT01AN: Actual Treatment for Period 01 (n) | TRT01A: Actual Treatment for Period 01 | AEBODSYS: Body System or Organ Class                 | AEDECOD: Dictionary-Derived Term     |
|:--------------------------|:-----------------------------------|--------------------------------------------:|:---------------------------------------|:-----------------------------------------------------|:-------------------------------------|
| CDISCPILOT01              | 01-701-1015                        |                                           1 | Placebo~(N=86)                         | GASTROINTESTINAL DISORDERS                           | DIARRHOEA                            |
| CDISCPILOT01              | 01-701-1015                        |                                           1 | Placebo~(N=86)                         | GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS | APPLICATION SITE ERYTHEMA            |
| CDISCPILOT01              | 01-701-1015                        |                                           1 | Placebo~(N=86)                         | GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS | APPLICATION SITE PRURITUS            |
| CDISCPILOT01              | 01-701-1023                        |                                           1 | Placebo~(N=86)                         | CARDIAC DISORDERS                                    | ATRIOVENTRICULAR BLOCK SECOND DEGREE |
| CDISCPILOT01              | 01-701-1023                        |                                           1 | Placebo~(N=86)                         | SKIN AND SUBCUTANEOUS TISSUE DISORDERS               | ERYTHEMA                             |
| CDISCPILOT01              | 01-701-1023                        |                                           1 | Placebo~(N=86)                         | SKIN AND SUBCUTANEOUS TISSUE DISORDERS               | ERYTHEMA                             |
| CDISCPILOT01              | 01-701-1023                        |                                           1 | Placebo~(N=86)                         | SKIN AND SUBCUTANEOUS TISSUE DISORDERS               | ERYTHEMA                             |
| CDISCPILOT01              | 01-701-1028                        |                                           3 | Xanomeline High Dose~(N=72)            | GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS | APPLICATION SITE ERYTHEMA            |
| CDISCPILOT01              | 01-701-1028                        |                                           3 | Xanomeline High Dose~(N=72)            | GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS | APPLICATION SITE PRURITUS            |
| CDISCPILOT01              | 01-701-1034                        |                                           3 | Xanomeline High Dose~(N=72)            | GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS | APPLICATION SITE PRURITUS            |

Big N Added to ADAE (Attached to TRT01A - The last groupbyvars column)
