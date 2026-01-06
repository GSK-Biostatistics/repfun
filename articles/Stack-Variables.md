# Stack-Variables

## Vignette Build Datetime

``` r
message(paste0('Datetime: ',Sys.Date(),':',Sys.time()))
#> Datetime: 2026-01-06:2026-01-06 19:25:49.172487
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
repfun:::rfenv$G_POPDATA %>% dplyr::mutate(TRT01AN=ifelse(TRT01A=='Placebo',1,ifelse(TRT01A=='Xanomeline Low Dose',2,3))) %>%  
              repfun::ru_labels(varlabels=list('TRT01AN'='Actual Treatment for Period 01 (n)')) -> G_POPDATA
```

## Read in ADAE and Apply Population

``` r
adae <- repfun:::rfenv$adamdata$adae.rda() %>% dplyr::select(-SAFFL) %>% 
  repfun::ru_getdata(G_POPDATA, c("STUDYID", "USUBJID"), keeppopvars=c("TRT01AN", "TRT01A"))
```

## Generate Counts and Percents for AE Body System and Preferred Term then Denormalize

``` r
aesum_p <- repfun::ru_freq(adae,
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
                                                 acrossvar="TRT01AN", acrossvarlabel="TRT01A", 
                                                 acrossvarprefix=c("tt_ac", "tt_p")) %>% 
                                       dplyr::mutate(ord1=ifelse(tt_summarylevel==0,0,1)) %>%              
                                       dplyr::rename(ord2=tt_summarylevel) %>% 
                                       dplyr::arrange(ord1,AEBODSYS,ord2,AEDECOD) %>%  
                                       dplyr::select(-c(starts_with('tt_p'),starts_with('ord')))
```

## Stack Body System and Preferred Term

``` r
aesum_s <- repfun::ru_stackvar(dsetin=aesum_p,varsin=c('AEBODSYS','AEDECOD'),
                       varout='SYSPREF',sepc='/',splitc='~',
                       varlabel='Body System/Preferred Term')
```

## Display the Data Set Containing the Stacked Column

``` r
lbls <- sapply(aesum_s,function(x){attr(x,"label")})
knitr::kable(head(aesum_s,10), col.names=paste(names(lbls),lbls,sep=": "), 
             caption = "Denormalized Data Set for Counts and Percents with Stacked Column") %>%  
  kable_styling(full_width = T) %>% column_spec(c(1,2,3), width_min = c('2in','2in','2in'))
```

| SYSPREF: Body System/Preferred Term                     | AEBODSYS: Body System or Organ Class | AEDECOD: Dictionary-Derived Term     | tt_ac01: Placebo | tt_ac02: Xanomeline Low Dose | tt_ac03: Xanomeline High Dose | tt_ac99: Total |
|:--------------------------------------------------------|:-------------------------------------|:-------------------------------------|:-----------------|:-----------------------------|:------------------------------|:---------------|
| ANY EVENT/~ANY EVENT                                    | ANY EVENT                            | ANY EVENT                            | 69 (80%)         | 86 (90%)                     | 70 (97%)                      | 225 (89%)      |
| CARDIAC DISORDERS/~ANY EVENT                            | CARDIAC DISORDERS                    | ANY EVENT                            | 13 (15%)         | 16 (17%)                     | 15 (21%)                      | 44 (17%)       |
| CARDIAC DISORDERS/~ATRIAL FIBRILLATION                  | CARDIAC DISORDERS                    | ATRIAL FIBRILLATION                  | 1 (1%)           | 2 (2%)                       | 2 (3%)                        | 5 (2%)         |
| CARDIAC DISORDERS/~ATRIAL FLUTTER                       | CARDIAC DISORDERS                    | ATRIAL FLUTTER                       | 0 (0%)           | 1 (1%)                       | 1 (1%)                        | 2 (1%)         |
| CARDIAC DISORDERS/~ATRIAL HYPERTROPHY                   | CARDIAC DISORDERS                    | ATRIAL HYPERTROPHY                   | 1 (1%)           | 0 (0%)                       | 0 (0%)                        | 1 (0%)         |
| CARDIAC DISORDERS/~ATRIOVENTRICULAR BLOCK FIRST DEGREE  | CARDIAC DISORDERS                    | ATRIOVENTRICULAR BLOCK FIRST DEGREE  | 1 (1%)           | 1 (1%)                       | 0 (0%)                        | 2 (1%)         |
| CARDIAC DISORDERS/~ATRIOVENTRICULAR BLOCK SECOND DEGREE | CARDIAC DISORDERS                    | ATRIOVENTRICULAR BLOCK SECOND DEGREE | 2 (2%)           | 2 (2%)                       | 1 (1%)                        | 5 (2%)         |
| CARDIAC DISORDERS/~BRADYCARDIA                          | CARDIAC DISORDERS                    | BRADYCARDIA                          | 1 (1%)           | 0 (0%)                       | 0 (0%)                        | 1 (0%)         |
| CARDIAC DISORDERS/~BUNDLE BRANCH BLOCK LEFT             | CARDIAC DISORDERS                    | BUNDLE BRANCH BLOCK LEFT             | 1 (1%)           | 0 (0%)                       | 0 (0%)                        | 1 (0%)         |
| CARDIAC DISORDERS/~BUNDLE BRANCH BLOCK RIGHT            | CARDIAC DISORDERS                    | BUNDLE BRANCH BLOCK RIGHT            | 1 (1%)           | 1 (1%)                       | 0 (0%)                        | 2 (1%)         |

Denormalized Data Set for Counts and Percents with Stacked Column
