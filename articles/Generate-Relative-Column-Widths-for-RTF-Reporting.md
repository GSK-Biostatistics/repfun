# Generate-Relative-Column-Widths-for-RTF-Reporting

## Vignette Build Datetime

``` r
message(paste0('Datetime: ',Sys.Date(),':',Sys.time()))
#> Datetime: 2025-12-02:2025-12-02 19:33:09.511227
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

## Generate Counts and Percents for AE Body System and Preferred Term then Denormalize It and Add Page Numbers

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
                                       dplyr::arrange(tt_summarylevel, AEBODSYS, AEDECOD) %>%
                                       dplyr::mutate(ord1=ifelse(tt_summarylevel==0,0,1)) %>% 
                                       dplyr::rename(ord2=tt_summarylevel) %>% 
                                       dplyr::arrange(ord1,AEBODSYS,ord2,AEDECOD) %>%    
                                       dplyr::select(-c(starts_with('tt_p'),starts_with('ord'))) %>%
                                       repfun::ru_addpage(grpvars=c('AEBODSYS'),rowsprbdy=30) 
```

## Example 1: Generate a List of Relative Column Widths for RTF Reporting with No Defaults

``` r
widths1 <- repfun::ru_width_rtf(aesum_p,c('AEBODSYS','AEDECOD','tt_ac01','tt_ac02','tt_ac03','tt_ac99'))
print(widths1)
#> AEBODSYS  AEDECOD  tt_ac01  tt_ac02  tt_ac03  tt_ac99 
#>       46       32        5        5        5        6
```

## Example 2: Generate a List of Relative Column Widths for RTF Reporting with Defaults

``` r
widths2 <- repfun::ru_width_rtf(aesum_p,c('AEBODSYS','AEDECOD','tt_ac01','tt_ac02','tt_ac03','tt_ac99'),
                        list('AEBODSYS'=35, 'AEDECOD'=30))
print(widths2)
#> AEBODSYS  AEDECOD  tt_ac01  tt_ac02  tt_ac03  tt_ac99 
#>       35       30        8        8        8       10
```
