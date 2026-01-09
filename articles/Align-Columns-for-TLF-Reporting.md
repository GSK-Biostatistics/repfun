# Align-Columns-for-TLF-Reporting

## Vignette Build Datetime

``` r
message(paste0('Datetime: ',Sys.Date(),':',Sys.time()))
#> Datetime: 2026-01-09:2026-01-09 15:19:30.635476
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
repfun::rs_setup(D_POP="SAFFL",D_POPLBL="Safety",D_POPDATA=repfun::adsl %>% 
           dplyr::filter(SAFFL =='Y'), D_SUBJID=c("STUDYID","USUBJID"), R_ADAMDATA=datdir)
repfun:::rfenv$G_POPDATA %>% dplyr::mutate(TRT01AN=ifelse(TRT01A=='Placebo',1,ifelse(TRT01A=='Xanomeline Low Dose',2,3))) %>% 
              repfun::ru_labels(varlabels=list('TRT01AN'='Actual Treatment for Period 01 (n)')) -> G_POPDATA
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
aesum_t <- aesum %>% repfun::ru_denorm(varstodenorm=c("tt_result", "PERCENT"), 
                               groupbyvars=c("tt_summarylevel", "AEBODSYS", "AEDECOD"), 
                               acrossvar="TRT01AN",
                               acrossvarlabel="TRT01A", acrossvarprefix=c("tt_ac", "tt_p"))
```

## Align the Columns for TLF Reporting

``` r
aesum_t_a <- repfun::ru_align(aesum_t, "tt_ac:") %>% 
  select(-c('tt_p01','tt_p02','tt_p03','tt_p99','tt_summarylevel'))
```

## Display the Aligned AE Counts and Percents Data Set

``` r
print("Aligned Reporting Data Set for AE Counts and Percents")
#> [1] "Aligned Reporting Data Set for AE Counts and Percents"
print(head(aesum_t_a,10))
#>                                                AEBODSYS   AEDECOD  tt_ac01
#> 1                                             ANY EVENT ANY EVENT 69 (80%)
#> 2                                     CARDIAC DISORDERS ANY EVENT 13 (15%)
#> 3            CONGENITAL, FAMILIAL AND GENETIC DISORDERS ANY EVENT  0  (0%)
#> 4                           EAR AND LABYRINTH DISORDERS ANY EVENT  1  (1%)
#> 5                                         EYE DISORDERS ANY EVENT  4  (5%)
#> 6                            GASTROINTESTINAL DISORDERS ANY EVENT 17 (20%)
#> 7  GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS ANY EVENT 21 (24%)
#> 8                               HEPATOBILIARY DISORDERS ANY EVENT  1  (1%)
#> 9                               IMMUNE SYSTEM DISORDERS ANY EVENT  0  (0%)
#> 10                          INFECTIONS AND INFESTATIONS ANY EVENT 16 (19%)
#>     tt_ac02  tt_ac03   tt_ac99
#> 1  86 (90%) 70 (97%) 225 (89%)
#> 2  16 (17%) 15 (21%)  44 (17%)
#> 3   1  (1%)  2  (3%)   3  (1%)
#> 4   2  (2%)  1  (1%)   4  (2%)
#> 5   2  (2%)  1  (1%)   7  (3%)
#> 6  16 (17%) 20 (28%)  53 (21%)
#> 7  51 (53%) 36 (50%) 108 (43%)
#> 8   0  (0%)  0  (0%)   1  (0%)
#> 9   1  (1%)  1  (1%)   2  (1%)
#> 10 10 (10%) 13 (18%)  39 (15%)
```

## Generate Counts and Percents for Baseline Characteristics Data

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

## Denormalize the Baseline Characteristics Summary Statistics Data

``` r
demstats_t <- demstats %>% repfun::ru_denorm(varstodenorm=c("tt_result"), 
                                     groupbyvars=c("tt_avid", "tt_avnm", "tt_svid", "tt_svnm"),                                                                              acrossvar="TRT01AN", acrossvarlabel="TRT01A", 
                                     acrossvarprefix=c("tt_ac"))
```

## Align the Columns for TLF Reporting

``` r
demstats_t_a <- repfun::ru_align(demstats_t, "tt_ac:")
```

## Display the Aligned Baseline Characteristics Summary Statistics Data Set

``` r
print("Aligned Reporting Data Set for Baseline Characteristics Summary Stats")
#> [1] "Aligned Reporting Data Set for Baseline Characteristics Summary Stats"
print(head(demstats_t_a,10))
#>    tt_avid tt_avnm tt_svid tt_svnm  tt_ac01  tt_ac02  tt_ac03  tt_ac99
#> 1        1     AGE       1       n  86       96       72      254     
#> 2        1     AGE       2    Mean  75.21    75.96    73.78    75.09  
#> 3        1     AGE       3  Median  76.00    78.00    75.50    77.00  
#> 4        1     AGE       4      SD   8.590    8.114    7.944    8.246 
#> 5        1     AGE       5     Min  52.0     51.0     56.0     51.0   
#> 6        1     AGE       6     Max  89.0     88.0     88.0     89.0   
#> 7        2 TRTDURD       1       n  85       95       72      252     
#> 8        2 TRTDURD       2    Mean 149.541   86.811  112.222  115.230 
#> 9        2 TRTDURD       3  Median 182.000   63.000   96.500  132.000 
#> 10       2 TRTDURD       4      SD  60.3544  70.4737  65.5233  70.7137
```
