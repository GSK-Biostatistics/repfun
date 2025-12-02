# Add-Page-Numbers-to-Dataframe

## Vignette Build Datetime

``` r
message(paste0('Datetime: ',Sys.Date(),':',Sys.time()))
#> Datetime: 2025-12-02:2025-12-02 21:09:39.330548
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
repfun:::rfenv$G_POPDATA %>% mutate(TRT01AN=ifelse(TRT01A=='Placebo',1,ifelse(TRT01A=='Xanomeline Low Dose',2,3))) %>% 
  repfun::ru_labels(varlabels=list('TRT01AN'='Actual Treatment for Period 01 (n)')) -> G_POPDATA
```

## Read in ADAE and Apply Population

``` r
adae <- repfun:::rfenv$adamdata$adae.rda() %>% select(-SAFFL) %>% 
  repfun::ru_getdata(G_POPDATA, c("STUDYID", "USUBJID"), keeppopvars=c("TRT01AN", "TRT01A"))
```

## Generate Counts and Percents for AE Body System and Preferred Term then Denormalize It

``` r
aesum_t <- repfun::ru_freq(adae,
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
                   resultpctdps=0) %>% repfun::ru_denorm(varstodenorm=c("tt_result", "PERCENT"), 
                                                 groupbyvars=c("tt_summarylevel", "AEBODSYS", "AEDECOD"), 
                                                 acrossvar="TRT01AN",
                                                 acrossvarlabel="TRT01A", acrossvarprefix=c("tt_ac", "tt_p")) %>% 
                                       arrange(tt_summarylevel, AEBODSYS, AEDECOD) %>%
                                       mutate(ord1=ifelse(tt_summarylevel==0,0,1)) %>% 
                                       rename(ord2=tt_summarylevel) %>% arrange(ord1,AEBODSYS,ord2,AEDECOD) %>%    
                                       select(-c(starts_with('tt_p'),starts_with('ord')))
#print(head(aesum_t,10))
#str(aesum_t)
```

## Add Simple Page Numbers then Display the Dataframe using 30 Rows in Body of Report

``` r
#===========================
# Example 1: Simple paging.
#===========================
aesum_p1 <- repfun::ru_addpage(aesum_t,grpvars=c('AEBODSYS'),rowsprbdy=30) %>%
  mutate(Row_Number=row_number()) %>%
  repfun::ru_labels(varlabels=list('Row_Number'='DataFrame Row Number'))
lbls <- lapply(aesum_p1,function(x){attr(x,"label")})
knitr::kable(head(aesum_p1,10), col.names=paste(names(lbls),lbls,sep=": "),
             caption="Example 1: Simple Page Numbers Added to Dataframe using 30 Rows in Body of Report") %>%
  kable_styling(full_width = T) %>% column_spec(c(1,2), width_min = '3in')
```

| AEBODSYS: Body System or Organ Class | AEDECOD: Dictionary-Derived Term     | tt_ac01: Placebo | tt_ac02: Xanomeline Low Dose | tt_ac03: Xanomeline High Dose | tt_ac99: Total | PAGEVAR: Page Variable | Row_Number: DataFrame Row Number |
|:-------------------------------------|:-------------------------------------|:-----------------|:-----------------------------|:------------------------------|:---------------|-----------------------:|---------------------------------:|
| ANY EVENT                            | ANY EVENT                            | 69 (80%)         | 86 (90%)                     | 70 (97%)                      | 225 (89%)      |                      1 |                                1 |
| CARDIAC DISORDERS                    | ANY EVENT                            | 13 (15%)         | 16 (17%)                     | 15 (21%)                      | 44 (17%)       |                      1 |                                2 |
| CARDIAC DISORDERS                    | ATRIAL FIBRILLATION                  | 1 (1%)           | 2 (2%)                       | 2 (3%)                        | 5 (2%)         |                      1 |                                3 |
| CARDIAC DISORDERS                    | ATRIAL FLUTTER                       | 0 (0%)           | 1 (1%)                       | 1 (1%)                        | 2 (1%)         |                      1 |                                4 |
| CARDIAC DISORDERS                    | ATRIAL HYPERTROPHY                   | 1 (1%)           | 0 (0%)                       | 0 (0%)                        | 1 (0%)         |                      1 |                                5 |
| CARDIAC DISORDERS                    | ATRIOVENTRICULAR BLOCK FIRST DEGREE  | 1 (1%)           | 1 (1%)                       | 0 (0%)                        | 2 (1%)         |                      1 |                                6 |
| CARDIAC DISORDERS                    | ATRIOVENTRICULAR BLOCK SECOND DEGREE | 2 (2%)           | 2 (2%)                       | 1 (1%)                        | 5 (2%)         |                      1 |                                7 |
| CARDIAC DISORDERS                    | BRADYCARDIA                          | 1 (1%)           | 0 (0%)                       | 0 (0%)                        | 1 (0%)         |                      1 |                                8 |
| CARDIAC DISORDERS                    | BUNDLE BRANCH BLOCK LEFT             | 1 (1%)           | 0 (0%)                       | 0 (0%)                        | 1 (0%)         |                      1 |                                9 |
| CARDIAC DISORDERS                    | BUNDLE BRANCH BLOCK RIGHT            | 1 (1%)           | 1 (1%)                       | 0 (0%)                        | 2 (1%)         |                      1 |                               10 |

Example 1: Simple Page Numbers Added to Dataframe using 30 Rows in Body
of Report

## Add Paging using NoSplitVars when it Won’t Work with 30 Rows in Body of Report

``` r
#=======================================================================================================================
# Example 2: No splitvars, but 30 rows won't work for this data. (Which will revert to simple paging as in Example 1.)
#=======================================================================================================================
aesum_p2 <- repfun::ru_addpage(aesum_t,grpvars=c('AEBODSYS'),rowsprbdy=30,nosplitvars=TRUE) %>%
  mutate(Row_Number=row_number()) %>% repfun::ru_labels(varlabels=list('Row_Number'='DataFrame Row Number'))
lbls <- lapply(aesum_p2,function(x){attr(x,"label")})
knitr::kable(
  head(aesum_p2,10), col.names=paste(names(lbls),lbls,sep=": "),
  caption="Example 2: Add Paging to Dataframe with NoSplitVars when it Won't Work (Revert to Simple Paging)") %>%
  kable_styling(full_width = T) %>% column_spec(c(1,2), width_min ='3in')
```

| AEBODSYS: Body System or Organ Class | AEDECOD: Dictionary-Derived Term     | tt_ac01: Placebo | tt_ac02: Xanomeline Low Dose | tt_ac03: Xanomeline High Dose | tt_ac99: Total | PAGEVAR: Page Variable | Row_Number: DataFrame Row Number |
|:-------------------------------------|:-------------------------------------|:-----------------|:-----------------------------|:------------------------------|:---------------|-----------------------:|---------------------------------:|
| ANY EVENT                            | ANY EVENT                            | 69 (80%)         | 86 (90%)                     | 70 (97%)                      | 225 (89%)      |                      1 |                                1 |
| CARDIAC DISORDERS                    | ANY EVENT                            | 13 (15%)         | 16 (17%)                     | 15 (21%)                      | 44 (17%)       |                      1 |                                2 |
| CARDIAC DISORDERS                    | ATRIAL FIBRILLATION                  | 1 (1%)           | 2 (2%)                       | 2 (3%)                        | 5 (2%)         |                      1 |                                3 |
| CARDIAC DISORDERS                    | ATRIAL FLUTTER                       | 0 (0%)           | 1 (1%)                       | 1 (1%)                        | 2 (1%)         |                      1 |                                4 |
| CARDIAC DISORDERS                    | ATRIAL HYPERTROPHY                   | 1 (1%)           | 0 (0%)                       | 0 (0%)                        | 1 (0%)         |                      1 |                                5 |
| CARDIAC DISORDERS                    | ATRIOVENTRICULAR BLOCK FIRST DEGREE  | 1 (1%)           | 1 (1%)                       | 0 (0%)                        | 2 (1%)         |                      1 |                                6 |
| CARDIAC DISORDERS                    | ATRIOVENTRICULAR BLOCK SECOND DEGREE | 2 (2%)           | 2 (2%)                       | 1 (1%)                        | 5 (2%)         |                      1 |                                7 |
| CARDIAC DISORDERS                    | BRADYCARDIA                          | 1 (1%)           | 0 (0%)                       | 0 (0%)                        | 1 (0%)         |                      1 |                                8 |
| CARDIAC DISORDERS                    | BUNDLE BRANCH BLOCK LEFT             | 1 (1%)           | 0 (0%)                       | 0 (0%)                        | 1 (0%)         |                      1 |                                9 |
| CARDIAC DISORDERS                    | BUNDLE BRANCH BLOCK RIGHT            | 1 (1%)           | 1 (1%)                       | 0 (0%)                        | 2 (1%)         |                      1 |                               10 |

Example 2: Add Paging to Dataframe with NoSplitVars when it Won’t Work
(Revert to Simple Paging)

## Add Paging using NoSplitVars when it Will Work with 35 Rows in Body of Report

``` r
#=============================================================================
# Example 3: NoSplitVars on AEBODSYS using 35 rows which works for this data.
#=============================================================================
aesum_p3 <- repfun::ru_addpage(aesum_t,grpvars=c('AEBODSYS'),rowsprbdy=35,nosplitvars=TRUE) %>%
  mutate(Row_Number=row_number()) %>% repfun::ru_labels(varlabels=list('Row_Number'='DataFrame Row Number'))
lbls <- lapply(aesum_p3,function(x){attr(x,"label")})
knitr::kable(
  head(aesum_p3,10), col.names=paste(names(lbls),lbls,sep=": "),
  caption = "Example 3: Add Paging to Dataframe with NoSplitVars when it Will Work using 35 Rows in Body of Report") %>%
  kable_styling(full_width = T) %>% column_spec(c(1,2), width_min ='3in')
```

| AEBODSYS: Body System or Organ Class | AEDECOD: Dictionary-Derived Term     | tt_ac01: Placebo | tt_ac02: Xanomeline Low Dose | tt_ac03: Xanomeline High Dose | tt_ac99: Total | PAGEVAR: Page Variable | Row_Number: DataFrame Row Number |
|:-------------------------------------|:-------------------------------------|:-----------------|:-----------------------------|:------------------------------|:---------------|-----------------------:|---------------------------------:|
| ANY EVENT                            | ANY EVENT                            | 69 (80%)         | 86 (90%)                     | 70 (97%)                      | 225 (89%)      |                      1 |                                1 |
| CARDIAC DISORDERS                    | ANY EVENT                            | 13 (15%)         | 16 (17%)                     | 15 (21%)                      | 44 (17%)       |                      1 |                                2 |
| CARDIAC DISORDERS                    | ATRIAL FIBRILLATION                  | 1 (1%)           | 2 (2%)                       | 2 (3%)                        | 5 (2%)         |                      1 |                                3 |
| CARDIAC DISORDERS                    | ATRIAL FLUTTER                       | 0 (0%)           | 1 (1%)                       | 1 (1%)                        | 2 (1%)         |                      1 |                                4 |
| CARDIAC DISORDERS                    | ATRIAL HYPERTROPHY                   | 1 (1%)           | 0 (0%)                       | 0 (0%)                        | 1 (0%)         |                      1 |                                5 |
| CARDIAC DISORDERS                    | ATRIOVENTRICULAR BLOCK FIRST DEGREE  | 1 (1%)           | 1 (1%)                       | 0 (0%)                        | 2 (1%)         |                      1 |                                6 |
| CARDIAC DISORDERS                    | ATRIOVENTRICULAR BLOCK SECOND DEGREE | 2 (2%)           | 2 (2%)                       | 1 (1%)                        | 5 (2%)         |                      1 |                                7 |
| CARDIAC DISORDERS                    | BRADYCARDIA                          | 1 (1%)           | 0 (0%)                       | 0 (0%)                        | 1 (0%)         |                      1 |                                8 |
| CARDIAC DISORDERS                    | BUNDLE BRANCH BLOCK LEFT             | 1 (1%)           | 0 (0%)                       | 0 (0%)                        | 1 (0%)         |                      1 |                                9 |
| CARDIAC DISORDERS                    | BUNDLE BRANCH BLOCK RIGHT            | 1 (1%)           | 1 (1%)                       | 0 (0%)                        | 2 (1%)         |                      1 |                               10 |

Example 3: Add Paging to Dataframe with NoSplitVars when it Will Work
using 35 Rows in Body of Report
