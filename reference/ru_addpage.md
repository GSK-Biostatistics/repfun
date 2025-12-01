# Add a Page Number Column to an Existing Dataframe

Take incoming dataframe and add page number variable to it accounting
for grouping variables, stacked variables, and no-split variables.

## Usage

``` r
ru_addpage(
  dsetin,
  grpvars = NULL,
  stackvars = NULL,
  varlabls = NULL,
  rowsprbdy = NULL,
  startpaging = 0,
  lastbygrp = FALSE,
  fpage = "all",
  nftnotes = 0,
  nosplitvars = FALSE,
  npgvars = 0
)
```

## Arguments

- dsetin:

  The dataframe for which a paging variable will be added.

- grpvars:

  Grouping variables used in the output (used for nosplitvars).

- stackvars:

  Specify stacked grouping variables (reduces \# of page lines available
  for data).

- varlabls:

  Apply labels to outgoing dataframe.

- rowsprbdy:

  Number of rows in the body of the report.

- startpaging:

  Set to zero on first call, and \> 0 on recalls to fix widows.

- lastbygrp:

  Set to true if this page is processing the last value of the grouping
  variables (used when footnote is applied only to last page).

- fpage:

  Setting to 'last' indicates that footnotes are only displayed on the
  last page.

- nftnotes:

  Enter the number of footnotes (determines \# of page lines available
  for data).

- nosplitvars:

  Setting to true requires all values of the last grouping/stackvar must
  be on the same page (if possible).

- npgvars:

  Number of page-by variables for this report (reduces \# of page lines
  available for data).

## Value

A dataframe based on the incoming dataframe but with a paging variable
added.

## Author

Chris Rook, <cr883296@gmail.com>  
Yongwei Wang, <yongwei.x.wang@viivhealthcare.com>

## Examples

``` r
library(repfun)
library(dplyr)
datdir <- file.path(gsub("\\","/",tempdir(),fixed=TRUE),"datdir");
dir.create(datdir,showWarnings=FALSE)
repfun::copydata(datdir)
repfun::rs_setup(D_POPDATA=repfun::adsl %>% dplyr::filter(SAFFL =='Y'),
                 D_SUBJID=c("STUDYID","USUBJID"),
                 R_DICTION=NULL,
                 R_OTHERDATA=NULL,
                 R_INPUTDATA=NULL,
                 R_RAWDATA=NULL,
                 R_SDTMDATA=NULL,
                 R_ADAMDATA=datdir)
G_POPDATA <- repfun:::rfenv$G_POPDATA %>%
 dplyr::mutate(TRT01AN=ifelse(TRT01A=='Placebo',1,
                ifelse(TRT01A=='Xanomeline Low Dose',2,3))) %>%
 repfun::ru_labels(varlabels=list('TRT01AN'='Actual Treatment for Period 001 (n)'))
adae <- repfun:::rfenv$adamdata$adae.rda() %>%
        dplyr::inner_join(G_POPDATA, by=c('STUDYID','USUBJID','SAFFL','TRT01A'))
aesum_t <- repfun::ru_freq(
             adae,
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
             resultpctdps=0) %>%
 repfun::ru_denorm(varstodenorm=c("tt_result", "PERCENT"),
           groupbyvars=c("tt_summarylevel", "AEBODSYS", "AEDECOD"),
           acrossvar="TRT01AN",
           acrossvarlabel="TRT01A",
           acrossvarprefix=c("tt_ac", "tt_p")) %>%
 dplyr::mutate(ord1=ifelse(tt_summarylevel==0,0,1)) %>%
 dplyr::rename(ord2=tt_summarylevel) %>%
 dplyr::arrange(ord1,AEBODSYS,ord2,AEDECOD) %>%
 dplyr::select(-c(starts_with('tt_p'),starts_with('ord')))

#Example 1: Simple paging.
#aesum_p1 <- repfun::ru_addpage(aesum_t,grpvars=c('AEBODSYS'),rowsprbdy=30)
#print(head(aesum_p1,10))

#Example 2: No splitvars, but 30 rows won't work for this data.
#aesum_p2 <- repfun::ru_addpage(aesum_t,grpvars=c('AEBODSYS'),rowsprbdy=30,
#                        nosplitvars=TRUE)
#print(head(aesum_p2,10))

# Example 3: No splitvars, but 35 rows is enough.
aesum_p3 <- repfun::ru_addpage(aesum_t,grpvars=c('AEBODSYS'),rowsprbdy=35,
                    nosplitvars=TRUE)
print(head(aesum_p3,10))
#>             AEBODSYS                              AEDECOD  tt_ac01  tt_ac02
#> 1          ANY EVENT                            ANY EVENT 69 (80%) 86 (90%)
#> 2  CARDIAC DISORDERS                            ANY EVENT 13 (15%) 16 (17%)
#> 3  CARDIAC DISORDERS                  ATRIAL FIBRILLATION   1 (1%)   2 (2%)
#> 4  CARDIAC DISORDERS                       ATRIAL FLUTTER   0 (0%)   1 (1%)
#> 5  CARDIAC DISORDERS                   ATRIAL HYPERTROPHY   1 (1%)   0 (0%)
#> 6  CARDIAC DISORDERS  ATRIOVENTRICULAR BLOCK FIRST DEGREE   1 (1%)   1 (1%)
#> 7  CARDIAC DISORDERS ATRIOVENTRICULAR BLOCK SECOND DEGREE   2 (2%)   2 (2%)
#> 8  CARDIAC DISORDERS                          BRADYCARDIA   1 (1%)   0 (0%)
#> 9  CARDIAC DISORDERS             BUNDLE BRANCH BLOCK LEFT   1 (1%)   0 (0%)
#> 10 CARDIAC DISORDERS            BUNDLE BRANCH BLOCK RIGHT   1 (1%)   1 (1%)
#>     tt_ac03   tt_ac99 PAGEVAR
#> 1  70 (97%) 225 (89%)       1
#> 2  15 (21%)  44 (17%)       1
#> 3    2 (3%)    5 (2%)       1
#> 4    1 (1%)    2 (1%)       1
#> 5    0 (0%)    1 (0%)       1
#> 6    0 (0%)    2 (1%)       1
#> 7    1 (1%)    5 (2%)       1
#> 8    0 (0%)    1 (0%)       1
#> 9    0 (0%)    1 (0%)       1
#> 10   0 (0%)    2 (1%)       1
```
