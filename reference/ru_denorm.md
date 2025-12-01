# Transpose a Data Frame

Pass in a data frame along with identification options and have it
transposed (denormalized, long to wide) to display treatment columns.

## Usage

``` r
ru_denorm(
  dsetin,
  varstodenorm = NULL,
  groupbyvars = NULL,
  acrossvar = NULL,
  acrossvarlabel = NULL,
  acrossvarprefix = "tt_",
  acrossvarsuffix = NULL
)
```

## Arguments

- dsetin:

  The data set to transpose.

- varstodenorm:

  The variables to transpose.

- groupbyvars:

  Definition of one row in the output data frame.

- acrossvar:

  Variable to define the columns in the transposed data frame.

- acrossvarlabel:

  Variable to define the labels in the transposed data frame.

- acrossvarprefix:

  Add to the beginning of each value in the across variable in the
  output data frame.

- acrossvarsuffix:

  Add to the end of each value in the across variable in the output data
  frame.

## Value

A data frame based on the incoming data frame transposed from long to
wide.

## Details

dsetin, varstodenorm=NULL, groupbyvars=NULL, acrossvar=NULL,
acrossvarlabel=NULL, acrossvarprefix="tt\_", acrossvarsuffix=NULL

## Author

Yongwei Wang, <yongwei.x.wang@viivhealthcare.com>  
Chris Rook, <cr883296@gmail.com>

## Examples

``` r
#====================
# AEs: N and Percent
#====================
library(repfun)
library(dplyr)
datdir <- file.path(gsub("\\","/",tempdir(),fixed=TRUE),"datdir")
dir.create(datdir,showWarnings=FALSE)
repfun::copydata(datdir)
repfun::rs_setup(D_POP="SAFFL",
                 D_POPLBL="Safety",
                 D_POPDATA=repfun::adsl %>% dplyr::filter(SAFFL =='Y'),
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
  repfun::ru_labels(varlabels=list('TRT01AN'='Actual Treatment for Period 01 (n)'))
adae <- repfun:::rfenv$adamdata$adae.rda() %>% select(-SAFFL) %>%
        repfun::ru_getdata(G_POPDATA, c("STUDYID", "USUBJID"),
                   keeppopvars=c("TRT01AN", "TRT01A"))
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
              varcodelistpairs=c(""),
              codelistnames=list(),
              resultpctdps=0) %>%
  repfun::ru_denorm(varstodenorm=c("tt_result", "PERCENT"),
            groupbyvars=c("tt_summarylevel", "AEBODSYS", "AEDECOD"),
            acrossvar="TRT01AN",
            acrossvarlabel="TRT01A",
            acrossvarprefix=c("tt_ac", "tt_p"))

#======================================
# Demography Statistics: N and Percent
#======================================
repfun::rs_setup(D_POP="SAFFL",
                 D_POPLBL="Safety",
                 D_POPDATA=repfun::adsl %>% dplyr::filter(SAFFL =='Y'),
                 D_SUBJID=c("STUDYID","USUBJID"),
                 R_DICTION=NULL,
                 R_OTHERDATA=NULL,
                 R_INPUTDATA=NULL,
                 R_RAWDATA=NULL,
                 R_SDTMDATA=NULL,
                 R_ADAMDATA=datdir)
G_POPDATA <- repfun:::rfenv$G_POPDATA %>%
              dplyr::mutate(TRT01AN=
                     ifelse(TRT01A=='Placebo',1,
                     ifelse(TRT01A=='Xanomeline Low Dose',2,3))) %>%
  repfun::ru_labels(varlabels=list('TRT01AN'='Actual Treatment for Period 01 (n)'))
demstats_t <- repfun::ru_sumstats(G_POPDATA,
                 analysisvars=c("AGE","TRTDURD"),
                 groupbyvars=c("STUDYID","TRT01AN"),
                 codedecodevarpairs=c("TRT01AN", "TRT01A"),
                 totalforvar="TRT01AN", totalid=99,
                 totaldecode="Total",
                 statsinrowsyn = "Y",
                 analysisvardps=list("AGE"=1,"TRTDURD"=2),
                 statslist=c("n", "mean", "median", "sd", "min", "max")) %>%
  repfun::ru_denorm(varstodenorm=c("tt_result"),
            groupbyvars=c("tt_avid", "tt_avnm", "tt_svid", "tt_svnm"),
            acrossvar="TRT01AN", acrossvarlabel="TRT01A",
            acrossvarprefix=c("tt_ac"))
```
