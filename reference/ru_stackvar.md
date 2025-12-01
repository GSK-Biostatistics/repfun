# Stack Columns of a Dataframe into New Column

Pass in a dataframe and columns to stack and have new dataframe returned
that contains the stacked columns.

## Usage

``` r
ru_stackvar(
  dsetin,
  sepc = "/",
  splitc = "\\line",
  varsin = NULL,
  varout = NULL,
  varlabel = NULL
)
```

## Arguments

- dsetin:

  Name of incoming dataframe with columns to have stacked.

- sepc:

  Separator character between the stacked columns.

- splitc:

  Split character between stacked columns.

- varsin:

  List of variables to be stacked.

- varout:

  Name of stacked column in dataframe.

- varlabel:

  Label for new stacked column.

## Value

The incoming dataframe with columns stacked as requested.

## Author

Chris Rook, <cr883296@gmail.com>  
Yongwei Wang, <yongwei.x.wang@viivhealthcare.com>

## Examples

``` r
library(repfun)
library(dplyr)
datdir <- file.path(gsub("\\","/",tempdir(),fixed=TRUE),"datdir")
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
  repfun::ru_labels(varlabels=list('TRT01AN'='Actual Treatment for Period 01 (n)'))
adae <- repfun:::rfenv$adamdata$adae.rda() %>%
        dplyr::inner_join(G_POPDATA, by=c('STUDYID','USUBJID','SAFFL','TRT01A'))
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
                   resultpctdps=0) %>%
repfun::ru_denorm(varstodenorm=c("tt_result", "PERCENT"),
          groupbyvars=c("tt_summarylevel", "AEBODSYS", "AEDECOD"),
          acrossvar="TRT01AN", acrossvarlabel="TRT01A",
          acrossvarprefix=c("tt_ac", "tt_p")) %>%
dplyr::mutate(ord1=ifelse(tt_summarylevel==0,0,1)) %>%
dplyr::rename(ord2=tt_summarylevel) %>%
dplyr::arrange(ord1,AEBODSYS,ord2,AEDECOD) %>%
dplyr::select(-c(starts_with('tt_p'),starts_with('ord'))) %>%
repfun::ru_stackvar(varsin=c('AEBODSYS','AEDECOD'),varout='SYSPREF',
                    varlabel='Body System/Preferred Term')
```
