# Calculate Descriptive Statistics

Pass in a data frame along with identification options and have
descriptive statistics derived.

## Usage

``` r
ru_sumstats(
  dsetin,
  analysisvars = NULL,
  analysisvarlabels = "",
  groupbyvars = NULL,
  statslist = c("n", "mean", "median", "min", "max", "sd", "q1", "q3"),
  statsinrowsyn = "N",
  analysisvardps = 0,
  statsdps = list(mean = 1, median = 1, sd = 2, se = 2),
  codedecodevarpairs = c(""),
  varcodelistpairs = c(""),
  codelistnames = list(),
  totalforvar = NULL,
  totalid = NULL,
  totaldecode = c("Total")
)
```

## Arguments

- dsetin:

  The data set that will be ounted to generate descriptive statistics.

- analysisvars:

  The variables to be analysed.

- analysisvarlabels:

  Specify a label statement which will be used to defined labels for
  statistics analysis variables defined in parameter ANALYSISVARS.

- groupbyvars:

  Specifies the variables whose values define the subgroup combinations
  for the analysis. The variables can be divided by statements inside of
  ( and ) to represent different levels of subgroup.

- statslist:

  Specifies a list of summary statistics to be produced.

- statsinrowsyn:

  Place resulting descriptive statistics in rows or columns.

- analysisvardps:

  Base precision of descriptive statistics prior to incorporating
  STATSDPS details.

- statsdps:

  List of additional statistic-specific precision values to add to
  ANALYSISVARDPS.

- codedecodevarpairs:

  Specifies code and decode variable pairs. Those variables should be in
  parameter GROUPBYVARSNUMER. One variable in the pair will contain the
  code, which is used in counting and ordering, and the other will
  contain decode, which is used for presentation.

- varcodelistpairs:

  List of code/decode pairs of variables.

- codelistnames:

  List of decodes for use with decoding code/decode pairs.

- totalforvar:

  Variable for which overall totals are required within all other
  grouped class variables.

- totalid:

  Value(s) used to populate the variable(s) specified in totalforvar.

- totaldecode:

  Value(s) used to populate the variable(s) of the decode variable(s) of
  the totalforvar.

## Value

A data frame based on the incoming data frame but collapsed by groups
with descriptive statistics added.

## Author

Yongwei Wang, <yongwei.x.wang@viivhealthcare.com>  
Chris Rook, <cr883296@gmail.com>

## Examples

``` r
library(repfun)
library(dplyr)
datdir <- file.path(gsub("\\","/",tempdir(),fixed=TRUE),"datdir");
dir.create(datdir,showWarnings=FALSE)
repfun::copydata(datdir)
rs_setup(D_POPDATA=repfun::adsl %>% dplyr::filter(SAFFL =='Y'),
         D_SUBJID=c("STUDYID","USUBJID"),
         R_DICTION=NULL,
         R_OTHERDATA=NULL,
         R_INPUTDATA=NULL,
         R_RAWDATA=NULL,
         R_SDTMDATA=NULL,
         R_ADAMDATA=datdir)
G_POPDATA <- repfun:::rfenv$G_POPDATA %>% dplyr::mutate(
   TRT01AN=ifelse(TRT01A=='Placebo',1,
           ifelse(TRT01A=='Xanomeline Low Dose',2,3))) %>%
           ru_labels(varlabels=list('TRT01AN'='Actual Treatment for Period 01 (n)'))
ru_sumstats(G_POPDATA,
            analysisvars=c("AGE","TRTDURD"),
            groupbyvars=c("STUDYID","TRT01AN"),
            codedecodevarpairs=c("TRT01AN", "TRT01A"),
            totalforvar="TRT01AN", totalid=99,
            totaldecode="Total",
            statsinrowsyn = "Y",
            analysisvardps=list("AGE"=1,"TRTDURD"=2),
            statslist=c("n", "mean", "median", "sd", "min", "max")) %>% head(10)
#>         STUDYID TRT01AN               TRT01A tt_result tt_result_num tt_svid
#> 1  CDISCPILOT01       1              Placebo        86      86.00000       1
#> 2  CDISCPILOT01       2  Xanomeline Low Dose        96      96.00000       1
#> 3  CDISCPILOT01       3 Xanomeline High Dose        72      72.00000       1
#> 4  CDISCPILOT01      99                Total       254     254.00000       1
#> 5  CDISCPILOT01       1              Placebo     75.21      75.20930       2
#> 6  CDISCPILOT01       2  Xanomeline Low Dose     75.96      75.95833       2
#> 7  CDISCPILOT01       3 Xanomeline High Dose     73.78      73.77778       2
#> 8  CDISCPILOT01      99                Total     75.09      75.08661       2
#> 9  CDISCPILOT01       1              Placebo     76.00      76.00000       3
#> 10 CDISCPILOT01       2  Xanomeline Low Dose     78.00      78.00000       3
#>    tt_svnm tt_avid tt_avnm
#> 1        n       1     AGE
#> 2        n       1     AGE
#> 3        n       1     AGE
#> 4        n       1     AGE
#> 5     Mean       1     AGE
#> 6     Mean       1     AGE
#> 7     Mean       1     AGE
#> 8     Mean       1     AGE
#> 9   Median       1     AGE
#> 10  Median       1     AGE
```
