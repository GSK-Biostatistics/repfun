# Assign Big N to Data Frame.

Pass in a data frame along with identification options and have Big N
added to it.

## Usage

``` r
ru_addbignvar(
  dsetintoaddbign,
  dsetintocount,
  countdistinctvars = c("STUDYID", "USUBJID"),
  groupbyvars = NULL,
  totalforvar = NULL,
  totalid = NULL,
  totaldecode = c("Total"),
  codedecodevarpairs = NULL,
  varcodelistpairs = NULL,
  codelistnames = list(),
  addbigntovarvalue = TRUE,
  splitchar = " "
)
```

## Arguments

- dsetintoaddbign:

  The data set that will hold the derived big N value.

- dsetintocount:

  The data set that will be counted to generate big N.

- countdistinctvars:

  Variable(s) that contain values to be counted uniquely within any
  output grouping.

- groupbyvars:

  Variables in DSETINTOCOUNT to group the data by when counting to
  deriving the big N.

- totalforvar:

  Variable for which overall totals are required within all other
  grouped class variables.

- totalid:

  Value(s) used to populate the variable(s) specified in totalforvar.

- totaldecode:

  Value(s) used to populate the variable(s) of the decode variable(s) of
  the totalforvar.

- codedecodevarpairs:

  Specifies code and decode variable pairs. Those variables should be in
  parameter GROUPBYVARSNUMER. One variable in the pair will contain the
  code, which is used in counting and ordering, and the other will
  contain decode, which is used for presentation.

- varcodelistpairs:

  List of code/decode pairs of variables.

- codelistnames:

  List of decodes for use with decoding code/decode pairs.

- addbigntovarvalue:

  Place big N in a new variable or append to an existing variable (last
  groupbyvars value)?

- splitchar:

  Text to insert between existing string and big N.

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
library(tibble)
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
G_POPDATA <- repfun:::rfenv$G_POPDATA %>% dplyr::mutate(TRT01AN=
                                         ifelse(TRT01A=='Placebo',1,
                                         ifelse(TRT01A=='Xanomeline Low Dose',2,3)))
attr(G_POPDATA$TRT01AN,"label") <- 'Actual Treatment for Period 01 (n)'
adae <- tibble::as_tibble(repfun:::rfenv$adamdata$adae.rda()) %>%
        dplyr::inner_join(G_POPDATA,
                          by=c('STUDYID','USUBJID','SAFFL','TRT01A')) %>%
        dplyr::filter(TRTEMFL=='Y')
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
                         splitchar="~") %>%
           dplyr::select(STUDYID, USUBJID, TRT01AN, TRT01A, AEBODSYS, AEDECOD)
```
