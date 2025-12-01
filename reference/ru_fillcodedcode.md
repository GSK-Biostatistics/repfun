# Fill missing code/decode records

Pass in a data frame with along code/decode variables and values to have
missings populated.

## Usage

``` r
ru_fillcodedcode(
  dsetin,
  codedecodevarpairs = NULL,
  varcodelistpairs = NULL,
  codelistnames = list(),
  groupbyvars = NULL,
  completetypes = TRUE
)
```

## Arguments

- dsetin:

  The data set that will be counted to generate numerators for counts
  and percents.

- codedecodevarpairs:

  Specifies code and decode variable pairs. Those variables should be in
  parameter GROUPBYVARSNUMER. One variable in the pair will contain the
  code, which is used in counting and ordering, and the other will
  contain decode, which is used for presentation.

- varcodelistpairs:

  List of code/decode pairs of variables.

- codelistnames:

  List of decodes for use with decoding code/decode pairs.

- groupbyvars:

  Set of by-variables used to merge the incoming data set with the
  decode data set.

- completetypes:

  Keep all code/decode pairs even it not present on the incoming data
  set?

## Value

A data frame based on the incoming data frame but with decode values
added along with records when completetypes is true.

## Author

Yongwei Wang, <yongwei.x.wang@viivhealthcare.com>  
Chris Rook, <cr883296@gmail.com>

## Examples

``` r
library(repfun)
fmtlist <- list('SEXS'=list('START'=list('M','F'),
                'LABEL'=c('Male','Female')))
adsl <- repfun::adsl
adsl2 <- repfun::ru_fillcodedcode(adsl, codedecodevarpairs=c("SEX", "SEXDCD"),
                          varcodelistpairs=c("SEX", "SEXS"),
                          codelistnames=fmtlist)
unique(adsl2[c("SEX","SEXDCD")])
#>     SEX SEXDCD
#> 1     F Female
#> 180   M   Male
```
