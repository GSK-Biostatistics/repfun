# Modify groupbyvars by adding or removing decode vars.

Pass in a vector of group-by variables along with a vector of
code/decode pairs to have decode variables added or removed.

## Usage

``` r
ru_groupbyvars(groupbyvars, codedecodevarpairs, adddecode = TRUE)
```

## Arguments

- groupbyvars:

  Vector of group-by variables.

- codedecodevarpairs:

  Specifies code and decode variable pairs. Those variables should be in
  parameter GROUPBYVARSNUMER. One variable in the pair will contain the
  code, which is used in counting and ordering, and the other will
  contain decode, which is used for presentation.

- adddecode:

  Add decode variables (true) or remove (false).

## Value

A data frame based on the incoming data frame but collapsed by groups
with counts and percents added.

## Author

Yongwei Wang, <yongwei.x.wang@viivhealthcare.com>  
Chris Rook, <cr883296@gmail.com>

## Examples

``` r
library(repfun)
add_decode <- repfun::ru_groupbyvars(
              c("TRTCD", "TRTGRP", "ATOXGRN", "AEDECOD", "AEBODSYS"),
              c("TRTCD", "TRTGRP", "ATOXGRN", "ATOXGR"), TRUE)
rem_decode <- repfun::ru_groupbyvars(
              c("TRTCD", "TRTGRP", "ATOXGRN", "AEDECOD", "AEBODSYS"),
              c("TRTCD", "TRTGRP", "ATOXGRN", "ATOXGR"), FALSE)
```
