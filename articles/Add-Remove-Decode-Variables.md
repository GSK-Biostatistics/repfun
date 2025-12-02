# Add-Remove-Decode-Variables

## Vignette Build Datetime

``` r
message(paste0('Datetime: ',Sys.Date(),':',Sys.time()))
#> Datetime: 2025-12-02:2025-12-02 20:25:08.998274
```

``` r
library(repfun)
```

## Add Decode Variables to the Incoming List

``` r
add_decode <- repfun::ru_groupbyvars(c("TRTCD", "TRTGRP", "ATOXGRN", "AEDECOD", "AEBODSYS"), 
                             c("TRTCD", "TRTGRP", "ATOXGRN", "ATOXGR"), TRUE)
print(add_decode)
#> [1] "TRTCD"    "TRTGRP"   "ATOXGRN"  "ATOXGR"   "AEDECOD"  "AEBODSYS"
```

## Remove Decode Variables from the Incoming List

``` r
rem_decode <- repfun::ru_groupbyvars(c("TRTCD", "TRTGRP", "ATOXGRN", "AEDECOD", "AEBODSYS"), 
                             c("TRTCD", "TRTGRP", "ATOXGRN", "ATOXGR"), FALSE)
print(rem_decode)
#> [1] "TRTCD"    "ATOXGRN"  "AEDECOD"  "AEBODSYS"
```
