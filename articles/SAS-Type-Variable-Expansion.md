# SAS-Type-Variable-Expansion

## Vignette Build Datetime

``` r
message(paste0('Datetime: ',Sys.Date(),':',Sys.time()))
#> Datetime: 2026-01-07:2026-01-07 18:39:49.313772
```

## Load Libraries

``` r
library(repfun)
```

## Example 1: SAS Type Expansion of Variable List

``` r
df <- data.frame(tt_ac01=c('1','2','3'),
                 tt_ac02=c('a','b','b'),
                 tt_ac03=c('10','11','12'))
repfun::ru_expvarlist(df, varsin="tt_ac:")
#> [1] "tt_ac01" "tt_ac02" "tt_ac03"
```

## Example 2: SAS Type Expansion of Variable List

``` r
df <- data.frame(tt_ac01=c('1','2','3'),
                 tt_ac03=c('10','11','12'))
repfun::ru_expvarlist(df, varsin="tt_ac:")
#> [1] "tt_ac01" "tt_ac03"
```

## Example 3: SAS Type Expansion of Variable List

``` r
df <- data.frame(tt_ac01=c('1','2','3'),
                 tt_ac03=c('10','11','12'))
repfun::ru_expvarlist(df, varsin="tt_ac01-tt_ac10")
#> [1] "tt_ac01" "tt_ac03"
```
