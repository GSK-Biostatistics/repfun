# Compare 2 data frames and report differences.

Pass in a base and compare data frame to find out if they are equal
similar to proc compare in SAS.

## Usage

``` r
ru_datacompare(dsetinbase, dsetincomp, idvars, maxprint = 50)
```

## Arguments

- dsetinbase:

  First data set.

- dsetincomp:

  Second data set.

- idvars:

  Match on these values prior to comparing records.

- maxprint:

  Maximum number of differences per variable to display.

## Value

An output similar to proc compare will be printed and returned as a
list.

## Author

Yongwei Wang, <yongwei.x.wang@viivhealthcare.com>  
Chris Rook, <cr883296@gmail.com>

## Examples

``` r
library(repfun)
repfun::ru_datacompare(iris,iris,idvars='Species')
#> $NamesOnlyInBase
#> character(0)
#> 
#> $NamesOnlyInComp
#> character(0)
#> 
#> $DiffentTypes
#> named logical(0)
#> 
#> $DifferentAttr
#> named list()
#> 
#> $NnubmerOfObsInCommon
#> [1] 152
#> 
#> $NnubmerOfObsOnlyInBase
#> [1] 0
#> 
#> $NnubmerOfObsOnlyInComp
#> [1] 0
#> 
```
