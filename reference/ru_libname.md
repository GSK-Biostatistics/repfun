# Automate retrieval of data sets/frames in specified folder.

Provide a folder containing data sets/frames and have a list of function
calls returned that can be used to quickly access individual data
sets/frames.

## Usage

``` r
ru_libname(datapath)
```

## Arguments

- datapath:

  Location of reporting data sets.

## Value

List of function calls for use in quickly accessing individual data
sets/frames.

## Author

Yongwei Wang, <yongwei.x.wang@viivhealthcare.com>  
Chris Rook, <cr883296@gmail.com>

## Examples

``` r
library(repfun)
datdir <- file.path(gsub("\\","/",tempdir(),fixed=TRUE),"datdir")
dir.create(datdir,showWarnings=FALSE)
repfun::copydata(datdir)
adamdata <- repfun::ru_libname(datdir)
adamdata$adae.rda() %>% dplyr::filter(TRTEMFL=='Y') -> teae
print(head(teae[,c(1:10)]),10)
#> # A
#> #   tibble:
#> #   6 ×
#> #   10
#>   STUDYID 
#>   <chr>   
#> 1 CDISCPI…
#> 2 CDISCPI…
#> 3 CDISCPI…
#> 4 CDISCPI…
#> 5 CDISCPI…
#> 6 CDISCPI…
#> # ℹ 9
#> #   more
#> #   variables:
#> #   DOMAIN <chr>,
#> #   USUBJID <chr>,
#> #   AESEQ <dbl>,
#> #   AESPID <chr>, …
```
