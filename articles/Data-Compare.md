# Data-Compare

## Vignette Build Datetime

``` r
message(paste0('Datetime: ',Sys.Date(),':',Sys.time()))
#> Datetime: 2025-12-02:2025-12-02 21:10:30.915148
```

## Load Libraries

``` r
library(repfun)
library(dplyr)
```

## Build Data Frames for Compare

``` r
  iris1 <- iris %>% dplyr::mutate(ID=dplyr::row_number())
  iris2 <- iris %>% dplyr::mutate(ID=dplyr::row_number()) %>% 
    dplyr::mutate(Petal.Length=Petal.Length+1)
```

## Compare the Data Frames

``` r
irisdiffs <- repfun::ru_datacompare(iris1, iris2, idvars=c('Species','ID'), maxprint=10)
```

## Display the Results

``` r
irisdiffs
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
#> [1] 0
#> 
#> $NnubmerOfObsOnlyInBase
#> [1] 150
#> 
#> $NnubmerOfObsOnlyInComp
#> [1] 150
#> 
#> $Diff_Petal.Length
#>    Species ID rowNumber Petal.Length.x Petal.Length.y
#> 1   setosa  1         1            1.4            2.4
#> 2   setosa 10        10            1.5            2.5
#> 3   setosa 11        11            1.5            2.5
#> 4   setosa 12        12            1.6            2.6
#> 5   setosa 13        13            1.4            2.4
#> 6   setosa 14        14            1.1            2.1
#> 7   setosa 15        15            1.2            2.2
#> 8   setosa 16        16            1.5            2.5
#> 9   setosa 17        17            1.3            2.3
#> 10  setosa 18        18            1.4            2.4
```
