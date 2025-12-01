# Fill NA values with specified values or zeros/blanks by default.

Pass in a data frame along with a vector of variables and a vector of
fill values. (Default fill is 0 for numeric and blank " " for
character.)

## Usage

``` r
ru_fillna(dsetin, vars = NULL, fills = NULL)
```

## Arguments

- dsetin:

  Incoming data frame to have labels added to columns.

- vars:

  Vector of variables to replace NA values.

- fills:

  Vector of fill values.

## Value

The incoming data frame with the requested NA values replaced.

## Author

Yongwei Wang, <yongwei.x.wang@viivhealthcare.com>  
Chris Rook, <cr883296@gmail.com>

## Examples

``` r
library(repfun)
repfun::ru_fillna(airquality, vars=c('Ozone','Solar.R'), fills=c(1111,2222)) %>% head(10)
#>    Ozone Solar.R Wind Temp Month Day
#> 1     41     190  7.4   67     5   1
#> 2     36     118  8.0   72     5   2
#> 3     12     149 12.6   74     5   3
#> 4     18     313 11.5   62     5   4
#> 5   1111    2222 14.3   56     5   5
#> 6     28    2222 14.9   66     5   6
#> 7     23     299  8.6   65     5   7
#> 8     19      99 13.8   59     5   8
#> 9      8      19 20.1   61     5   9
#> 10  1111     194  8.6   69     5  10
```
