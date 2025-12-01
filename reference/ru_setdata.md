# Append data sets even when variables do not match

Pass in a collection of data frames separated by commas and they will be
appended.

## Usage

``` r
ru_setdata(..., keeprownames = TRUE)
```

## Arguments

- ...:

  A collection of data frames.

- keeprownames:

  Convert row names on data frame to columns and keep the column.

## Value

The incoming data frames combined.

## Author

Yongwei Wang, <yongwei.x.wang@viivhealthcare.com>  
Chris Rook, <cr883296@gmail.com>

## Examples

``` r
library(repfun)
repfun::ru_setdata(head(mtcars,5),head(airquality,5))
#>            Row.names  mpg cyl disp  hp drat    wt  qsec vs am gear carb Ozone
#> 1          Mazda RX4 21.0   6  160 110 3.90 2.620 16.46  0  1    4    4    NA
#> 2      Mazda RX4 Wag 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4    NA
#> 3         Datsun 710 22.8   4  108  93 3.85 2.320 18.61  1  1    4    1    NA
#> 4     Hornet 4 Drive 21.4   6  258 110 3.08 3.215 19.44  1  0    3    1    NA
#> 5  Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2    NA
#> 6                  1   NA  NA   NA  NA   NA    NA    NA NA NA   NA   NA    41
#> 7                  2   NA  NA   NA  NA   NA    NA    NA NA NA   NA   NA    36
#> 8                  3   NA  NA   NA  NA   NA    NA    NA NA NA   NA   NA    12
#> 9                  4   NA  NA   NA  NA   NA    NA    NA NA NA   NA   NA    18
#> 10                 5   NA  NA   NA  NA   NA    NA    NA NA NA   NA   NA    NA
#>    Solar.R Wind Temp Month Day
#> 1       NA   NA   NA    NA  NA
#> 2       NA   NA   NA    NA  NA
#> 3       NA   NA   NA    NA  NA
#> 4       NA   NA   NA    NA  NA
#> 5       NA   NA   NA    NA  NA
#> 6      190  7.4   67     5   1
#> 7      118  8.0   72     5   2
#> 8      149 12.6   74     5   3
#> 9      313 11.5   62     5   4
#> 10      NA 14.3   56     5   5
```
