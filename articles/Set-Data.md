# Set-Data

## Vignette Build Datetime

``` r
message(paste0('Datetime: ',Sys.Date(),':',Sys.time()))
#> Datetime: 2026-01-06:2026-01-06 21:03:00.596445
```

## Load Libraries

``` r
library(repfun)
library(dplyr)
```

## Append data frames with different columns

``` r
  fun <- repfun::ru_setdata(head(mtcars,5),head(airquality,5))
  fun[['Row.names']] <- as.character(fun$Row.names)
```

## Display the Results

``` r
knitr::kable(fun, caption = "Combining Data Frames with Different Columns")
```

| Row.names         |  mpg | cyl | disp |  hp | drat |    wt |  qsec |  vs |  am | gear | carb | Ozone | Solar.R | Wind | Temp | Month | Day |
|:------------------|-----:|----:|-----:|----:|-----:|------:|------:|----:|----:|-----:|-----:|------:|--------:|-----:|-----:|------:|----:|
| Mazda RX4         | 21.0 |   6 |  160 | 110 | 3.90 | 2.620 | 16.46 |   0 |   1 |    4 |    4 |    NA |      NA |   NA |   NA |    NA |  NA |
| Mazda RX4 Wag     | 21.0 |   6 |  160 | 110 | 3.90 | 2.875 | 17.02 |   0 |   1 |    4 |    4 |    NA |      NA |   NA |   NA |    NA |  NA |
| Datsun 710        | 22.8 |   4 |  108 |  93 | 3.85 | 2.320 | 18.61 |   1 |   1 |    4 |    1 |    NA |      NA |   NA |   NA |    NA |  NA |
| Hornet 4 Drive    | 21.4 |   6 |  258 | 110 | 3.08 | 3.215 | 19.44 |   1 |   0 |    3 |    1 |    NA |      NA |   NA |   NA |    NA |  NA |
| Hornet Sportabout | 18.7 |   8 |  360 | 175 | 3.15 | 3.440 | 17.02 |   0 |   0 |    3 |    2 |    NA |      NA |   NA |   NA |    NA |  NA |
| 1                 |   NA |  NA |   NA |  NA |   NA |    NA |    NA |  NA |  NA |   NA |   NA |    41 |     190 |  7.4 |   67 |     5 |   1 |
| 2                 |   NA |  NA |   NA |  NA |   NA |    NA |    NA |  NA |  NA |   NA |   NA |    36 |     118 |  8.0 |   72 |     5 |   2 |
| 3                 |   NA |  NA |   NA |  NA |   NA |    NA |    NA |  NA |  NA |   NA |   NA |    12 |     149 | 12.6 |   74 |     5 |   3 |
| 4                 |   NA |  NA |   NA |  NA |   NA |    NA |    NA |  NA |  NA |   NA |   NA |    18 |     313 | 11.5 |   62 |     5 |   4 |
| 5                 |   NA |  NA |   NA |  NA |   NA |    NA |    NA |  NA |  NA |   NA |   NA |    NA |      NA | 14.3 |   56 |     5 |   5 |

Combining Data Frames with Different Columns
