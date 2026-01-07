# Add-Labels

## Vignette Build Datetime

``` r
message(paste0('Datetime: ',Sys.Date(),':',Sys.time()))
#> Datetime: 2026-01-07:2026-01-07 18:38:00.428993
```

``` r
library(repfun)
```

## Before Adding Labels

``` r
knitr::kable(head(mtcars,5), caption = "Before Invoking repfun::ru_labels()")
```

|                   |  mpg | cyl | disp |  hp | drat |    wt |  qsec |  vs |  am | gear | carb |
|:------------------|-----:|----:|-----:|----:|-----:|------:|------:|----:|----:|-----:|-----:|
| Mazda RX4         | 21.0 |   6 |  160 | 110 | 3.90 | 2.620 | 16.46 |   0 |   1 |    4 |    4 |
| Mazda RX4 Wag     | 21.0 |   6 |  160 | 110 | 3.90 | 2.875 | 17.02 |   0 |   1 |    4 |    4 |
| Datsun 710        | 22.8 |   4 |  108 |  93 | 3.85 | 2.320 | 18.61 |   1 |   1 |    4 |    1 |
| Hornet 4 Drive    | 21.4 |   6 |  258 | 110 | 3.08 | 3.215 | 19.44 |   1 |   0 |    3 |    1 |
| Hornet Sportabout | 18.7 |   8 |  360 | 175 | 3.15 | 3.440 | 17.02 |   0 |   0 |    3 |    2 |

Before Invoking repfun::ru_labels()

## After Adding Labels Using Base Method

``` r
df <- repfun::ru_labels(mtcars,varlabels=list(mpg='Miles Per Gallon',cyl='Number of Cylinders'))
knitr::kable(head(df,5), col.names=sapply(df,function(x){attr(x,"label")}),
             caption = "After Invoking repfun::ru_labels() - style = 'base'")
```

|                   | Miles Per Gallon | Number of Cylinders | disp |  hp | drat |    wt |  qsec |  vs |  am | gear | carb |
|:------------------|-----------------:|--------------------:|-----:|----:|-----:|------:|------:|----:|----:|-----:|-----:|
| Mazda RX4         |             21.0 |                   6 |  160 | 110 | 3.90 | 2.620 | 16.46 |   0 |   1 |    4 |    4 |
| Mazda RX4 Wag     |             21.0 |                   6 |  160 | 110 | 3.90 | 2.875 | 17.02 |   0 |   1 |    4 |    4 |
| Datsun 710        |             22.8 |                   4 |  108 |  93 | 3.85 | 2.320 | 18.61 |   1 |   1 |    4 |    1 |
| Hornet 4 Drive    |             21.4 |                   6 |  258 | 110 | 3.08 | 3.215 | 19.44 |   1 |   0 |    3 |    1 |
| Hornet Sportabout |             18.7 |                   8 |  360 | 175 | 3.15 | 3.440 | 17.02 |   0 |   0 |    3 |    2 |

After Invoking repfun::ru_labels() - style = ‘base’

## After Adding Labels Using Hmisc Method

``` r
df2 <- repfun::ru_labels(mtcars,varlabels=list(mpg='Miles Per Gallon',
                                       cyl='Number of Cylinders'),style='hmisc')
knitr::kable(head(df,5), col.names=sapply(df2,function(x){attr(x,"label")}),
             caption = "After Invoking repfun::ru_labels() - style = 'hmisc'")
```

|                   | Miles Per Gallon | Number of Cylinders | disp |  hp | drat |    wt |  qsec |  vs |  am | gear | carb |
|:------------------|-----------------:|--------------------:|-----:|----:|-----:|------:|------:|----:|----:|-----:|-----:|
| Mazda RX4         |             21.0 |                   6 |  160 | 110 | 3.90 | 2.620 | 16.46 |   0 |   1 |    4 |    4 |
| Mazda RX4 Wag     |             21.0 |                   6 |  160 | 110 | 3.90 | 2.875 | 17.02 |   0 |   1 |    4 |    4 |
| Datsun 710        |             22.8 |                   4 |  108 |  93 | 3.85 | 2.320 | 18.61 |   1 |   1 |    4 |    1 |
| Hornet 4 Drive    |             21.4 |                   6 |  258 | 110 | 3.08 | 3.215 | 19.44 |   1 |   0 |    3 |    1 |
| Hornet Sportabout |             18.7 |                   8 |  360 | 175 | 3.15 | 3.440 | 17.02 |   0 |   0 |    3 |    2 |

After Invoking repfun::ru_labels() - style = ‘hmisc’
