# Fill-Missings

## Vignette Build Datetime

``` r
message(paste0('Datetime: ',Sys.Date(),':',Sys.time()))
#> Datetime: 2026-01-02:2026-01-02 20:04:24.771511
```

## Load Libraries

``` r
library(repfun)
library(dplyr)
```

## Before Filling Missings

``` r
airquality %>% mutate(CharVar1=ifelse(row_number() %% 2,'Text String 1',NA),
                      CharVar2=ifelse(row_number() %% 3,'Text String 2',NA)) -> df
knitr::kable(head(df,10), caption = "Before Invoking repfun::ru_fillna()")
```

| Ozone | Solar.R | Wind | Temp | Month | Day | CharVar1      | CharVar2      |
|------:|--------:|-----:|-----:|------:|----:|:--------------|:--------------|
|    41 |     190 |  7.4 |   67 |     5 |   1 | Text String 1 | Text String 2 |
|    36 |     118 |  8.0 |   72 |     5 |   2 | NA            | Text String 2 |
|    12 |     149 | 12.6 |   74 |     5 |   3 | Text String 1 | NA            |
|    18 |     313 | 11.5 |   62 |     5 |   4 | NA            | Text String 2 |
|    NA |      NA | 14.3 |   56 |     5 |   5 | Text String 1 | Text String 2 |
|    28 |      NA | 14.9 |   66 |     5 |   6 | NA            | NA            |
|    23 |     299 |  8.6 |   65 |     5 |   7 | Text String 1 | Text String 2 |
|    19 |      99 | 13.8 |   59 |     5 |   8 | NA            | Text String 2 |
|     8 |      19 | 20.1 |   61 |     5 |   9 | Text String 1 | NA            |
|    NA |     194 |  8.6 |   69 |     5 |  10 | NA            | Text String 2 |

Before Invoking repfun::ru_fillna()

## After Filling Missings

``` r
df2 <- repfun::ru_fillna(df, vars=c('Ozone','Solar.R','CharVar1','CharVar2'), 
                 fills=c(1111,2222,'AAAA','BBBB'))
knitr::kable(head(df2,10), caption = "After Invoking repfun::ru_fillna()")
```

| Ozone | Solar.R | Wind | Temp | Month | Day | CharVar1      | CharVar2      |
|------:|--------:|-----:|-----:|------:|----:|:--------------|:--------------|
|    41 |     190 |  7.4 |   67 |     5 |   1 | Text String 1 | Text String 2 |
|    36 |     118 |  8.0 |   72 |     5 |   2 | AAAA          | Text String 2 |
|    12 |     149 | 12.6 |   74 |     5 |   3 | Text String 1 | BBBB          |
|    18 |     313 | 11.5 |   62 |     5 |   4 | AAAA          | Text String 2 |
|  1111 |    2222 | 14.3 |   56 |     5 |   5 | Text String 1 | Text String 2 |
|    28 |    2222 | 14.9 |   66 |     5 |   6 | AAAA          | BBBB          |
|    23 |     299 |  8.6 |   65 |     5 |   7 | Text String 1 | Text String 2 |
|    19 |      99 | 13.8 |   59 |     5 |   8 | AAAA          | Text String 2 |
|     8 |      19 | 20.1 |   61 |     5 |   9 | Text String 1 | BBBB          |
|  1111 |     194 |  8.6 |   69 |     5 |  10 | AAAA          | Text String 2 |

After Invoking repfun::ru_fillna()
