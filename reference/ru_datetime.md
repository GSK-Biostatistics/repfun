# Add numeric datetimes to data frame that only has character versions.

Pass in a data frame and variables ending in "DTC" will have
corresponding numeric versions created and saved.

## Usage

``` r
ru_datetime(dsetin, includevars = NULL)
```

## Arguments

- dsetin:

  Incoming data frame to have numeric datetimes added.

- includevars:

  Specify which variables ending in DTC will be processed.

## Value

The incoming data frame with numeric dates and times added will be
returned.

## Author

Yongwei Wang, <yongwei.x.wang@viivhealthcare.com>  
Chris Rook, <cr883296@gmail.com>

## Examples

``` r
library(repfun)
library(knitr)
ae <- repfun::ru_datetime(repfun::ae)
knitr::kable(head(ae[,grepl('(DT$|TM$|DTC$)',names(ae))],5),
             caption = "After Invoking ru_labels()")
#> 
#> 
#> Table: After Invoking ru_labels()
#> 
#> |AEDTC      |AESTDTC    |AEENDTC    |AEDT       | AETM|AEDTM |AESTDT     | AESTTM|AESTDTM |AEENDT     | AEENTM|AEENDTM |
#> |:----------|:----------|:----------|:----------|----:|:-----|:----------|------:|:-------|:----------|------:|:-------|
#> |2014-01-16 |2014-01-03 |NA         |2014-01-16 |   NA|NA    |2014-01-03 |     NA|NA      |NA         |     NA|NA      |
#> |2014-01-16 |2014-01-03 |NA         |2014-01-16 |   NA|NA    |2014-01-03 |     NA|NA      |NA         |     NA|NA      |
#> |2014-01-16 |2014-01-09 |2014-01-11 |2014-01-16 |   NA|NA    |2014-01-09 |     NA|NA      |2014-01-11 |     NA|NA      |
#> |2012-08-27 |2012-08-26 |NA         |2012-08-27 |   NA|NA    |2012-08-26 |     NA|NA      |NA         |     NA|NA      |
#> |2012-08-27 |2012-08-07 |2012-08-30 |2012-08-27 |   NA|NA    |2012-08-07 |     NA|NA      |2012-08-30 |     NA|NA      |
```
