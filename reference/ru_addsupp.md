# Cobmine an SDTM domain with its corresponding Supplemental data set

Pass in an SDTM data frame along with its Supplemental version and they
will be combined.

## Usage

``` r
ru_addsupp(dsetin, dsetinsupp)
```

## Arguments

- dsetin:

  Incoming data frame to have supplemental data added.

- dsetinsupp:

  The supplemental data set to add.

## Value

The original SDTM data set with its supplemental data appended as new
variables.

## Author

Yongwei Wang, <yongwei.x.wang@viivhealthcare.com>  
Chris Rook, <cr883296@gmail.com>

## Examples

``` r
library(repfun)
library(knitr)
ae <- repfun::ae
suppae <- repfun::suppae
aesupp <- repfun::ru_addsupp(dsetin=ae,dsetinsupp=suppae)
knitr::kable(head(aesupp,5), caption = "SDTM.AE combined with SDTM.SUPPAE")
#> 
#> 
#> Table: SDTM.AE combined with SDTM.SUPPAE
#> 
#> |STUDYID      |DOMAIN |USUBJID     | AESEQ|AESPID |AETERM                               |AELLT                    | AELLTCD|AEDECOD                              | AEPTCD|AEHLT    | AEHLTCD|AEHLGT    | AEHLGTCD|AEBODSYS                                             | AEBDSYCD|AESOC                                                | AESOCCD|AESEV |AESER |AEACN |AEREL    |AEOUT                      |AESCAN |AESCONG |AESDISAB |AESDTH |AESHOSP |AESLIFE |AESOD |AEDTC      |AESTDTC    |AEENDTC    | AESTDY| AEENDY|AETRTEM |
#> |:------------|:------|:-----------|-----:|:------|:------------------------------------|:------------------------|-------:|:------------------------------------|------:|:--------|-------:|:---------|--------:|:----------------------------------------------------|--------:|:----------------------------------------------------|-------:|:-----|:-----|:-----|:--------|:--------------------------|:------|:-------|:--------|:------|:-------|:-------|:-----|:----------|:----------|:----------|------:|------:|:-------|
#> |CDISCPILOT01 |AE     |01-701-1015 |     1|E07    |APPLICATION SITE ERYTHEMA            |APPLICATION SITE REDNESS |      NA|APPLICATION SITE ERYTHEMA            |     NA|HLT_0617 |      NA|HLGT_0152 |       NA|GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS |       NA|GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS |      NA|MILD  |N     |NA    |PROBABLE |NOT RECOVERED/NOT RESOLVED |N      |N       |N        |N      |N       |N       |N     |2014-01-16 |2014-01-03 |NA         |      2|     NA|Y       |
#> |CDISCPILOT01 |AE     |01-701-1015 |     2|E08    |APPLICATION SITE PRURITUS            |APPLICATION SITE ITCHING |      NA|APPLICATION SITE PRURITUS            |     NA|HLT_0317 |      NA|HLGT_0338 |       NA|GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS |       NA|GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS |      NA|MILD  |N     |NA    |PROBABLE |NOT RECOVERED/NOT RESOLVED |N      |N       |N        |N      |N       |N       |N     |2014-01-16 |2014-01-03 |NA         |      2|     NA|Y       |
#> |CDISCPILOT01 |AE     |01-701-1015 |     3|E06    |DIARRHOEA                            |DIARRHEA                 |      NA|DIARRHOEA                            |     NA|HLT_0148 |      NA|HLGT_0588 |       NA|GASTROINTESTINAL DISORDERS                           |       NA|GASTROINTESTINAL DISORDERS                           |      NA|MILD  |N     |NA    |REMOTE   |RECOVERED/RESOLVED         |N      |N       |N        |N      |N       |N       |N     |2014-01-16 |2014-01-09 |2014-01-11 |      8|     10|Y       |
#> |CDISCPILOT01 |AE     |01-701-1023 |     3|E10    |ATRIOVENTRICULAR BLOCK SECOND DEGREE |AV BLOCK SECOND DEGREE   |      NA|ATRIOVENTRICULAR BLOCK SECOND DEGREE |     NA|HLT_0415 |      NA|HLGT_0086 |       NA|CARDIAC DISORDERS                                    |       NA|CARDIAC DISORDERS                                    |      NA|MILD  |N     |NA    |POSSIBLE |NOT RECOVERED/NOT RESOLVED |N      |N       |N        |N      |N       |N       |N     |2012-08-27 |2012-08-26 |NA         |     22|     NA|Y       |
#> |CDISCPILOT01 |AE     |01-701-1023 |     1|E08    |ERYTHEMA                             |ERYTHEMA                 |      NA|ERYTHEMA                             |     NA|HLT_0284 |      NA|HLGT_0192 |       NA|SKIN AND SUBCUTANEOUS TISSUE DISORDERS               |       NA|SKIN AND SUBCUTANEOUS TISSUE DISORDERS               |      NA|MILD  |N     |NA    |POSSIBLE |NOT RECOVERED/NOT RESOLVED |N      |N       |N        |N      |N       |N       |N     |2012-08-27 |2012-08-07 |2012-08-30 |      3|     26|Y       |
```
