# Proc-Contents

## Vignette Build Datetime

``` r
message(paste0('Datetime: ',Sys.Date(),':',Sys.time()))
#> Datetime: 2026-01-21:2026-01-21 13:37:14.923435
```

``` r
library(repfun)
```

## Generate a Proc Contents style report of a data frame in R

``` r
#fnam <- paste0(base::tempdir(),"/test-ru_contents.txt")
#sink(fnam)
ret <- repfun::ru_contents(airquality)
#sink()
```

## Display the result

``` r
#cat(readLines(fnam), sep = '\n')
base::cat("\n", ret$s_data_info)
#> 
#>  Data Set Name       airquality                                        
#>  Member Type         Data                                              
#>  Created             NA                                                
#>  Last Modified       NA                                                
#>  Label                                                                 
#>  Observations        153                                               
#>  Variables           6                                                 
#>  Filename            airquality                                        
#>  File Size           0 MB                                              
#>  File Size (bytes)   5632
base::cat(ret$fmt_str)
#> 
#> 
#>      Variable  Type       Len Class       Format      Label
base::cat("\n", ret$s_var_info_1)
#> 
#>    6 Day       integer      4                                                                           
#>    5 Month     integer      4                                                                           
#>    1 Ozone     integer      4                                                                           
#>    2 Solar.R   integer      4                                                                           
#>    4 Temp      integer      4                                                                           
#>    3 Wind      double       8
```
