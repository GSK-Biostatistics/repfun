# Convert-Format-Data-to-Codelist

## Vignette Build Datetime

``` r
message(paste0('Datetime: ',Sys.Date(),':',Sys.time()))
#> Datetime: 2026-01-06:2026-01-06 18:23:18.396973
```

## Load Libraries

``` r
library(repfun)
```

## Invoke Setup Function

``` r
tmpdr <- tempdir()
datdir <- file.path(gsub("\\","/",tmpdr,fixed=TRUE),"datdir")
fname <- system.file("formats", "formats.sas7bdat", package = "repfun")
dir.create(datdir,showWarnings=FALSE)
file.copy(from=fname,to=paste0(datdir,'/formats.sas7bdat'))
#> [1] TRUE
repfun::rs_setup(R_RFMTDIR=datdir)
```

## Convert RFMTDIR Format Data Set to List

``` r
list <- repfun::ru_data2codelist(repfun:::rfenv$rfmtdata$formats())
```

## Display the Results

``` r
message(paste0('First Code Value: ',list$SEXS$START[[1]])) # Code value 1
#> First Code Value: F
message(paste0('First Decode Value: ',list$SEXS$LABEL[[1]])) # Decode value 1
#> First Decode Value: Female
message(paste0('Second Code Value: ',list$SEXS$START[[2]])) # Code value 2
#> Second Code Value: M
message(paste0('Second Decode Value: ',list$SEXS$LABEL[[2]])) # Decode value 2
#> Second Decode Value: Male
```
