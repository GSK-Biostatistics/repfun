# Return a list with codelist, code and label based on input codelist dataset.

Pass in a data set from a SAS format catalog (or similar) and have a
list returned in the structure of a SAS format for decoding variables.

## Usage

``` r
ru_data2codelist(
  dsetin,
  codelistvarname = "FMTNAME",
  codevarname = "START",
  decodevarname = "LABEL",
  typevarname = "TYPE"
)
```

## Arguments

- dsetin:

  Name of incoming data set structured as a SAS format catalog saved as
  a data set.

- codelistvarname:

  Name of the variable containing the SAS format or similar.

- codevarname:

  Name of the variable that holds the code value.

- decodevarname:

  Name of the variable that holds the decode value.

- typevarname:

  Type of format (character or numeric).

## Value

A data frame based on the incoming data frame but with decode values
added along with records when completetypes is true.

## Author

Yongwei Wang, <yongwei.x.wang@viivhealthcare.com>  
Chris Rook, <cr883296@gmail.com>

## Examples

``` r
library(repfun)
datdir <- file.path(gsub("\\","/",tempdir(),fixed=TRUE),"datdir")
dir.create(datdir,showWarnings=FALSE)
repfun::copydata(datdir)
repfun::rs_setup(R_RFMTDIR=datdir)
list <- repfun::ru_data2codelist(repfun:::rfenv$rfmtdata$formats.rda())
list$SEXS$START[[1]] # Code value 1
#> [1] "F"
list$SEXS$LABEL[[1]] # Decode value 1
#> [1] "Female"
list$SEXS$START[[2]] # Code value 2
#> [1] "M"
list$SEXS$LABEL[[2]] # Decode value 2
#> [1] "Male"
```
