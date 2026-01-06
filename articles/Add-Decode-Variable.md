# Add-Decode-Variable

## Vignette Build Datetime

``` r
message(paste0('Datetime: ',Sys.Date(),':',Sys.time()))
#> Datetime: 2026-01-06:2026-01-06 21:01:06.984354
```

## Load Libraries

``` r
library(repfun)
library(dplyr)
```

## Define data library

``` r
tmpdr <- tempdir()
datdir <- file.path(gsub("\\","/",tmpdr,fixed=TRUE),"datdir")
fname <- system.file("formats", "formats.sas7bdat", package = "repfun")
dir.create(datdir,showWarnings=FALSE)
file.copy(from=fname,to=paste0(datdir,'/formats.sas7bdat'))
#> [1] TRUE
rfmtdir <- repfun::ru_libname(datdir)
```

## Read in Format Data Set

``` r
fmtdata <- rfmtdir$formats()
```

## Make List from Format Data Set

``` r
fmtlist <- repfun::ru_data2codelist(fmtdata,
                            codelistvarname="FMTNAME",
                            codevarname="START",
                            decodevarname="LABEL",
                            typevarname="TYPE")
```

## Apply the List to Create a Decode Variable on ADSL

``` r
adsl <- repfun::adsl
addvar <- repfun::ru_fillcodedcode(adsl, 
                           codedecodevarpairs=c("SEX", "SEXDCD"), 
                           varcodelistpairs=c("SEX", "SEXS"), 
                           codelistnames=fmtlist)  %>% 
          dplyr::select(SEX,SEXDCD) %>%
          repfun::ru_labels(varlabels=list('SEXDCD'='Sex Decode'))
```

## Display the Results

``` r
lbls <- sapply(addvar,function(x){attr(x,"label")})
knitr::kable(head(addvar,10), col.names=paste(names(lbls),lbls,sep=": "), 
             caption = "Add Decode Variable for ADSL$SEX")
```

| SEX: Sex | SEXDCD: Sex Decode |
|:---------|:-------------------|
| F        | Female             |
| F        | Female             |
| F        | Female             |
| F        | Female             |
| F        | Female             |
| F        | Female             |
| F        | Female             |
| F        | Female             |
| F        | Female             |
| F        | Female             |

Add Decode Variable for ADSL\$SEX
