# Assign Big N to Data Frame.

Merge input data with population data to keep only subjects which are in
population sub data,

## Usage

``` r
ru_getdata(
  dsetin,
  dsetinpop = rfenv$G_POPDATA,
  subjidvars = c("STUDYID", "USUBJID"),
  subpop = rfenv$G_SUBPOP,
  pop = rfenv$G_POP,
  keeppopvars = rfenv$G_KEEPPOPVARS
)
```

## Arguments

- dsetin:

  The data set that will be merged with the population data set.

- dsetinpop:

  The population data set.

- subjidvars:

  Variable(s) that define a unique subject.

- subpop:

  A sub-population expression where variables are on DSETINPOP.

- pop:

  The population expression (SAFFL=='Y').

- keeppopvars:

  Variables to keep on the population data set.

## Value

A data frame based on the incoming data frame but restricted to the
population of interest with relevant population variables added.

## Author

Yongwei Wang, <yongwei.x.wang@viivhealthcare.com>  
Chris Rook, <cr883296@gmail.com>

## Examples

``` r
library(repfun)
library(dplyr)
datdir <- file.path(gsub("\\","/",tempdir(),fixed=TRUE),"datdir")
dir.create(datdir,showWarnings=FALSE)
outdir <- file.path(gsub("\\","/",tempdir(),fixed=TRUE),"outdir")
dir.create(outdir,showWarnings=FALSE)
fmtdir <- file.path(gsub("\\","/",tempdir(),fixed=TRUE),"fmtdir")
dir.create(fmtdir,showWarnings=FALSE)
repfun::copydata(datdir)
repfun::rs_setup(D_POP="SAFFL",
                 D_POPLBL="Safety",
                 D_POPDATA=repfun::adsl,
                 D_SUBJID=c("STUDYID","USUBJID"),
                 R_DICTION=NULL,
                 R_OTHERDATA=NULL,
                 R_INPUTDATA=NULL,
                 R_RAWDATA=NULL,
                 R_SDTMDATA=NULL,
                 R_ADAMDATA=datdir)
G_POPDATA <- repfun:::rfenv$G_POPDATA %>%
  dplyr::mutate(TRT01AN=ifelse(TRT01A=='Placebo',1,
                 ifelse(TRT01A=='Xanomeline Low Dose',2,3)),
         SAFFL=ifelse((row_number() %% 10) == 0,'N',SAFFL))
attr(G_POPDATA$TRT01AN,"label") <- 'Actual Treatment for Period 01 (n)'
attr(G_POPDATA$SAFFL,"label") <- 'Safety Population Flag'
adae <- repfun:::rfenv$adamdata$adae.rda() %>% dplyr::select(-SAFFL)
adae2 <- repfun::ru_getdata(adae, G_POPDATA, c("STUDYID", "USUBJID"),
                    keeppopvars=c("TRT01AN", "TRT01A"))
```
