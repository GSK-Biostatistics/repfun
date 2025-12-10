# Pass values to setup function and the global environment will be arranged for use reporting tools.

Pass values to setup function and the global environment will be
arranged for use reporting tools.

## Usage

``` r
rs_setup(
  R_DICTION = "./inst/formats",
  R_MACDIRS = NULL,
  R_DDDATA = NULL,
  R_OTHERDATA = paste0("./data"),
  R_INPUTDATA = paste0("./data"),
  R_RAWDATA = paste0("./data"),
  R_SDTMDATA = paste0("./data"),
  R_ADAMDATA = paste0("./data"),
  R_RFMTDIR = paste0("./inst/formats"),
  D_CENTID = "SITEID",
  D_DATADATE = NULL,
  D_DSPLYNUM = 1,
  D_DSPLYTYP = "T",
  D_FONTSIZE = 10,
  D_KEEPPOPVARS = NULL,
  D_OUTFILE = "./inst/t_ru_list_1.rtf",
  D_PGMPTH = "./R/rs_setup.R",
  D_STUDYID = NULL,
  D_STUDY_DESC = NULL,
  D_POP = "ITTEFL",
  D_POPDATA = NULL,
  D_POPLBL = "Intent-to-Treat",
  D_SUBJID = "USUBJID",
  D_SUBPOP = NULL,
  D_SUBSET = NULL,
  D_TITLE1 = NULL,
  D_TITLE2 = NULL,
  D_TITLE3 = NULL,
  D_TITLE4 = NULL,
  D_TITLE5 = NULL,
  D_TITLE6 = NULL,
  D_TITLE7 = NULL,
  D_FOOT1 = NULL,
  D_FOOT2 = NULL,
  D_FOOT3 = NULL,
  D_FOOT4 = NULL,
  D_FOOT5 = NULL,
  D_FOOT6 = NULL,
  D_FOOT7 = NULL,
  D_FOOT8 = NULL,
  D_FOOT9 = NULL,
  D_USERID = Sys.getenv("USERNAME"),
  D_RTFYN = "N",
  D_DEBUG = 0
)
```

## Arguments

- R_DICTION:

  Location of reporting dictionaries.

- R_MACDIRS:

  List of folders to search for functions when they are invoked.

- R_DDDATA:

  Location to write DDDATA reporting data sets.

- R_OTHERDATA:

  Location of additional production data sets.

- R_INPUTDATA:

  Location of permanent formats data sets and miscellaneous data sets.

- R_RAWDATA:

  Location of raw data sets.

- R_SDTMDATA:

  Location of SDTM data sets.

- R_ADAMDATA:

  Location of ADAM data sets.

- R_RFMTDIR:

  Location of format catalogs and corresponding lists.

- D_CENTID:

  Variable name for investigational center.

- D_DATADATE:

  Date of data sets for use in titles/footnotes.

- D_DSPLYNUM:

  Display number for title.

- D_DSPLYTYP:

  Type of Display ('T','L','F').

- D_FONTSIZE:

  Size of font on output file which is RTF by default (with automated
  PDF conversion via script).

- D_KEEPPOPVARS:

  Variables to keep on the population data set when merging to apply
  populations and sub-populations.

- D_OUTFILE:

  Production location for output TLFs.

- D_PGMPTH:

  Path of the driver file that generates current outputs or data sets.

- D_STUDYID:

  Protocol ID for the study.

- D_STUDY_DESC:

  Description of Study.

- D_POP:

  Population variable from ADSL that must equal Y for subjects to be
  included in the analysis.

- D_POPDATA:

  Data set that contains the population to be analyzed.

- D_POPLBL:

  Label for population being analyzed which can be used in the TLF
  header.

- D_SUBJID:

  The variable used to uniquely identify a subject in this analysis.

- D_SUBPOP:

  Condition to identify the sub-population when applied to ADSL.

- D_SUBSET:

  Condition to filter data from incoming source data sets used for this
  TLF.

- D_TITLE1:

  First title text.

- D_TITLE2:

  Second title text.

- D_TITLE3:

  Third title text.

- D_TITLE4:

  Fourth title text.

- D_TITLE5:

  Fifth title text.

- D_TITLE6:

  Sixth title text.

- D_TITLE7:

  Seventh title text.

- D_FOOT1:

  First footnote text.

- D_FOOT2:

  Second footnote text.

- D_FOOT3:

  Third footnote text.

- D_FOOT4:

  Fourth footnote text.

- D_FOOT5:

  Fifth footnote text.

- D_FOOT6:

  Sixth footnote text.

- D_FOOT7:

  Seventh footnote text.

- D_FOOT8:

  Eighth footnote text.

- D_FOOT9:

  Ninth footnote text.

- D_USERID:

  User name.

- D_RTFYN:

  Y or N to generate RTf output.

- D_DEBUG:

  Level of debugging to show in log files.

## Value

'NULL' because variables defined for use with the reporting tools.

## Author

Yongwei Wang, <yongwei.x.wang@viivhealthcare.com>  
Chris Rook, <cr883296@gmail.com>

## Examples

``` r
library(repfun)
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
datdir <- file.path(gsub("\\","/",tempdir(),fixed=TRUE),"datdir")
dir.create(datdir,showWarnings=FALSE)
outdir <- file.path(gsub("\\","/",tempdir(),fixed=TRUE),"outdir")
dir.create(outdir,showWarnings=FALSE)
fmtdir <- file.path(gsub("\\","/",tempdir(),fixed=TRUE),"fmtdir")
dir.create(fmtdir,showWarnings=FALSE)
repfun::copydata(datdir)
repfun::rs_setup(
D_CENTID="SITEID",
D_DATADATE=Sys.Date(),
D_DSPLYNUM=1,
D_DSPLYTYP='T',
D_FONTSIZE=10,
D_FOOT1='1.) Only treatment emergent events related to lipids are displayed.',
D_FOOT2='2.) Subjects are counted once in each body system & preferred term.',
D_KEEPPOPVARS=c('STUDYID','USUBJID','SAFFL'),
D_USERID=Sys.getenv("USERNAME"),
D_STUDYID='ABCXYZPDQ',
D_POP="SAFFL",
D_POPDATA=repfun::adsl %>% dplyr::filter(SAFFL=='Y'),
D_POPLBL="Safety",
D_SUBJID=c("STUDYID","USUBJID"),
D_TITLE1=paste0('Table 1: Summary of Treatment Emergent Adverse Events'),
D_RTFYN="Y",
D_DEBUG=0,
R_DICTION=NULL,
R_OTHERDATA=NULL,
R_INPUTDATA=NULL,
R_RAWDATA=NULL,
R_SDTMDATA=NULL,
R_ADAMDATA=datdir,
R_RFMTDIR=fmtdir,
R_DDDATA=paste0(outdir,'/t_ru_list_1.rds'),
D_OUTFILE=paste0(outdir,"/t_ru_list_1.rtf"),
D_PGMPTH="/path/to/code/rs_setup.R")
```
