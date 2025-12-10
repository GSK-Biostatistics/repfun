# R function to mimic the SAS macro %tu_list.

Pass in a dataframe and reporting settings to have RTF output generated.

## Usage

``` r
ru_list(
  dsetin,
  stackvar1 = NULL,
  stackvar2 = NULL,
  stackvar3 = NULL,
  stackvar4 = NULL,
  stackvar5 = NULL,
  stackvar6 = NULL,
  stackvar7 = NULL,
  stackvar8 = NULL,
  stackvar9 = NULL,
  stackvar10 = NULL,
  stackvar11 = NULL,
  stackvar12 = NULL,
  stackvar13 = NULL,
  stackvar14 = NULL,
  stackvar15 = NULL,
  display = "Y",
  varlabelstyle = "NOT IMPLEMENTED",
  dddatasetlabel = NULL,
  splitchar = "\n",
  getdatayn = "N",
  labelvarsyn = NULL,
  computebeforepagelines = NULL,
  computebeforepagevars = NULL,
  columns = NULL,
  ordervars = NULL,
  descending = NULL,
  orderformatted = "NOT IMPLEMENTED",
  orderfreq = "NOT IMPLEMENTED",
  orderdata = NULL,
  noprintvars = NULL,
  byvars = NULL,
  flowvars = "NOT IMPLEMENTED",
  widths = NULL,
  defaultwidths = "NOT IMPLEMENTED",
  skipvars = NULL,
  pagevars = NULL,
  idvars = NULL,
  linevars = NULL,
  centrevars = NULL,
  leftvars = NULL,
  rightvars = NULL,
  colspacing = 2,
  varspacing = "NOT IMPLEMENTED",
  formats = "NOT IMPLEMENTED",
  labels = NULL,
  break1 = "NOT IMPLEMENTED",
  break2 = "NOT IMPLEMENTED",
  break3 = "NOT IMPLEMENTED",
  break4 = "NOT IMPLEMENTED",
  break5 = "NOT IMPLEMENTED",
  nowidowvar = NULL,
  sharecolvars = NULL,
  sharecolvarsindent = 2,
  overallsummary = "n",
  proptions = "HEADLINE",
  denormyn = "N",
  varsToDenorm = NULL,
  groupByVars = NULL,
  acrossVar = NULL,
  acrossVarLabel = NULL,
  acrossColVarPrefix = NULL,
  acrossVarListName = NULL,
  lpp = 24,
  rpp = 50,
  toprow = "single",
  spanlbls = NULL,
  spanwidths = NULL,
  spanjust = NULL,
  spanbbord = NULL,
  spantbord = NULL,
  span2lbls = NULL,
  span2widths = NULL,
  span2just = NULL,
  span2bbord = NULL,
  xptyn = "N"
)
```

## Arguments

- dsetin:

  Incoming data frame or list of data frames.

- stackvar1:

  Create Stacked variables (e.g.
  stackvar1=list(varsin=c('invid','subjid'), varout='st_inv_subj',
  sepc='/', splitc='\n'))

- stackvar2:

  Create Stacked variables (e.g.
  stackvar2=list(varsin=c('invid','subjid'), varout='st_inv_subj',
  sepc='/', splitc='\n'))

- stackvar3:

  Create Stacked variables (e.g.
  stackvar3=list(varsin=c('invid','subjid'), varout='st_inv_subj',
  sepc='/', splitc='\n'))

- stackvar4:

  Create Stacked variables (e.g.
  stackvar4=list(varsin=c('invid','subjid'), varout='st_inv_subj',
  sepc='/', splitc='\n'))

- stackvar5:

  Create Stacked variables (e.g.
  stackvar5=list(varsin=c('invid','subjid'), varout='st_inv_subj',
  sepc='/', splitc='\n'))

- stackvar6:

  Create Stacked variables (e.g.
  stackvar6=list(varsin=c('invid','subjid'), varout='st_inv_subj',
  sepc='/', splitc='\n'))

- stackvar7:

  Create Stacked variables (e.g.
  stackvar7=list(varsin=c('invid','subjid'), varout='st_inv_subj',
  sepc='/', splitc='\n'))

- stackvar8:

  Create Stacked variables (e.g.
  stackvar8=list(varsin=c('invid','subjid'), varout='st_inv_subj',
  sepc='/', splitc='\n'))

- stackvar9:

  Create Stacked variables (e.g.
  stackvar9=list(varsin=c('invid','subjid'), varout='st_inv_subj',
  sepc='/', splitc='\n'))

- stackvar10:

  Create Stacked variables (e.g.
  stackvar10=list(varsin=c('invid','subjid'), varout='st_inv_subj',
  sepc='/', splitc='\n'))

- stackvar11:

  Create Stacked variables (e.g.
  stackvar11=list(varsin=c('invid','subjid'), varout='st_inv_subj',
  sepc='/', splitc='\n'))

- stackvar12:

  Create Stacked variables (e.g.
  stackvar12=list(varsin=c('invid','subjid'), varout='st_inv_subj',
  sepc='/', splitc='\n'))

- stackvar13:

  Create Stacked variables (e.g.
  stackvar13=list(varsin=c('invid','subjid'), varout='st_inv_subj',
  sepc='/', splitc='\n'))

- stackvar14:

  Create Stacked variables (e.g.
  stackvar14=list(varsin=c('invid','subjid'), varout='st_inv_subj',
  sepc='/', splitc='\n'))

- stackvar15:

  Create Stacked variables (e.g.
  stackvar15=list(varsin=c('invid','subjid'), varout='st_inv_subj',
  sepc='/', splitc='\n'))

- display:

  Specifies whether the report should be created

- varlabelstyle:

  Specifies the label style for variables (SHORT or STD)

- dddatasetlabel:

  Label to be applied to the DD dataset

- splitchar:

  Split character

- getdatayn:

  Control execution of tu_getdata

- labelvarsyn:

  Control execution of tu_labelvars

- computebeforepagelines:

  Specifies the text to be produced for the Compute Before Page lines
  (labelkey labelfmt colon labelvar)

- computebeforepagevars:

  Names of variables that shall define the sort order for Compute Before
  Page lines

- columns:

  Column parameter

- ordervars:

  Order variables

- descending:

  Descending ORDERVARS

- orderformatted:

  ORDER=FORMATTED variables

- orderfreq:

  ORDER=FREQ variables

- orderdata:

  ORDER=DATA variables

- noprintvars:

  No print vars (usually used to order the display)

- byvars:

  By variables

- flowvars:

  Variables with flow option

- widths:

  Column widths

- defaultwidths:

  List of default column widths

- skipvars:

  similar to SAS statement skipvars Break after ` / skip`

- pagevars:

  similar to SAS statement pagevars Break after ` / page`

- idvars:

  ID variables

- linevars:

  Order variable printed with line statements.

- centrevars:

  Centre justify variables

- leftvars:

  Left justify variables

- rightvars:

  Right justify variables

- colspacing:

  Overall spacing value.

- varspacing:

  Spacing for individual variables.

- formats:

  Format specification

- labels:

  Label definitions.

- break1:

  Break statements.

- break2:

  Break statements.

- break3:

  Break statements.

- break4:

  Break statements.

- break5:

  Break statements.

- nowidowvar:

  Not in version 1

- sharecolvars:

  Order variables that share print space.

- sharecolvarsindent:

  Indentation factor

- overallsummary:

  Overall summary line at top of tables

- proptions:

  PROC REPORT statement options

- denormyn:

  Controls whether denormalisation will occur

- varsToDenorm:

  List of variables to be denormalised/transposed. Passed one at a time
  to the PROC TRANSPOSE VAR statement.

- groupByVars:

  List of BY variables passed to PROC TRANSPOSE BY statement.

- acrossVar:

  Variable used in the PROC TRANSPOSE ID statement.

- acrossVarLabel:

  Variable used in the PROC TRANSPOSE IDLABEL statement.

- acrossColVarPrefix:

  Text passed to the PROC TRANSPOSE PREFIX statement.

- acrossVarListName:

  Macro variable name to contain the list of columns created by the
  transpose of the first variable in VARSTODENORM.

- lpp:

  Lines within body of report (only used with manual paging).

- rpp:

  Total lines per page, when there is no wrapping and excluding titles
  and footnotes - passed directly to r2rtf().

- toprow:

  Control lines above first column header.

- spanlbls:

  List of level 1 column spanning header labels.

- spanwidths:

  List of level 1 column spanning header widths.

- spanjust:

  List of level 1 column spanning header justifications.

- spanbbord:

  List of level 1 column spanning bottom border values.

- spantbord:

  List of level 1 column spanning top border values.

- span2lbls:

  List of level 2 column spanning header labels (above Level 1).

- span2widths:

  List of level 2 column spanning header widths (above Level 1).

- span2just:

  List of level 2 column spanning header justifications (above Level 1).

- span2bbord:

  List of level 2 column spanning bottom border values (above Level 1).

- xptyn:

  Write DDDATA data frame as XPT file in addition to RDS?

## Value

'NULL' because a formatted RTF report is generated to the specified
file.

## Author

Chris Rook, <cr883296@gmail.com>  
Yongwei Wang, <yongwei.x.wang@viivhealthcare.com>

## Examples

``` r
library(repfun)
library(dplyr)
library(tibble)
datdir <- file.path(gsub("\\","/",tempdir(),fixed=TRUE),"datdir")
dir.create(datdir,showWarnings=FALSE)
repfun::copydata(datdir)
outdir <- file.path(gsub("\\","/",tempdir(),fixed=TRUE),"outdir")
dir.create(outdir,showWarnings=FALSE)
#===================================
# Set up the reporting environment.
#===================================
setup <- function(tlfid){
  repfun::rs_setup(
    D_DATADATE=Sys.Date(),
    D_DSPLYNUM=tlfid,
    D_FOOT1='1.) Only treatment emergent events related to lipids are displayed.',
    D_FOOT2='2.) Subjects counted once in each body system & preferred term.',
    D_KEEPPOPVARS=c('STUDYID','USUBJID','SAFFL'),
    D_STUDYID='ABCXYZPDQ',
    D_POP="SAFFL",
    D_POPDATA=repfun::adsl %>%
      dplyr::filter(SAFFL =='Y') %>%
      dplyr::mutate(TRT01AN=ifelse(TRT01A=='Placebo',1,
                            ifelse(TRT01A=='Xanomeline Low Dose',2,3))) %>%
      repfun::ru_labels(varlabels=
                list('TRT01AN'='Actual Treatment for Period 01 (n)')),
    D_POPLBL="Safety",
    D_SUBJID=c("STUDYID","USUBJID"),
    D_TITLE1=paste0('Table ',tlfid,': Summary of Treatment Emergent Adverse Events'),
    D_OUTFILE=paste0(outdir,"/t_ru_list_",tlfid,".rtf"),
    D_PGMPTH="/path/to/code/ru_list.R",
    R_DDDATA=paste0(outdir,'/t_ru_list_',tlfid,'.rds'),
    R_ADAMDATA=datdir)
}

#============================================
# Process ADAE - derive counts and percents.
#============================================
setup(1)
aesum <- repfun::ru_freq(repfun:::rfenv$adamdata$adae.rda() %>% dplyr::select(-SAFFL) %>%
                 repfun::ru_getdata(repfun:::rfenv$G_POPDATA, c("STUDYID", "USUBJID"),
                 keeppopvars=c("TRT01AN", "TRT01A")),
                 dsetindenom=repfun:::rfenv$G_POPDATA,
                 countdistinctvars=c('STUDYID','USUBJID'),
                 groupbyvarsnumer=c('TRT01AN','TRT01A','AEBODSYS','AEDECOD'),
                 anyeventvars = c('AEBODSYS','AEDECOD'),
                 anyeventvalues = c('ANY EVENT','ANY EVENT'),
                 groupbyvarsdenom=c('TRT01AN'),
                 resultstyle="NUMERPCT",
                 totalforvar=c('TRT01AN'),
                 totalid=99,
                 totaldecode='Total',
                 codedecodevarpairs=c("TRT01AN", "TRT01A"),
                 varcodelistpairs=c(""),
                 codelistnames=list(),
                 resultpctdps=0) %>%
dplyr::arrange(TRT01AN,TRT01A,AEBODSYS,tt_summarylevel,AEDECOD,NUMERCNT,DENOMCNT) %>%
repfun::ru_align("tt_result")

#==========================================================================
# Table 2:  Summary of Adverse Events using NOWIDOWVAR (remove SOCs that
# will not fit on 1 page with 10pt font)
#==========================================================================
setup(2)
SOCterms <- aesum %>% dplyr::distinct(AEBODSYS,AEDECOD)
SOCcnts <- table(SOCterms$AEBODSYS)
repfun::ru_list(aesum %>% dplyr::filter(!(AEBODSYS %in% names(SOCcnts[SOCcnts>=20]))),
        columns=c('AEBODSYS','AEDECOD','tt_01','tt_02','tt_03','tt_99'),
        nowidowvar='AEBODSYS',
        widths=c(5.5,4.5,1.75,1.9,1.9,1.75),
        skipvars=c('AEBODSYS'),
        centrevars=c('tt_01','tt_02','tt_03','tt_99'),
        ordervars=c('AEBODSYS','tt_summarylevel','AEDECOD'),
        noprintvars=c('tt_summarylevel'),
        denormyn='Y',
        varsToDenorm=c('tt_result'),
        groupByVars=c('AEBODSYS','tt_summarylevel','AEDECOD'),
        acrossVar="TRT01AN",
        acrossVarLabel="TRT01A",
        acrossColVarPrefix='tt_',
        dddatasetlabel=paste0('DD Dataframe for AE Table ',repfun:::rfenv$G_DSPLYNUM),
        lpp=24)
```
