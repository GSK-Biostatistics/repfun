#' R function to mimic the SAS macro %tu_list.
#'
#' Pass in a dataframe and reporting settings to have RTF output generated.
#'
#' @param dsetin Incoming data frame or list of data frames.
#' @param stackvar1 Create Stacked variables (e.g. stackvar1=list(varsin=c('invid','subjid'), varout='st_inv_subj', sepc='/', splitc='\\n'))
#' @param stackvar2 Create Stacked variables (e.g. stackvar2=list(varsin=c('invid','subjid'), varout='st_inv_subj', sepc='/', splitc='\\n'))
#' @param stackvar3 Create Stacked variables (e.g. stackvar3=list(varsin=c('invid','subjid'), varout='st_inv_subj', sepc='/', splitc='\\n'))
#' @param stackvar4 Create Stacked variables (e.g. stackvar4=list(varsin=c('invid','subjid'), varout='st_inv_subj', sepc='/', splitc='\\n'))
#' @param stackvar5 Create Stacked variables (e.g. stackvar5=list(varsin=c('invid','subjid'), varout='st_inv_subj', sepc='/', splitc='\\n'))
#' @param stackvar6 Create Stacked variables (e.g. stackvar6=list(varsin=c('invid','subjid'), varout='st_inv_subj', sepc='/', splitc='\\n'))
#' @param stackvar7 Create Stacked variables (e.g. stackvar7=list(varsin=c('invid','subjid'), varout='st_inv_subj', sepc='/', splitc='\\n'))
#' @param stackvar8 Create Stacked variables (e.g. stackvar8=list(varsin=c('invid','subjid'), varout='st_inv_subj', sepc='/', splitc='\\n'))
#' @param stackvar9 Create Stacked variables (e.g. stackvar9=list(varsin=c('invid','subjid'), varout='st_inv_subj', sepc='/', splitc='\\n'))
#' @param stackvar10 Create Stacked variables (e.g. stackvar10=list(varsin=c('invid','subjid'), varout='st_inv_subj', sepc='/', splitc='\\n'))
#' @param stackvar11 Create Stacked variables (e.g. stackvar11=list(varsin=c('invid','subjid'), varout='st_inv_subj', sepc='/', splitc='\\n'))
#' @param stackvar12 Create Stacked variables (e.g. stackvar12=list(varsin=c('invid','subjid'), varout='st_inv_subj', sepc='/', splitc='\\n'))
#' @param stackvar13 Create Stacked variables (e.g. stackvar13=list(varsin=c('invid','subjid'), varout='st_inv_subj', sepc='/', splitc='\\n'))
#' @param stackvar14 Create Stacked variables (e.g. stackvar14=list(varsin=c('invid','subjid'), varout='st_inv_subj', sepc='/', splitc='\\n'))
#' @param stackvar15 Create Stacked variables (e.g. stackvar15=list(varsin=c('invid','subjid'), varout='st_inv_subj', sepc='/', splitc='\\n'))
#' @param display Specifies whether the report should be created
#' @param varlabelstyle Specifies the label style for variables (SHORT or STD)
#' @param dddatasetlabel Label to be applied to the DD dataset
#' @param splitchar Split character
#' @param getdatayn Control execution of tu_getdata
#' @param labelvarsyn Control execution of tu_labelvars
#' @param computebeforepagelines Specifies the text to be produced for the Compute Before Page lines (labelkey labelfmt colon labelvar)
#' @param computebeforepagevars Names of variables that shall define the sort order for Compute Before Page lines
#' @param columns Column parameter
#' @param ordervars Order variables
#' @param descending Descending ORDERVARS
#' @param orderformatted ORDER=FORMATTED variables
#' @param orderfreq ORDER=FREQ variables
#' @param orderdata ORDER=DATA variables
#' @param noprintvars No print vars (usually used to order the display)
#' @param byvars By variables
#' @param flowvars Variables with flow option
#' @param widths Column widths
#' @param defaultwidths List of default column widths
#' @param skipvars similar to SAS statement skipvars Break after <var> / skip
#' @param pagevars similar to SAS statement pagevars Break after <var> / page
#' @param idvars ID variables
#' @param linevars Order variable printed with line statements.
#' @param centrevars Centre justify variables
#' @param leftvars Left justify variables
#' @param rightvars Right justify variables
#' @param colspacing Overall spacing value.
#' @param varspacing Spacing for individual variables.
#' @param formats Format specification
#' @param labels Label definitions.
#' @param break1 Break statements.
#' @param break2 Break statements.
#' @param break3 Break statements.
#' @param break4 Break statements.
#' @param break5 Break statements.
#' @param nowidowvar Not in version 1
#' @param sharecolvars Order variables that share print space.
#' @param sharecolvarsindent Indentation factor
#' @param overallsummary Overall summary line at top of tables
#' @param proptions PROC REPORT statement options
#' @param denormyn Controls whether denormalisation will occur
#' @param varsToDenorm List of variables to be denormalised/transposed. Passed one at a time to the PROC TRANSPOSE VAR statement.
#' @param groupByVars List of BY variables passed to PROC TRANSPOSE BY statement.
#' @param acrossVar Variable used in the PROC TRANSPOSE ID statement.
#' @param acrossVarLabel Variable used in the PROC TRANSPOSE IDLABEL statement.
#' @param acrossColVarPrefix Text passed to the PROC TRANSPOSE PREFIX statement.
#' @param acrossVarListName Macro variable name to contain the list of columns created by the transpose of the first variable in VARSTODENORM.
#' @param lpp Lines within body of report (only used with manual paging).
#' @param rpp Total lines per page, when there is no wrapping and excluding titles and footnotes - passed directly to r2rtf().
#' @param toprow Control lines above first column header.
#' @param spanlbls List of level 1 column spanning header labels.
#' @param spanwidths List of level 1 column spanning header widths.
#' @param spanjust List of level 1 column spanning header justifications.
#' @param spanbbord List of level 1 column spanning bottom border values.
#' @param spantbord List of level 1 column spanning top border values.
#' @param span2lbls List of level 2 column spanning header labels (above Level 1).
#' @param span2widths List of level 2 column spanning header widths (above Level 1).
#' @param span2just List of level 2 column spanning header justifications (above Level 1).
#' @param span2bbord List of level 2 column spanning bottom border values (above Level 1).
#' @param xptyn Write DDDATA data frame as XPT file in addition to RDS?
#'
#' @return 'NULL' because a formatted RTF report is generated to the specified file.
#'
#' @author Chris Rook, \email{cr883296@gmail.com} \cr
#'         Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com}
#'
#' @examples
#' library(repfun)
#' library(dplyr)
#' library(tibble)
#' datdir <- file.path(gsub("\\","/",tempdir(),fixed=TRUE),"datdir")
#' dir.create(datdir,showWarnings=FALSE)
#' repfun::copydata(datdir)
#' outdir <- file.path(gsub("\\","/",tempdir(),fixed=TRUE),"outdir")
#' dir.create(outdir,showWarnings=FALSE)
#' #===================================
#' # Set up the reporting environment.
#' #===================================
#' setup <- function(tlfid){
#'   repfun::rs_setup(
#'     D_DATADATE=Sys.Date(),
#'     D_DSPLYNUM=tlfid,
#'     D_FOOT1='1.) Only treatment emergent events related to lipids are displayed.',
#'     D_FOOT2='2.) Subjects counted once in each body system & preferred term.',
#'     D_KEEPPOPVARS=c('STUDYID','USUBJID','SAFFL'),
#'     D_STUDYID='ABCXYZPDQ',
#'     D_POP="SAFFL",
#'     D_POPDATA=repfun::adsl %>%
#'       dplyr::filter(SAFFL =='Y') %>%
#'       dplyr::mutate(TRT01AN=ifelse(TRT01A=='Placebo',1,
#'                             ifelse(TRT01A=='Xanomeline Low Dose',2,3))) %>%
#'       repfun::ru_labels(varlabels=
#'                 list('TRT01AN'='Actual Treatment for Period 01 (n)')),
#'     D_POPLBL="Safety",
#'     D_SUBJID=c("STUDYID","USUBJID"),
#'     D_TITLE1=paste0('Table ',tlfid,': Summary of Treatment Emergent Adverse Events'),
#'     D_OUTFILE=paste0(outdir,"/t_ru_list_",tlfid,".rtf"),
#'     D_PGMPTH="/path/to/code/ru_list.R",
#'     R_DDDATA=paste0(outdir,'/t_ru_list_',tlfid,'.rds'),
#'     R_ADAMDATA=datdir)
#' }
#'
#' #============================================
#' # Process ADAE - derive counts and percents.
#' #============================================
#' setup(1)
#' aesum <- repfun::ru_freq(repfun:::rfenv$adamdata$adae.rda() %>% dplyr::select(-SAFFL) %>%
#'                  repfun::ru_getdata(repfun:::rfenv$G_POPDATA, c("STUDYID", "USUBJID"),
#'                  keeppopvars=c("TRT01AN", "TRT01A")),
#'                  dsetindenom=repfun:::rfenv$G_POPDATA,
#'                  countdistinctvars=c('STUDYID','USUBJID'),
#'                  groupbyvarsnumer=c('TRT01AN','TRT01A','AEBODSYS','AEDECOD'),
#'                  anyeventvars = c('AEBODSYS','AEDECOD'),
#'                  anyeventvalues = c('ANY EVENT','ANY EVENT'),
#'                  groupbyvarsdenom=c('TRT01AN'),
#'                  resultstyle="NUMERPCT",
#'                  totalforvar=c('TRT01AN'),
#'                  totalid=99,
#'                  totaldecode='Total',
#'                  codedecodevarpairs=c("TRT01AN", "TRT01A"),
#'                  varcodelistpairs=c(""),
#'                  codelistnames=list(),
#'                  resultpctdps=0) %>%
#' dplyr::arrange(TRT01AN,TRT01A,AEBODSYS,tt_summarylevel,AEDECOD,NUMERCNT,DENOMCNT) %>%
#' repfun::ru_align("tt_result")
#'
#' #==========================================================================
#' # Table 2:  Summary of Adverse Events using NOWIDOWVAR (remove SOCs that
#' # will not fit on 1 page with 10pt font)
#' #==========================================================================
#' setup(2)
#' SOCterms <- aesum %>% dplyr::distinct(AEBODSYS,AEDECOD)
#' SOCcnts <- table(SOCterms$AEBODSYS)
#' repfun::ru_list(aesum %>% dplyr::filter(!(AEBODSYS %in% names(SOCcnts[SOCcnts>=20]))),
#'         columns=c('AEBODSYS','AEDECOD','tt_01','tt_02','tt_03','tt_99'),
#'         nowidowvar='AEBODSYS',
#'         widths=c(5.5,4.5,1.75,1.9,1.9,1.75),
#'         skipvars=c('AEBODSYS'),
#'         centrevars=c('tt_01','tt_02','tt_03','tt_99'),
#'         ordervars=c('AEBODSYS','tt_summarylevel','AEDECOD'),
#'         noprintvars=c('tt_summarylevel'),
#'         denormyn='Y',
#'         varsToDenorm=c('tt_result'),
#'         groupByVars=c('AEBODSYS','tt_summarylevel','AEDECOD'),
#'         acrossVar="TRT01AN",
#'         acrossVarLabel="TRT01A",
#'         acrossColVarPrefix='tt_',
#'         dddatasetlabel=paste0('DD Dataframe for AE Table ',repfun:::rfenv$G_DSPLYNUM),
#'         lpp=24)
#'
#' @importFrom Hmisc label
#' @import dplyr r2rtf haven stringr
#' @export
#'
ru_list <- function(dsetin,                               ## Input domain dataset ##

                    ##========================================================##
                    ## Need to implement below but it is just a simple paste. ##
                    ##========================================================##
                    stackvar1=NULL,                       ## Create Stacked variables (e.g. stackvar1=list(varsin=c('invid','subjid'), varout='st_inv_subj', sepc='/', splitc='\n')) ##
                    stackvar2=NULL,                       ## Create Stacked variables (e.g. stackvar2=list(varsin=c('invid','subjid'), varout='st_inv_subj', sepc='/', splitc='\n')) ##
                    stackvar3=NULL,                       ## Create Stacked variables (e.g. stackvar3=list(varsin=c('invid','subjid'), varout='st_inv_subj', sepc='/', splitc='\n')) ##
                    stackvar4=NULL,                       ## Create Stacked variables (e.g. stackvar4=list(varsin=c('invid','subjid'), varout='st_inv_subj', sepc='/', splitc='\n')) ##
                    stackvar5=NULL,                       ## Create Stacked variables (e.g. stackvar5=list(varsin=c('invid','subjid'), varout='st_inv_subj', sepc='/', splitc='\n')) ##
                    stackvar6=NULL,                       ## Create Stacked variables (e.g. stackvar6=list(varsin=c('invid','subjid'), varout='st_inv_subj', sepc='/', splitc='\n')) ##
                    stackvar7=NULL,                       ## Create Stacked variables (e.g. stackvar7=list(varsin=c('invid','subjid'), varout='st_inv_subj', sepc='/', splitc='\n')) ##
                    stackvar8=NULL,                       ## Create Stacked variables (e.g. stackvar8=list(varsin=c('invid','subjid'), varout='st_inv_subj', sepc='/', splitc='\n')) ##
                    stackvar9=NULL,                       ## Create Stacked variables (e.g. stackvar9=list(varsin=c('invid','subjid'), varout='st_inv_subj', sepc='/', splitc='\n')) ##
                    stackvar10=NULL,                      ## Create Stacked variables (e.g. stackvar10=list(varsin=c('invid','subjid'), varout='st_inv_subj', sepc='/', splitc='\n')) ##
                    stackvar11=NULL,                      ## Create Stacked variables (e.g. stackvar11=list(varsin=c('invid','subjid'), varout='st_inv_subj', sepc='/', splitc='\n')) ##
                    stackvar12=NULL,                      ## Create Stacked variables (e.g. stackvar12=list(varsin=c('invid','subjid'), varout='st_inv_subj', sepc='/', splitc='\n')) ##
                    stackvar13=NULL,                      ## Create Stacked variables (e.g. stackvar13=list(varsin=c('invid','subjid'), varout='st_inv_subj', sepc='/', splitc='\n')) ##
                    stackvar14=NULL,                      ## Create Stacked variables (e.g. stackvar14=list(varsin=c('invid','subjid'), varout='st_inv_subj', sepc='/', splitc='\n')) ##
                    stackvar15=NULL,                      ## Create Stacked variables (e.g. stackvar15=list(varsin=c('invid','subjid'), varout='st_inv_subj', sepc='/', splitc='\n')) ##

                    display='Y',                          ## Specifies whether the report should be created ##
                    varlabelstyle='NOT IMPLEMENTED',      ## Specifies the label style for variables (SHORT or STD) ##
                    dddatasetlabel=NULL,                  ## Label to be applied to the DD dataset ##
                    splitchar='\n',                       ## Split character ##
                    getdatayn='N',                        ## Control execution of tu_getdata ##
                    labelvarsyn=NULL,                     ## Control execution of tu_labelvars ##
                    computebeforepagelines=NULL,          ## Specifies the text to be produced for the Compute Before Page lines (labelkey labelfmt colon labelvar) ##
                    computebeforepagevars=NULL,           ## Names of variables that shall define the sort order for Compute Before Page lines ##
                    columns=NULL,                         ## Column parameter ##
                    ordervars=NULL,                       ## Order variables ##
                    descending=NULL,                      ## Descending ORDERVARS ##
                    orderformatted='NOT IMPLEMENTED',     ## ORDER=FORMATTED variables ##
                    orderfreq='NOT IMPLEMENTED',          ## ORDER=FREQ variables ##
                    orderdata=NULL,                       ## ORDER=DATA variables ##
                    noprintvars=NULL,                     ## No print vars (usually used to order the display) ##
                    byvars=NULL,                          ## By variables ##
                    flowvars='NOT IMPLEMENTED',           ## Variables with flow option ##
                    widths=NULL,                          ## Column widths ##
                    defaultwidths='NOT IMPLEMENTED',      ## List of default column widths ##

                    skipvars=NULL,                        ## Break after <var> / skip ##
                    pagevars=NULL,                        ## Break after <var> / page ##
                    idvars=NULL,                          ## ID variables ##
                    linevars=NULL,                        ## Order variable printed with line statements. ##
                    centrevars=NULL,                      ## Centre justify variables ##
                    leftvars=NULL,                        ## Left justify variables ##
                    rightvars=NULL,                       ## Right justify variables ##
                    colspacing=2,                         ## Overall spacing value. ##
                    varspacing='NOT IMPLEMENTED',         ## Spacing for individual variables. ##

                    formats='NOT IMPLEMENTED',            ## Format specification ##
                    labels=NULL,                          ## Label definitions. ##

                    ##=========================================================##
                    ## Use the 2 parameters above instead: skipvars, pagevars. ##
                    ##=========================================================##
                    break1='NOT IMPLEMENTED',             ## Break statements. ##
                    break2='NOT IMPLEMENTED',             ## Break statements. ##
                    break3='NOT IMPLEMENTED',             ## Break statements. ##
                    break4='NOT IMPLEMENTED',             ## Break statements. ##
                    break5='NOT IMPLEMENTED',             ## Break statements. ##

                    ##====================================================================##
                    ## Already implemented in the stackvars function(s), reuse that code. ##
                    ##====================================================================##
                    nowidowvar=NULL,                      ## Not in version 1 ##
                    sharecolvars=NULL,                    ## Order variables that share print space. ##
                    sharecolvarsindent=2,                 ## Indentation factor ##

                    ##=====================##
                    ## Come back to these. ##
                    ##=====================##
                    overallsummary='n',                   ## Overall summary line at top of tables ##
                    proptions='HEADLINE',                 ## PROC REPORT statement options ##

                    ##====================================================================##
                    ## Parameters below will be used when transposing incoming dataframe. ##
                    ##====================================================================##
                    denormyn='N',                         ## Controls whether denormalisation will occur ##
                    varsToDenorm=NULL,                    ## List of variables to be denormalised/transposed. Passed one at a time to the PROC TRANSPOSE VAR statement. ##
                    groupByVars=NULL,                     ## List of BY variables passed to PROC TRANSPOSE BY statement. ##
                    acrossVar=NULL,                       ## Variable used in the PROC TRANSPOSE ID statement. ##
                    acrossVarLabel=NULL,                  ## Variable used in the PROC TRANSPOSE IDLABEL statement. ##
                    acrossColVarPrefix=NULL,              ## Text passed to the PROC TRANSPOSE PREFIX statement. ##
                    acrossVarListName=NULL,               ## Macro variable name to contain the list of columns created by the transpose of the first variable in VARSTODENORM. ##

                    ##=================##
                    ## New parameters. ##
                    ##=================##
                    lpp = 24,                             ## Lines within body of report. ##
                    rpp = 50,                             ## Total lines per page, when there is no wrapping. ##

                    toprow = 'single',                    ## Default line above column headers.

                    spanlbls = NULL,                      ## Level 1 spanning column headers.
                    spanwidths = NULL,                    ## Level 1 spanning widths.
                    spanjust = NULL,                      ## Level 1 spanning justifications.
                    spanbbord = NULL,                     ## Level 1 spanning bottom borders.
                    spantbord = NULL,                     ## Level 1 spanning top borders.

                    span2lbls = NULL,                     ## Level 2 spanning column headers (above Level 1).
                    span2widths = NULL,                   ## Level 2 spanning widths (above Level 1).
                    span2just = NULL,                     ## Level 2 spanning justifications (above Level 1).
                    span2bbord = NULL,                    ## Level 2 spanning bottom borders (above Level 1).

                    xptyn = 'N'                           ## Set to 'Y' to have DDDATA data set written to XPT file also.
){

  #=============================================
  # Read parameter settings from rs_setup call.
  #=============================================
  globfile <- paste0(gsub("\\","/",tempdir(),fixed=TRUE),'/GLOBALS.txt')
  if (file.exists(globfile)){source(globfile,local=TRUE)}

  if (G_DEBUG>0) print(paste0("RU_LIST: ", "Start of RU_LIST"))

  ##=====================
  ## Parameter checking.
  ##=====================
  if (is.null(dsetin)){return('RTERROR:  Parameter dsetin is d.  It can be a dataframe or a list of dataframes where the names will be the last title.')}
  if (!(defaultwidths=='NOT IMPLEMENTED')){return('RTWARNING:  Parameter defaultwidths is not implemented.  Use widths instead.')}
  if (!(break1=='NOT IMPLEMENTED')){return('RTWARNING:  Parameter break1 is not implemented.  Use skipvars and pagevars instead.')}
  if (!(break2=='NOT IMPLEMENTED')){return('RTWARNING:  Parameter break2 is not implemented.  Use skipvars and pagevars instead.')}
  if (!(break3=='NOT IMPLEMENTED')){return('RTWARNING:  Parameter break3 is not implemented.  Use skipvars and pagevars instead.')}
  if (!(break4=='NOT IMPLEMENTED')){return('RTWARNING:  Parameter break4 is not implemented.  Use skipvars and pagevars instead.')}
  if (!(break5=='NOT IMPLEMENTED')){return('RTWARNING:  Parameter break5 is not implemented.  Use skipvars and pagevars instead.')}

  assign('G_OUTFILE',gsub('\\.RTF','\\.rtf',rfenv$G_OUTFILE,ignore.case=TRUE),envir=rfenv)

  ##==================================
  ## Update default for special case.
  ##==================================
  if (rfenv$G_JUSTIFICATION=="landscape"){
    if (rpp==50){
      if (is.null(nowidowvar)){
        if (!is.null(sharecolvars)){
          rpp <- 37
        } else {
          rpp <- 34
        }
      }
    }
  }

  ##========================================================================##
  ## Determine the number of titles and footnotes, and put them into lists. ##
  ##========================================================================##
  titles <- c(); footnotes <- c();
  ntitles <- 0; nftnotes <- 0;
  for (i in 1:7){if (!is.null(eval(parse(text = paste0("rfenv$G_TITLE",i))))){ntitles <- ntitles+1; titles <- c(titles,eval(parse(text = paste0("rfenv$G_TITLE",i))))}}
  for (i in 1:9){if (!is.null(eval(parse(text = paste0("rfenv$G_FOOT",i))))){nftnotes <- nftnotes+1; footnotes <- c(footnotes,eval(parse(text = paste0("rfenv$G_FOOT",i))))}}

  ##=================================
  ## Some arguments must be vectors.
  ##=================================
  if (!is.null(pagevars)){
    if (!is.vector(pagevars)){pagevars <- c(pagevars)}
    if (rfenv$G_DEBUG>0) {print('RTNOTE:  Parameter pagevars converted to a vector.')}
  }
  if (!is.null(byvars)){
    if (!is.vector(byvars)){byvars <- c(byvars)}
    if (rfenv$G_DEBUG>0) {print('RTNOTE:  Parameter byvars converted to a vector.')}
  }

  ##===============================================================================================================================================================
  ## Two parameters are implemented the same way: byvars & pagevars.  They should not be used together.  Code below implements pagevars.  Set pagevars to byvars.
  ##===============================================================================================================================================================
  if (!is.null(byvars) & is.null(pagevars)){
    pagevars <- byvars
  } else if (!is.null(byvars) && !is.null(pagevars)){
    if (rfenv$G_DEBUG>0) {print('RTNOTE:  Both byvars and pagevars have been specified.  They will be combined (pagevars first).  Use computebeforepagelines to label both.')
      print(paste0('The following byvars have been added to pagevars: ',setdiff(byvars,pagevars)))}
    pagevars <- c(pagevars, setdiff(byvars,pagevars))
  }

  ##============================================================================================================================================
  ## If a list of dataframes is passed into dsetin then recursively invoke this function on each element using all the same parameter settings.
  ##============================================================================================================================================
  if (is.list(dsetin) && !ggplot2::is_ggplot(dsetin) && !is.data.frame(dsetin)){

    if (rfenv$G_DEBUG>0) {print('RTNOTE:  Parameter dsetin contains a list of dataframes and will be processed as a page-by-variable with list names as the final title.')}

    ##================================================================
    ## Save original outfile, dddata locations and number of titles.
    ##================================================================
    OG_OUTFILE <- rfenv$G_OUTFILE
    OG_DDDATA <- rfenv$G_DDDATA
    ontitles <- ntitles

    ##================================================================================================================================
    ## Create 2 lists:  1.) containing the by-page outputs (filenames), and 2.) containing the by-page dddata data sets (filenames).
    ##================================================================================================================================
    allpages <- c()
    alldddata <- c()
    allttls <- c()

    ##===========================================================================================================
    ## Iterate over the pages dataframes.  Produce an output and dddata data set for each.  Then combine after.
    ##===========================================================================================================
    for (dfx in 1:length(dsetin)){

      if (rfenv$G_DEBUG>0) {print(paste0('RTNOTE:  Processing pages dataframe # ',dfx))}

      ##==============================================================================================================
      ## For this page-by value, add new title, update both tlf filename and dddata data set to include page number.
      ##==============================================================================================================
      assign(paste0('G_TITLE',ontitles+1),names(dsetin)[[dfx]],envir=rfenv)
      assign('G_OUTFILE',gsub('.rtf',paste0('_page_',dfx,'.rtf'),OG_OUTFILE,fixed=TRUE),envir=rfenv)
      assign('G_DDDATA',gsub('.rds',paste0('_page_',dfx,'.rds'),OG_DDDATA,fixed=TRUE),envir=rfenv)

      ##=======================================================
      ## Update the 2 lists for outputs and dddata data sets.
      ##=======================================================
      allpages <- c(allpages,rfenv$G_OUTFILE)
      alldddata <- c(alldddata,rfenv$G_DDDATA)
      allttls <- c(allttls,names(dsetin)[[dfx]])

      ru_list(
        dsetin=dsetin[[dfx]],
        stackvar1=stackvar1,
        stackvar2=stackvar2,
        stackvar3=stackvar3,
        stackvar4=stackvar4,
        stackvar5=stackvar5,
        stackvar6=stackvar6,
        stackvar7=stackvar7,
        stackvar8=stackvar8,
        stackvar9=stackvar9,
        stackvar10=stackvar10,
        stackvar11=stackvar11,
        stackvar12=stackvar12,
        stackvar13=stackvar13,
        stackvar14=stackvar14,
        stackvar15=stackvar15,
        display=display,
        varlabelstyle=varlabelstyle,
        dddatasetlabel=dddatasetlabel,
        splitchar=splitchar,
        getdatayn=getdatayn,
        labelvarsyn=labelvarsyn,
        computebeforepagelines=computebeforepagelines,
        computebeforepagevars=computebeforepagevars,
        columns=columns,
        ordervars=ordervars,
        descending=descending,
        orderformatted=orderformatted,
        orderfreq=orderfreq,
        orderdata=orderdata,
        noprintvars=noprintvars,
        byvars=byvars,
        flowvars=flowvars,
        widths=widths,
        defaultwidths=defaultwidths,
        skipvars=skipvars,
        pagevars=pagevars,
        idvars=idvars,
        linevars=linevars,
        centrevars=centrevars,
        leftvars=leftvars,
        rightvars=rightvars,
        colspacing=colspacing,
        varspacing=varspacing,
        formats=formats,
        labels=labels,
        break1=break1,
        break2=break2,
        break3=break3,
        break4=break4,
        break5=break5,
        nowidowvar=nowidowvar,
        sharecolvars=sharecolvars,
        sharecolvarsindent=sharecolvarsindent,
        overallsummary=overallsummary,
        proptions=proptions,
        denormyn=denormyn,
        varsToDenorm=varsToDenorm,
        groupByVars=groupByVars,
        acrossVar=acrossVar,
        acrossVarLabel=acrossVarLabel,
        acrossColVarPrefix=acrossColVarPrefix,
        acrossVarListName=acrossVarListName,
        lpp=lpp,
        rpp=rpp)
    }

    ##===================================
    ## Combine individual page files.
    ##===================================
    r2rtf::assemble_rtf(allpages, OG_OUTFILE)

    ##========================================
    ## Remove the individual page RTF files.
    ##========================================
    for (f in 1:length(dsetin)){
      if (is.character(allpages[[f]]) && !(allpages[[f]] == '') && file.exists(allpages[[f]])) {
        file.remove(allpages[[f]])
      }
    }

    ##======================================================================
    ## Combine the DDDATA data sets and remove the page-specific versions.
    ##======================================================================
    dddata <- data.frame()
    for (f in 1:length(dsetin)){
      if (file.exists(alldddata[[f]])){
        tmpds <- readRDS(alldddata[[f]])
        tmpds[['PAGBYCAT']] <- allttls[[f]]
        attr(tmpds[['PAGBYCAT']],'label') <- 'Page-by-Category'
        if (nrow(tmpds)>0){
          if ('Row.names' %in% names(dddata)){
            dddata <- dddata %>% dplyr::select(-Row.names)
          }
          dddata <- ru_setdata(dddata,tmpds)
        }
      }

      #===============================
      # Remove intermediate RDS file.
      #===============================
      if (is.character(alldddata[[f]]) && !(alldddata[[f]] == '') && file.exists(alldddata[[f]])) {
        file.remove(alldddata[[f]])
      }

      #===============================
      # Remove intermediate XPT file.
      #===============================
      if (is.character(gsub('rds','xpt',alldddata[[f]])) && !(gsub('rds','xpt',alldddata[[f]]) == '') && file.exists(gsub('rds','xpt',alldddata[[f]]))) {
        file.remove(gsub('rds','xpt',alldddata[[f]]))
      }
    }

    ##===================================================================
    ## Reset global environment variables to what they were originally.
    ##===================================================================
    assign('G_OUTFILE',OG_OUTFILE,envir=rfenv)
    assign('G_DDDATA',OG_DDDATA,envir=rfenv)
    assign(paste0('G_TITLE',ontitles+1),NULL,envir=rfenv)

    ##=============================================
    ## Write final DD Dataset when there is data.
    ##=============================================
    if (nrow(dddata)>0) {
      if (!is.null(dddatasetlabel)){attr(dddata, "label") <- dddatasetlabel}
      saveRDS(dddata %>% dplyr::select(-Row.names), rfenv$G_DDDATA)
      if (toupper(xptyn)=='Y'){
        haven::write_xpt(readRDS(rfenv$G_DDDATA), gsub('rds','xpt',rfenv$G_DDDATA))
      }
      if (rfenv$G_DEBUG>0) {print(paste0('DD Dataframe written to file: ',rfenv$G_DDDATA))}
    }
    if (rfenv$G_DEBUG>0){
      return('***** Multi-page report processed. *****')
    } else (return(invisible(NULL)))
  }

  #===============================
  # Prep for reporting function.
  #===============================
  if (display == 'Y'){
    if (!("r2rtf" %in% rownames(utils::installed.packages()))) {utils::install.packages("r2rtf")}
  }



  if (rfenv$G_JUSTIFICATION=="landscape"){

  } else if (rfenv$G_JUSTIFICATION=="portrait"){

  }



  ##============================================
  ## Set page sizes and orientations defaults.
  ##============================================
  if (rfenv$G_JUSTIFICATION=="landscape"){
      wdth <- 11
      colwdth <- 9
      spc <- 12900
      ##============================================
      ## Adjust lines per page based on font size.
      ##============================================
      if (rfenv$G_FONTSIZE == 8){
        rpp <- 75
      }
  } else if (rfenv$G_JUSTIFICATION=="portrait"){
      wdth <- 8.5
      colwdth <- 6.5
      spc <- 9350
      #======================
      # Update the default.
      #======================
      if (rpp==50){
        ##============================================
        ## Adjust lines per page based on font size.
        ##============================================
        if (rfenv$G_FONTSIZE == 8){
            rpp <- 175
        } else {
            rpp <- 125
        }
      }
  }

  ##========================
  ## No data in dataframe.
  ##========================
  if (is.data.frame(dsetin) && nrow(dsetin)==0){

    nlns <- round((30-(ntitles+nftnotes))/2)
    tmpdf <- data.frame(ND=paste0(strrep('\\line',nlns),' No data to report ',strrep('\\line',nlns)))

    tmpdf %>%
      r2rtf::rtf_title(title=titles,
                       text_font=rep(9,ntitles),
                       text_font_size=rep(rfenv$G_FONTSIZE,ntitles),
                       text_format=rep('',ntitles),
                       text_space_before=20) %>%
      r2rtf::rtf_page(orientation = rfenv$G_JUSTIFICATION,
                      nrow=(rpp - ntitles - nftnotes),
                      width=wdth,
                      col_width=colwdth,
                      margin=c(1.00,1.00,1.20,1.0,1.15,0.95)) %>%
      r2rtf::rtf_page_header(text=c(paste0("\\pard\\ql{\\f8 Protocol: ", rfenv$G_STUDYID ," \\line Population: ", rfenv$G_POPLBL ,
                                           " \\cell}\\cellx5000 \\pard\\qr{\\f8 Page \\chpgn  of {\\field{\\*\\fldinst NUMPAGES }} \\cell}\\cellx",spc," {\\row} \\fs1{ \\uc1\\u160* }")),
                             text_font_size=rfenv$G_FONTSIZE,
                             text_convert=FALSE) %>%
      r2rtf::rtf_page_footer(text=paste0('User: ', rfenv$G_USERID,' ',rfenv$G_PGMPTH,' ',format(Sys.time(), "%Y-%m-%d %T")),    # '\\i Source:\\i0 '
                             text_font=9,text_font_size=rfenv$G_FONTSIZE,text_justification='l',text_convert=FALSE) %>%
      r2rtf::rtf_colheader(' ',
                           col_rel_width = 1,
                           text_justification = c('c'),
                           border_left = rep('',1),
                           border_right = rep('',1),
                           text_font=9,
                           text_font_size=rfenv$G_FONTSIZE) %>%
      r2rtf::rtf_body(col_rel_width = 1,
                      text_justification = rep('c',1),
                      border_left = rep('',1),
                      border_right = rep('',1),
                      text_font=9,
                      text_font_size=rfenv$G_FONTSIZE) %>%
      {if (!is.null(footnotes)) r2rtf::rtf_footnote(., footnote=footnotes, text_font=9, text_font_size=rfenv$G_FONTSIZE, text_justification='l', as_table=FALSE, text_convert=FALSE) else .} %>%
      r2rtf::rtf_encode(page_footnote = 'all') %>%
      r2rtf::write_rtf(rfenv$G_OUTFILE)

    if (rfenv$G_DEBUG > 0) {
      return('***** No Data to Report *****')
    } else {return(invisible(NULL))}
  }

  ##================================
  ## Deal with plot objects first.
  ##================================
  if (display=='Y' && ggplot2::is_ggplot(dsetin)){

    if (rfenv$G_DEBUG>0) {print('RTNOTE:  A ggplot object is being processed.')}

    #===============================================================
    # Retrieve data from ggplot object and write to DD dataframe.
    #===============================================================
    dddata <- ggplot2::ggplot_build(dsetin)$data[[1]]
    dddata <- data.frame(lapply(dddata, as.character), stringsAsFactors=FALSE)
    if (nrow(dddata)>0) {
      if (!is.null(dddatasetlabel)){attr(dddata, "label") <- dddatasetlabel}
      saveRDS(dddata, rfenv$G_DDDATA)
      if (toupper(xptyn)=='Y'){
        haven::write_xpt(readRDS(rfenv$G_DDDATA), gsub('rds','xpt',rfenv$G_DDDATA))
      }
      if (rfenv$G_DEBUG>0) {print(paste0('DD Dataframe written to file: ',rfenv$G_DDDATA))}
    }

    ##==================================================
    ## Write to png file.  Will be removed at the end.
    ##==================================================
    figfile <- file.path(gsub('.rtf','.png',rfenv$G_OUTFILE,fixed=TRUE))
    grDevices::png(figfile,width=rfenv$G_WIDTH,height=rfenv$G_HEIGHT,units='in',res=100)   # pointsize=rfenv$G_FONTSIZE,
    print(dsetin)
    grDevices::dev.off()

    ##==================================================
    ## Generate RTF file.
    ##==================================================
    r2rtf::rtf_read_figure(figfile) %>%
      r2rtf::rtf_title(title=titles,
                       text_font=rep(9,ntitles),
                       text_font_size=rep(rfenv$G_FONTSIZE,ntitles),
                       text_format=rep('',ntitles),
                       text_space_before=20) %>%
      r2rtf::rtf_page(orientation = rfenv$G_JUSTIFICATION,
                      nrow=(rpp - ntitles - nftnotes),
                      width=wdth,
                      col_width=colwdth,
                      margin=c(1.00,1.00,1.20,1.0,1.15,0.95)) %>%
      r2rtf::rtf_page_header(text=c(paste0("\\pard\\ql{\\f8 Protocol: ", rfenv$G_STUDYID ," \\line Population: ", rfenv$G_POPLBL ,
                                           " \\cell}\\cellx5000 \\pard\\qr{\\f8 Page \\chpgn  of {\\field{\\*\\fldinst NUMPAGES }} \\cell}\\cellx",spc," {\\row} \\fs1{ \\uc1\\u160* }")),
                             text_font_size=rfenv$G_FONTSIZE,
                             text_convert=FALSE) %>%
      r2rtf::rtf_page_footer(text=paste0('User: ', rfenv$G_USERID,' ',rfenv$G_PGMPTH,' ',format(Sys.time(), "%Y-%m-%d %T")),    # '\\i Source:\\i0 '
                             text_font=9,text_font_size=rfenv$G_FONTSIZE,text_justification='l',text_convert=FALSE) %>%
      r2rtf::rtf_figure( fig_width = rfenv$G_WIDTH, fig_height = rfenv$G_HEIGHT, fig_format = NULL) %>%
      {if (!is.null(footnotes)) r2rtf::rtf_footnote(., footnote=footnotes, text_font=9, text_font_size=rfenv$G_FONTSIZE, text_justification='l', as_table=FALSE, text_convert=FALSE) else .} %>%
      r2rtf::rtf_encode(doc_type = "figure") %>%
      r2rtf::write_rtf(file = file.path(rfenv$G_OUTFILE))

    ##=======================
    ## Remove the png file.
    ##=======================
    if (is.character(figfile) && !(figfile == '') && file.exists(figfile)) {
      file.remove(figfile)
    }

    if (rfenv$G_DEBUG>0) {
      return('Exit ru_list(*** Figure ***)')
    } else {return(invisible(NULL))}

  }

  ##==============================================================##
  ## Read in incoming data set as either a string or a dataframe. ##
  ##==============================================================##
  if (is.character(dsetin)){
    list_of_dfs_as_strings <- ls(envir=.GlobalEnv)[grepl('data.frame', sapply(ls(envir=.GlobalEnv), function(x) class(get(x))))]
    if (dsetin %in% list_of_dfs_as_strings){tmpdf <- get(dsetin, envir=.GlobalEnv)}
    else {
      filnam <- paste0(rfenv$G_ADAMDATA,dsetin,'.sas7bdat')
      if (file.exists(filnam)){tmpdf <- haven::read_sas(filnam)}
    }
  } else {tmpdf <- dsetin}

  ##=====================================================================================================================##
  ## Invoke function to set detault widths for variables unspecified.  Use passed in value if all numeric and not named. ##
  ##=====================================================================================================================##
  allnumeric <- NULL
  if (!is.null(widths)){
    allnumeric <- TRUE
    for (i in widths) {
      if (!is.numeric(i)){allnumeric <- FALSE}
    }
    for (n in names(widths)){
      if (!is.numeric(n)){allnumeric <- FALSE}
    }
  }

  ##======================================================================================##
  ## Set the widths value either using passed in value or via function call to estimate.  ##
  ##======================================================================================##
  if (!is.null(allnumeric) && allnumeric){
    widths <- widths
  } else {
    widths <- ru_width_rtf(tmpdf, varsin=columns)
  }

  ##================================================
  ## Number of columns in final reporting data set.
  ##================================================
  if (is.null(columns)){return('RTERROR:  Parameter columns is required.  This is the list of variables that will be displayed on the report.')}
  repcols <- unique(c(columns,noprintvars))
  ncols1 <- length(repcols) - length(noprintvars)     # Without page variable counted.
  ncols2 <- ncols1 + length(pagevars)                 # With page variable counted.
  widths2 <- c(widths,rep(0,each=length(pagevars)))   # Update when page variable is added.

  ##=====================================================
  ## Apply ru_getdata() to the data set when requested.
  ##=====================================================
  if (!is.null(getdatayn) && toupper(getdatayn)=='Y'){
    tmpdf <- ru_getdata(tmpdf,rfenv$G_POPDATA,rfenv$G_SUBJID,subpop=rfenv$G_SUBPOP,pop=rfenv$G_POP,keeppopvars=rfenv$G_KEEPPOPVARS)
  }

  ##=====================================
  ## Denorm the data set when requested.
  ##=====================================
  if (toupper(denormyn)=='Y'){
    if ('DENOMCNT' %in% names(tmpdf)){
      tmpdf %>% dplyr::mutate(TRTLBL=ifelse(!grepl('N=',!!! rlang::syms(acrossVarLabel)),paste0(!!! rlang::syms(acrossVarLabel),'\\line (N=',DENOMCNT,')'),!!! rlang::syms(acrossVarLabel))) %>%
        dplyr::mutate(TRTLBL=ifelse(!grepl('n (',TRTLBL,fixed=TRUE),paste0(TRTLBL,'\\line n (%)'),TRTLBL)) -> tmpdf
      acrossVarLabel <- "TRTLBL"
    }
    tmpdf <- ru_denorm(tmpdf, varstodenorm=varsToDenorm, groupbyvars=groupByVars, acrossvar=acrossVar, acrossvarlabel=acrossVarLabel, acrossvarprefix=acrossColVarPrefix)
  }

  ##=====================================================
  ## Apply user-provided labels to incoming dataframe.
  ##=====================================================
  if (!is.null(labels)){
    tmpdf <- ru_labels(tmpdf,labels)
  }

  ##=========================================================================================
  ## If a variable has no label then set to name of column.  All variables will be labeled.
  ##=========================================================================================
  for (v in names(tmpdf)){
    if (is.null(attr(tmpdf[[v]],'label')) ){
      attr(tmpdf[[v]],'label') <- v
    } else if (is.na(attr(tmpdf[[v]],'label'))){
      attr(tmpdf[[v]],'label') <- v
    }
  }

  ##===========================##
  ## Get all dataframe labels. ##
  ##===========================##
  dflabels <- lapply(tmpdf, attr, "label")

  ##==============================##
  ## Handle stackvars parameters. ##
  ##==============================##
  for (i in 1:15){

    ##=======================================##
    ## Needed quantities for function call.  ##
    ##=======================================##
    stackval <- eval(parse(text = paste0("stackvar",i)))

    if (!is.null(stackval)){

      ##=================#
      ## Function call.  #
      ##=================#
      tmpdf <- ru_stackvar(dsetin       = tmpdf,
                           sepc         = stackval[['sepc']],
                           splitc       = stackval[['splitc']],
                           varsin       = stackval[['varsin']],
                           varout       = stackval[['varout']],
                           varlabel     = stackval[['varlabel']])

      ##===========================##
      ## Get all dataframe labels. ##
      ##===========================##
      dflabels <- lapply(tmpdf, attr, "label")

    }
  }

  ##=============================================================================================##
  ## Stacking is done, keep only needed variables which are in columns or ordervars or pagevars. ##
  ##=============================================================================================##
  keepvars <- unique(c(ordervars, columns, pagevars))
  tmpdf %>% dplyr::select(dplyr::all_of(keepvars)) -> tmpdf

  ##==========================================================================
  ## Order dataframe, when requested and all variables are in the dataframe.
  ##==========================================================================
  ordered <- FALSE
  grpvars <- NULL
  if (!is.null(ordervars) && all(ordervars %in% names(tmpdf)) && is.null(orderdata)){

    if (!is.vector(ordervars)){ordervars <- c(ordervars)}
    if (!is.null(descending)){
      if (!is.null(descending)){descending <- c(descending)}
    }

    vn=1
    ordlist <- c()
    for (v in ordervars){
      newvar <- paste0('_o_r_d_',vn,'_')
      ordlist <- c(ordlist,newvar)
      bothvars <- c(v,newvar)
      tmpdf %>% dplyr::distinct(!!! rlang::syms(v)) %>% dplyr::arrange(!!! rlang::syms(v)) %>% dplyr::mutate(!! newvar:=dplyr::row_number()) %>% dplyr::select(!!! rlang::syms(bothvars)) -> neword
      if (v %in% descending){
        neword %>% dplyr::mutate(!! newvar := -1*(!!! rlang::syms(newvar))) -> neword
      }
      suppressMessages(tmpdf %>% dplyr::left_join(neword) -> tmpdf)
      vn=vn+1
    }

    #=================================================================================================================================================================================
    # Some order variables can be numeric.  These should be noprintvars also.  Drop here as they will cause problems of having NA values on inserted records, such as with skipvars.
    #=================================================================================================================================================================================
    drpvars <- intersect(ordervars,noprintvars)      # Drop variables just used but not needed after.
    drpvars <- setdiff(drpvars,skipvars)             # These will be needed in next step.
    tmpdf %>% dplyr::arrange(!!! rlang::syms(ordlist)) %>% dplyr::select(-dplyr::all_of(ordlist)) %>% dplyr::select(- !! drpvars) -> tmpdf
    ordered=TRUE

  } else if (!is.null(ordervars) && all(ordervars %in% names(tmpdf)) && !is.null(orderdata) && orderdata) {
    grpvars <- ordervars  # ordervars with orderdata is just group by in the output.  No sorting done.
  }

  #======================================================================================
  # Convert numeric columns to character so that NA values can be dealt with as blanks.
  #======================================================================================
  labls <- lapply(tmpdf, attr, "label")
  tmpdf <- tmpdf %>% dplyr::mutate(dplyr::across(where(is.numeric) & !dplyr::all_of(pagevars), as.character))
  tmpdf <- tmpdf %>% ru_labels(varlabels=labls)

  #======================================================================================
  # Check for exact duplicate rows before implementing skipvars.  Put out message here.
  #======================================================================================
  chk4dups <- tmpdf[do.call(order, tmpdf), ]
  drs <- duplicated(chk4dups) | duplicated(chk4dups, fromLast = TRUE)
  if (any(drs)){
    print(paste0('***** W','ARNING: Duplicate rows detected. *****'))
    print(chk4dups[drs,])
  }

  ##=====================##
  ## Implement skipvars. ##
  ##=====================##
  if (!is.null(skipvars)){

    ##==========
    ## Unlabel.
    ##==========
    labls <- lapply(tmpdf, attr, "label")
    for (v in names(tmpdf)){
      attr(tmpdf[[v]],'label') <- NULL
    }

    #=====================================================================
    # If a skipvar is not in columns add it temporarily (removed below).
    #=====================================================================
    tmpcols <- columns
    for (rs in rev(skipvars)){
      tmpcols <- c(setdiff(c(rs),tmpcols),tmpcols)
    }

    #===========================================
    # Iterate over skipvars and insert a line.
    #===========================================
    newcols <- c(pagevars,tmpcols)
    for (s in skipvars){

      if (rfenv$G_DEBUG>0) {
        print('Value of s is:')
        print(s)
      }

      #============================================================================================================
      # Get all variables in columns up to the skip var.  Insert blank row.  For char vars replace NA with blank.
      #============================================================================================================
      inw <- which(!is.na(match(newcols,c(s))))
      skipafter <- newcols[1:inw]

      if (rfenv$G_DEBUG>0) {
        print('Value of skipafter is:')
        print(skipafter)
      }

      #=================================================
      # Group-by below is changing the order, reset it.
      #=================================================
      if (ordered==TRUE){
        tmpdf %>% dplyr::mutate(nn=dplyr::row_number()) %>% dplyr::group_by(!!! rlang::syms(skipafter)) %>% dplyr::filter(dplyr::row_number()==1) %>% dplyr::ungroup() %>% dplyr::select(!!skipafter,nn) -> reorder
      }

      tmpdf %>% dplyr::group_by(!!! rlang::syms(skipafter)) %>% dplyr::group_modify(~ add_row(.x,.after=Inf)) %>% dplyr::ungroup() -> tmpdf
      tmpdf %>% dplyr::mutate_if(is.character, ~replace_na(.,"")) %>% dplyr::distinct() -> tmpdf

      if (ordered==TRUE){
        suppressMessages(tmpdf %>% dplyr::left_join(reorder) %>% dplyr::arrange(nn) %>% dplyr::select(-nn) -> tmpdf)
      }
    }

    ##=========
    ## Relabel.
    ##=========
    for (v in names(tmpdf)){
      if (v %in% names(labls)){Hmisc::label(tmpdf[[v]]) <- labls[[v]]}
    }
  }

  ##==============================================================================================##
  ## Handle nowidowvars and page the incoming dataframe, or just manually page it when requested. ##
  ##==============================================================================================##
  stackvars <- NULL
  if ((!is.null(nowidowvar)) | (length(pagevars)==1 && (pagevars[[1]]=='PAGEVAR'))){

    rpp <- rpp+4

    ##===========================================================
    ## If set, get variables from columns up to the nowidowvar.
    ##===========================================================
    if (!is.null(sharecolvars)){
      inw <- which(!is.na(match(sharecolvars,nowidowvar)))
      stackvars <- sharecolvars[1:inw]
    }

    if (!is.null(columns)){
      inw <- which(!is.na(match(columns,nowidowvar)))
      grpvars <- columns[1:inw]
    }

    if (is.null(stackvars) && is.null(grpvars) && !is.null(ordervars)){
      stackvars <- ordervars
    }

    if (!is.null(stackvars) && !is.null(pagevars)){
      stackvars <- unique(c(pagevars,stackvars))
    }

    if (!is.null(grpvars) && !is.null(pagevars)){
      grpvars <- unique(c(pagevars,grpvars))
    }

    if (!is.null(nowidowvar)){
      nosplits=TRUE
    } else {
      nosplits=FALSE
    }

    ##==================================================================================================================================
    ## If page variables have been passed in, iterate over all unique pages and invoke the nowidow or manual paging function for each.
    ##==================================================================================================================================
    if (!is.null(pagevars)){

      ##================
      ## Unique pages.
      ##================
      tmpdf %>% dplyr::ungroup() %>% dplyr::select(!!! rlang::syms(pagevars)) %>% dplyr::distinct(!!! rlang::syms(pagevars)) -> rfenv$upages

      ##======================================================================
      ## Iterate over each page, subset data and invoke the paging function.
      ##======================================================================
      for (pgvl in 1:nrow(rfenv$upages)){
        rfenv$upages %>% dplyr::filter(dplyr::row_number()==pgvl) -> joinds
        suppressMessages(tmpdf %>% dplyr::inner_join(joinds) -> ntmpdf)

        ##===================================
        ## Max of 1000 iterations then exit.
        ##===================================
        rfenv$PAGING_ITERATIONS <- 0

        ntmpdf <- ru_addpage(ntmpdf,
                             grpvars=grpvars,
                             stackvars=stackvars,
                             varlabls=dflabels,
                             rowsprbdy=lpp-ntitles-nftnotes,
                             lastbygrp=TRUE,
                             fpage='all',
                             nftnotes=nftnotes,
                             nosplitvars=nosplits,
                             npgvars=length(pagevars))

        ##=======================================================================================
        ## Deal with case where options specified cannot work due to available spacing on page.
        ##=======================================================================================
        if (rfenv$PAGING_ITERATIONS>200){
          ntmpdf %>% dplyr::select (-c('catid','catn','widow')) %>% dplyr::mutate(PAGEVAR=pgvl) -> ntmpdf
          attr(ntmpdf[['PAGEVAR']],'label') <- 'Page Variable'
        }

        ##==========================
        ## Accumulate data frames.
        ##==========================
        if (pgvl==1){
          newdf <- ntmpdf
        } else if (pgvl==nrow(rfenv$upages)){
          newdf %>% rbind(ntmpdf) -> tmpdf
          pagevars <- c(pagevars,'PAGEVAR')
        } else {
          newdf %>% rbind(ntmpdf) -> newdf
        }
      }

      if (rfenv$PAGING_ITERATIONS>200){rpp <- rpp - 4}

    } else {

      ##===================================
      ## Max of 1000 iterations then exit.
      ##===================================
      rfenv$PAGING_ITERATIONS <- 0

      tmpdf <- ru_addpage(tmpdf,
                          grpvars=grpvars,
                          stackvars=stackvars,
                          varlabls=dflabels,
                          rowsprbdy=lpp-ntitles-nftnotes,
                          lastbygrp=TRUE,
                          fpage='all',
                          nftnotes=nftnotes,
                          nosplitvars=nosplits)

      pagevars <- c('PAGEVAR')

      ##=======================================================================================
      ## Deal with case where options specified cannot work due to available spacing on page.
      ##=======================================================================================
      if (rfenv$PAGING_ITERATIONS>200){
        tmpdf %>% dplyr::select (-c('catid','catn','widow')) %>% dplyr::mutate(PAGEVAR=1) -> tmpdf
        attr(tmpdf[['PAGEVAR']],'label') <- 'Page Variable'
        rpp <- rpp - 4
      }

      #=====================================================================================================
      # Deal with dangling skipvars containing blank rows (nowidow gets turned off above then it happens).
      #=====================================================================================================
      if (!is.null(skipvars)){
        chk4blank <- setdiff(names(tmpdf),c(skipvars,'PAGEVAR'))
        is_row_blank <- function(row, cols) {
          all(is.na(row[cols]) | row[cols] == "")
        }
        tmpdf$blankrow <- apply(tmpdf, 1, is_row_blank, cols = chk4blank)
        tmpdf %>% dplyr::group_by(!!! rlang::syms(skipvars)) %>%
          dplyr::mutate(rn1=row_number(), rn2=n()) %>% dplyr::ungroup() %>%
          dplyr::group_by(PAGEVAR) %>%
          dplyr::mutate(rn3=row_number()) %>% dplyr::ungroup() %>%
          dplyr::filter(!(blankrow==TRUE & rn1==rn2 & rn3==1)) %>%
          dplyr::select(-c(blankrow,rn1,rn2,rn3)) -> tmpdf
      }
    }

    ##==================================================================
    ## Update counter variables as needed due to paging variable added.
    ##==================================================================
    ncols2 <- ncols2 + 1
    widths2 <- c(widths2,0)

  }

  ##=========================##
  ## Deal with sharecolvars. ##
  ##=========================##
  alllabls <- lapply(tmpdf, attr, "label")
  if (!is.null(sharecolvars)){

    #====================================
    # Number of stackvars and new label.
    #====================================
    nstackvars <- length(sharecolvars)

    #========================
    # Stack variable labels.
    #========================
    stacklabl <- alllabls[sharecolvars[[1]]]
    for (i in 2:nstackvars){
      stacklabl <- paste(stacklabl,alllabls[sharecolvars[[i]]], sep=paste0('\n',paste(rep(' ',(i-1)*2),collapse='')))
    }

    #==============================================================
    # Temporarily add a PAGEVAR if one has not already been added.
    #==============================================================
    pagevaradded=FALSE
    if (is.null(pagevars)){
      tmpdf %>% dplyr::mutate(PAGEVAR=1) -> tmpdf
      pagevars <- c('PAGEVAR')
      pagevaradded <- TRUE
    }

    #=================================================
    # Derive new stacked variable drop old variables.
    #=================================================
    idnt <- strrep(' ',sharecolvarsindent)
    tmpdf %>% dplyr::group_by(!!! rlang::syms(pagevars)) %>% dplyr::mutate(STACK=ifelse(dplyr::row_number()==1,TRUE,FALSE)) %>% dplyr::ungroup() %>%
      dplyr::group_by(!!! rlang::syms(c(pagevars,sharecolvars[[1]]))) %>% dplyr::mutate(STACK=ifelse(dplyr::row_number()==1,TRUE,STACK)) %>% dplyr::ungroup() %>%
      dplyr::mutate(STACKVAR=ifelse(STACK==TRUE,paste(!!! rlang::syms(sharecolvars[[1]]),!!! rlang::syms(sharecolvars[[2]]),sep=paste0('\n',idnt)),
                                    paste0(idnt,!!! rlang::syms(sharecolvars[[2]])))) -> tmpdf

    if (length(sharecolvars)==3){
      tmpdf %>% dplyr::group_by(!!! rlang::syms(c(pagevars,utils::head(sharecolvars,2)))) %>% dplyr::mutate(STACK2=ifelse(dplyr::row_number()==1,TRUE,FALSE)) %>% dplyr::ungroup() %>%
        dplyr::mutate(STACKVAR=ifelse(STACK==FALSE & STACK2==TRUE,paste(paste0(idnt,!!! rlang::syms(sharecolvars[[2]])),!!! rlang::syms(sharecolvars[[3]]),sep=paste0('\n',idnt,idnt)), STACKVAR),
                      STACKVAR=ifelse(STACK==FALSE & STACK2==FALSE,paste0(idnt,idnt,!!! rlang::syms(sharecolvars[[3]])),STACKVAR),
                      STACKVAR=ifelse(STACK==TRUE,paste(STACKVAR,!!! rlang::syms(sharecolvars[[3]]),sep=paste0('\n',idnt,idnt)),STACKVAR)) -> tmpdf
    }

    #=================================================
    # Drop individual sharecolvars, no longer needed.
    #=================================================
    tmpdf %>% dplyr::select(-c(STACK,!!! rlang::syms(sharecolvars))) %>% dplyr::select(STACKVAR, dplyr::everything()) %>% {if(nstackvars==3) dplyr::select(.,-STACK2) else .} -> tmpdf

    #=================================================
    # Remove temporary PAGEVAR if it has been added.
    #=================================================
    if (pagevaradded){
      tmpdf %>% dplyr::select(-PAGEVAR) -> tmpdf
      pagevars <- NULL
    }

    #===========================================================
    # Convert above to separate row(s) for first stacked value.
    #===========================================================
    rcols <- setdiff(names(tmpdf),c('STACKVAR','PAGEVAR',pagevars))
    tmpdf %>% dplyr::mutate(obsnum=dplyr::row_number()) -> tmpdf
    tmpdf %>% dplyr::filter(grepl('\n',STACKVAR,fixed=TRUE)) -> firstval
    firstval %>% dplyr::mutate(nsplits = stringr::str_count(STACKVAR, '\\n')) %>% dplyr::filter(nsplits==2) %>% dplyr::select(-nsplits) -> secondval
    tmpdf %>% rbind(firstval) %>% rbind(secondval) %>% dplyr::arrange(obsnum) %>% dplyr::group_by(obsnum) %>% dplyr::mutate(rnum=dplyr::row_number(),totrows=n()) %>% dplyr::ungroup() -> tmpdf
    tmpdf %>% dplyr::mutate(dplyr::across(dplyr::all_of(rcols), ~replace(.,rnum==1 & totrows>=2,'')),
                            dplyr::across(dplyr::all_of(rcols), ~replace(.,rnum==2 & totrows>=3,''))) -> tmpdf

    tmpdf %>% dplyr::mutate(STACKVAR=ifelse(rnum==1,sub('\\n.*$','',STACKVAR),STACKVAR),
                            STACKVAR=ifelse(rnum>=2,sub('^.*?\\n','',STACKVAR),STACKVAR),
                            STACKVAR=ifelse(rnum>=3,sub('^.*?\\n','',sub('^.*?\\n','',STACKVAR)),STACKVAR),
                            STACKVAR=sub('\\n.*$','',STACKVAR)) %>% dplyr::select(-c(obsnum,rnum,totrows)) -> tmpdf

    #=========================================================================================
    # Relabel variables, then update all local reporting vars since DF structure has changed.
    #=========================================================================================
    Hmisc::label(tmpdf$STACKVAR) <- stacklabl
    for (v in names(tmpdf)){
      if (v %in% names(alllabls)){Hmisc::label(tmpdf[[v]]) <- alllabls[[v]]}
    }
    ncols1 <- ncols1 - (nstackvars-1)
    ncols2 <- ncols2 - (nstackvars-1)
    alllabls <- lapply(tmpdf[,!(names(tmpdf) %in% c('PAGEVAR'))], attr, "label")
    grpvars <- NULL

    ##==================================================================================
    ## If default widths have been used, collapse the first n values due to sharecols.
    ##==================================================================================
    colpse <- intersect(sharecolvars,names(widths))

    if (!is.null(colpse) && all(sharecolvars==colpse)){
      ##========
      ## widths
      ##========
      l1 <- widths[colpse]
      l2 <- widths[(length(colpse)+1):length(widths)]
      widths <- c(max(unlist(l1)),l2)
      ##=========
      ## widths2
      ##=========
      l1 <- widths2[colpse]
      l2 <- widths2[(length(colpse)+1):length(widths2)]
      widths2 <- c(max(unlist(l1)),l2)
    }

    #====================================================================
    # Remove consecutive duplicates that are blanks in STACKVAR column.
    #====================================================================
    tmpdf %>% dplyr::mutate(STACKVAR2=ifelse(stringr::str_trim(STACKVAR)=='','',STACKVAR),
                            STACKVAR2=ifelse(STACKVAR2=='',STACKVAR2,paste0(STACKVAR2,':',as.character(dplyr::row_number())))) -> tmpdf
    if ('PAGEVAR' %in% names(tmpdf)){
      tmpdf %>% dplyr::mutate(STACKVAR2=ifelse(STACKVAR2=='',STACKVAR2,paste0(STACKVAR2,':',as.character(PAGEVAR)))) -> tmpdf
    }
    tmpdf <- tmpdf[c(tmpdf$STACKVAR2[-1] != tmpdf$STACKVAR2[-nrow(tmpdf)],TRUE),] %>% dplyr::select(-STACKVAR2)
  }

  ##=========================================
  ## Labels list should not include pagevar.
  ##=========================================
  if ('PAGEVAR' %in% names(tmpdf)){
    alllabls <- lapply(tmpdf[,!(names(tmpdf) %in% c('PAGEVAR'))], attr, "label")
  }

  ##==================================##
  ## Drop noprintvars from dataframe. ##
  ##==================================##
  if (!is.null(noprintvars)){
    remvars <- intersect(noprintvars,names(tmpdf))
    if (length(remvars)>0){
      tmpdf %>% dplyr::select(-dplyr::all_of(remvars)) -> tmpdf
    }
  }

  ##=======================================
  ## Relabel variables prior to reporting.
  ##=======================================
  for (v in names(tmpdf)){
    if (v %in% names(alllabls)){Hmisc::label(tmpdf[[v]]) <- alllabls[[v]]}
  }

  #====================================================
  # Assign the column justifications to single vector.
  #====================================================
  nbsp <- "\\uc1\\u160*"
  coljust <- c()
  for (cn in names(tmpdf)){
    if (!(cn %in% pagevars)){
      if (cn %in% centrevars){
        coljust <- c(coljust,'c')
      } else if (cn %in% leftvars){
        coljust <- c(coljust,'l')
      } else if (cn %in% rightvars){
        coljust <- c(coljust,'r')
      } else {
        coljust <- c(coljust,'l')
      }
    }
  }
  if (is.null(pagevars)){
    newpage=FALSE
  } else {
    newpage=TRUE
  }

  #=======================================================
  # Reorder columns using columns argument and STACKVAR.
  #=======================================================
  allcols <- c('STACKVAR',columns,pagevars)
  colords <- intersect(allcols,names(tmpdf))
  tmpdf <- tmpdf[,colords]

  ##============================
  ## Save DD data set to file.
  ##============================
  if (!is.null(dddatasetlabel)){attr(tmpdf, "label") <- dddatasetlabel}
  saveRDS(tmpdf, rfenv$G_DDDATA)
  if (rfenv$G_DEBUG>0) {print(paste0('DD Dataframe written to file: ',rfenv$G_DDDATA))}

  ##==================================================
  ## Labels list must only be for columns variables.
  ##==================================================
  lblcols <- setdiff(colords,pagevars)
  collabls <- lapply(tmpdf[,lblcols], attr, "label")

  ##=================================
  ## Separate subline from pagevar.
  ##=================================
  if (!is.null(pagevars) && ('PAGEVAR' %in% (pagevars))){
    pgvar <- 'PAGEVAR'
    pagevars <- setdiff(pagevars,c('PAGEVAR'))
    if (length(pagevars) == 0) {pagevars <- NULL}
  } else {
    pgvar <- NULL
    newpage <- FALSE
  }

  ##============================
  ## Compute before page label.
  ##============================
  if (!is.null(computebeforepagelines)){
    grpvars <- setdiff(grpvars,pagevars)
    npgvars <- length(pagevars)
    tmpdf %>% dplyr::mutate(CBPVAL = eval(parse(text=computebeforepagelines))) %>% dplyr::select(-dplyr::all_of(pagevars)) -> tmpdf
    attr(tmpdf[['CBPVAL']],'label') <- 'Compute before page label'
    pagevars <- c('CBPVAL')
    npgdiff <- npgvars - 1
    ncols2 <- ncols2 - npgdiff
    if (npgdiff>0){widths2 <- utils::head(widths2,-npgdiff)}
    tmpdf <- tmpdf[,c(lblcols,pagevars,pgvar)]
  }

  ##============================================================
  ## Deal with noprintvars that are used elsewhere in the call.
  ##============================================================
  columns <- setdiff(columns,noprintvars)
  grpvars <- setdiff(grpvars,noprintvars)

  #============================
  # Invoke reporting function.
  #============================
  if (rfenv$G_DEBUG>0) {
    print(paste('columns: ',paste(columns,collapse=', ')))
    print(paste('noprintvars: ',paste(noprintvars,collapse=', ')))
    print(paste('grpvars: ',paste(grpvars,collapse=', ')))
    print(paste('pagevars: ', paste(pagevars,collapse=', ')))
    print(paste('pgvar: ', paste(pgvar,collapse=', ')))
    print(paste('newpage: ', paste(newpage,collapse=', ')))
    print(paste('widths: ' ,paste(widths,collapse=', ')))
    print(paste('widths2: ' ,paste(widths2,collapse=', ')))
    print(paste('ncols1: ', ncols1))
    print(paste('ncols2: ', ncols2))
    print(paste('ntitles: ', ntitles))
    print(paste('nftnotes: ', nftnotes))
    print(paste('coljust: ', paste(coljust,collapse=', ')))
    print(paste('collabls: ',paste(collabls,collapse='|')))
    print(paste0('rpp: ',rpp))
    print(paste('spanlbls: ', spanlbls))
    print(paste('spanwidths: ', paste(spanwidths,collapse=', ')))
    print(paste('spanjust: ', paste(spanjust,collapse=', ')))
    print(paste('spanbbord: ', paste(spanbbord,collapse=', ')))
  }

  if (is.null(pgvar) && is.null(pagevars)){rpp <- rpp-1}
  if (!is.null(nowidowvar) && is.null(sharecolvars) & !is.null(skipvars)) {rpp <- rpp + 1}

  if (display == 'Y'){

    tmpdf %>%
      r2rtf::rtf_title(title=titles,
                       text_font=rep(9,ntitles),
                       text_font_size=rep(rfenv$G_FONTSIZE,ntitles),
                       text_format=rep('',ntitles),
                       text_space_before=20) %>%
      r2rtf::rtf_page(orientation = rfenv$G_JUSTIFICATION,
                      nrow=(rpp - ntitles - nftnotes),
                      width=wdth,
                      col_width=colwdth,
                      margin=c(1.00,1.00,1.20,1.0,1.15,0.95)) %>%
      r2rtf::rtf_page_header(text=c(paste0("\\pard\\ql{\\f8 Protocol: ", rfenv$G_STUDYID ," \\line Population: ", rfenv$G_POPLBL ,
                                           " \\cell}\\cellx5000 \\pard\\qr{\\f8 Page \\chpgn  of {\\field{\\*\\fldinst NUMPAGES }} \\cell}\\cellx",spc," {\\row} \\fs1{ \\uc1\\u160* }")),
                             text_font_size=rfenv$G_FONTSIZE,
                             text_convert=FALSE) %>%
      r2rtf::rtf_page_footer(text=paste0('User: ', rfenv$G_USERID,' ',rfenv$G_PGMPTH,' ',format(Sys.time(), "%Y-%m-%d %T")),    # '\\i Source:\\i0 '
                             text_font=9,text_font_size=rfenv$G_FONTSIZE,text_justification='l',text_convert=FALSE) %>%

      {if (!is.null(span2lbls)) r2rtf::rtf_colheader(., colheader=span2lbls,
                                                     col_rel_width =span2widths,
                                                     text_justification = span2just,
                                                     border_left = rep('',length(span2widths)),
                                                     border_right = rep('',length(span2widths)),
                                                     border_bottom = span2bbord,
                                                     text_font=9,
                                                     text_font_size=rfenv$G_FONTSIZE) else .} %>%

      {if (!is.null(spanlbls)) r2rtf::rtf_colheader(., colheader=spanlbls,
                                                    col_rel_width =spanwidths,
                                                    text_justification = spanjust,
                                                    border_left = rep('',length(spanwidths)),
                                                    border_right = rep('',length(spanwidths)),
                                                    border_bottom = spanbbord,
                                                    border_top = spantbord,
                                                    text_font=9,
                                                    text_font_size=rfenv$G_FONTSIZE) else .} %>%

      r2rtf::rtf_colheader(colheader=paste(collabls,collapse='|'),
                           col_rel_width = widths,
                           text_justification = coljust,
                           border_top = rep(toprow,ncols1),
                           border_left = rep('',ncols1),
                           border_right = rep('',ncols1),
                           text_font=9,
                           text_font_size=rfenv$G_FONTSIZE) %>%
      r2rtf::rtf_body(col_rel_width = widths2,
                      text_justification = rep('l',ncols2),
                      border_left = rep('',ncols2),
                      border_right = rep('',ncols2),
                      text_font=9,
                      text_font_size=rfenv$G_FONTSIZE,
                      subline_by=pagevars,
                      page_by=pgvar,
                      pageby_header=TRUE,
                      new_page=newpage,
                      group_by=grpvars) %>%
      {if (!is.null(footnotes)) r2rtf::rtf_footnote(., footnote=footnotes, text_font=9, text_font_size=rfenv$G_FONTSIZE, text_justification='l', as_table=FALSE, text_convert=FALSE) else .} %>%
      r2rtf::rtf_encode(page_footnote = 'all') %>%
      r2rtf::write_rtf(rfenv$G_OUTFILE)

    #=================================================================
    # Check for and remove any floating page numbers in the RTF file.
    #=================================================================
    repfun::rem_pg_nums(rfenv$G_OUTFILE)

  } else {
    if (rfenv$G_DEBUG>0) {message('***** No display requested. *****')}
  }

  #=======================================================
  # Save DD data set as xpt file in addition to rds file.
  #=======================================================
  if (file.exists(rfenv$G_DDDATA) && !grepl("_page_[0-9][0-9]*\\.rds",rfenv$G_DDDATA)){
    if (toupper(xptyn)=='Y'){
      haven::write_xpt(readRDS(rfenv$G_DDDATA), gsub('rds','xpt',rfenv$G_DDDATA))
    }
  }

  if (G_DEBUG>0) print(paste0("RU_LIST: ", "End of RU_LIST"))
  return(invisible(NULL))
}
