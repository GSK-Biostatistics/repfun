#'
#' Pass values to setup function and the global environment will be arranged for use  reporting tools.
#'
#' @param R_DICTION Location of reporting dictionaries.
#' @param R_MACDIRS List of folders to search for functions when they are invoked.
#' @param R_DDDATA Location to write DDDATA reporting data sets.
#' @param R_OTHERDATA Location of additional production data sets.
#' @param R_INPUTDATA Location of permanent formats data sets and miscellaneous data sets.
#' @param R_RAWDATA Location of raw data sets.
#' @param R_SDTMDATA Location of SDTM data sets.
#' @param R_ADAMDATA Location of ADAM data sets.
#' @param R_RFMTDIR Location of format catalogs and corresponding lists.
#' @param D_CENTID Variable name for investigational center.
#' @param D_DATADATE Date of data sets for use in titles/footnotes.
#' @param D_DSPLYNUM Display number for title.
#' @param D_DSPLYTYP Type of Display ('T','L','F').
#' @param D_FONTSIZE Size of font on output file which is RTF by default (with automated PDF conversion via script).
#' @param D_KEEPPOPVARS Variables to keep on the population data set when merging to apply populations and sub-populations.
#' @param D_OUTFILE Production location for output TLFs.
#' @param D_PGMPTH Path of the driver file that generates current outputs or data sets.
#' @param D_STUDYID Protocol ID for the study.
#' @param D_STUDY_DESC Description of Study.
#' @param D_POP Population variable from ADSL that must equal Y for subjects to be included in the analysis.
#' @param D_POPDATA Data set that contains the population to be analyzed.
#' @param D_POPLBL Label for population being analyzed which can be used in the TLF header.
#' @param D_SUBJID The variable used to uniquely identify a subject in this analysis.
#' @param D_SUBPOP Condition to identify the sub-population when applied to ADSL.
#' @param D_SUBSET Condition to filter data from incoming source data sets used for this TLF.
#' @param D_TITLE1 First title text.
#' @param D_TITLE2 Second title text.
#' @param D_TITLE3 Third title text.
#' @param D_TITLE4 Fourth title text.
#' @param D_TITLE5 Fifth title text.
#' @param D_TITLE6 Sixth title text.
#' @param D_TITLE7 Seventh title text.
#' @param D_FOOT1 First footnote text.
#' @param D_FOOT2 Second footnote text.
#' @param D_FOOT3 Third footnote text.
#' @param D_FOOT4 Fourth footnote text.
#' @param D_FOOT5 Fifth footnote text.
#' @param D_FOOT6 Sixth footnote text.
#' @param D_FOOT7 Seventh footnote text.
#' @param D_FOOT8 Eighth footnote text.
#' @param D_FOOT9 Ninth footnote text.
#' @param D_USERID User name.
#' @param D_RTFYN Y or N to generate RTf output.
#' @param D_DEBUG Level of debugging to show in log files.
#'
#' @return Global variables defined for use with the reporting tools.
#'
#' @author Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com} \cr
#'         Chris Rook, \email{cr883296@gmail.com}
#'
#' @examples
#' library(repfun)
#' library(dplyr)
#' datdir <- file.path(gsub("\\","/",tempdir(),fixed=TRUE),"datdir")
#' dir.create(datdir,showWarnings=FALSE)
#' outdir <- file.path(gsub("\\","/",tempdir(),fixed=TRUE),"outdir")
#' dir.create(outdir,showWarnings=FALSE)
#' fmtdir <- file.path(gsub("\\","/",tempdir(),fixed=TRUE),"fmtdir")
#' dir.create(fmtdir,showWarnings=FALSE)
#' repfun::copydata(datdir)
#' repfun::rs_setup(
#' D_CENTID="SITEID",
#' D_DATADATE=Sys.Date(),
#' D_DSPLYNUM=1,
#' D_DSPLYTYP='T',
#' D_FONTSIZE=10,
#' D_FOOT1='1.) Only treatment emergent events related to lipids are displayed.',
#' D_FOOT2='2.) Subjects are counted once in each body system & preferred term.',
#' D_KEEPPOPVARS=c('STUDYID','USUBJID','SAFFL'),
#' D_USERID=Sys.getenv("USERNAME"),
#' D_STUDYID='ABCXYZPDQ',
#' D_POP="SAFFL",
#' D_POPDATA=repfun::adsl %>% dplyr::filter(SAFFL=='Y'),
#' D_POPLBL="Safety",
#' D_SUBJID=c("STUDYID","USUBJID"),
#' D_TITLE1=paste0('Table 1: Summary of Treatment Emergent Adverse Events'),
#' D_RTFYN="Y",
#' D_DEBUG=0,
#' R_DICTION=NULL,
#' R_OTHERDATA=NULL,
#' R_INPUTDATA=NULL,
#' R_RAWDATA=NULL,
#' R_SDTMDATA=NULL,
#' R_ADAMDATA=datdir,
#' R_RFMTDIR=fmtdir,
#' R_DDDATA=paste0(outdir,'/t_ru_list_1.rds'),
#' D_OUTFILE=paste0(outdir,"/t_ru_list_1.rtf"),
#' D_PGMPTH="/path/to/code/rs_setup.R")
#'
#' @export
#'
rs_setup <- function(
    R_DICTION="./inst/formats",
    R_MACDIRS=NULL,
    R_DDDATA=NULL,
    R_OTHERDATA=paste0("./data"),
    R_INPUTDATA=paste0("./data"),
    R_RAWDATA=paste0("./data"),
    R_SDTMDATA=paste0("./data"),
    R_ADAMDATA=paste0("./data"),
    R_RFMTDIR=paste0("./inst/formats"),
    D_CENTID="SITEID",
    D_DATADATE=NULL,
    D_DSPLYNUM=1,
    D_DSPLYTYP='T',
    D_FONTSIZE=10,
    D_KEEPPOPVARS=NULL,
    D_OUTFILE="./inst/t_ru_list_1.rtf",
    D_PGMPTH="./R/rs_setup.R",
    D_STUDYID=NULL,
    D_STUDY_DESC=NULL,
    D_POP="ITTEFL",
    D_POPDATA=NULL,
    D_POPLBL="Intent-to-Treat",
    D_SUBJID="USUBJID",
    D_SUBPOP=NULL,
    D_SUBSET=NULL,
    D_TITLE1=NULL,
    D_TITLE2=NULL,
    D_TITLE3=NULL,
    D_TITLE4=NULL,
    D_TITLE5=NULL,
    D_TITLE6=NULL,
    D_TITLE7=NULL,
    D_FOOT1=NULL,
    D_FOOT2=NULL,
    D_FOOT3=NULL,
    D_FOOT4=NULL,
    D_FOOT5=NULL,
    D_FOOT6=NULL,
    D_FOOT7=NULL,
    D_FOOT8=NULL,
    D_FOOT9=NULL,
    D_USERID=Sys.getenv("USERNAME"),
    D_RTFYN="N",
    D_DEBUG=0
) {

  #==============================
  # Global Variables
  #==============================

  rfenv$G_CENTID <- D_CENTID
  rfenv$G_DATADATE <- D_DATADATE
  rfenv$G_DSPLYNUM <- D_DSPLYNUM
  rfenv$G_DSPLYTYP <- D_DSPLYTYP

  if (grepl("L",toupper(as.character(D_FONTSIZE)))) {
      rfenv$G_FONTSIZE <- as.numeric(gsub('L','',toupper(as.character(D_FONTSIZE))))
      rfenv$G_JUSTIFICATION <- "landscape"
  } else if (grepl("P",toupper(D_FONTSIZE))){
      rfenv$G_FONTSIZE <- as.numeric(gsub('P','',toupper(as.character(D_FONTSIZE))))
      rfenv$G_JUSTIFICATION <- "portrait"
  } else {
      rfenv$G_FONTSIZE <- as.numeric(gsub('L','',toupper(as.character(D_FONTSIZE))))
      rfenv$G_JUSTIFICATION <- "landscape"
  }

  rfenv$G_KEEPPOPVARS <- D_KEEPPOPVARS
  rfenv$G_OUTFILE <- D_OUTFILE
  rfenv$G_PGMPTH <- D_PGMPTH
  rfenv$G_POP <- D_POP
  rfenv$G_POPLBL <- D_POPLBL
  rfenv$G_SUBJID <- D_SUBJID
  rfenv$G_SUBPOP <- D_SUBPOP
  rfenv$G_SUBSET <- D_SUBSET
  rfenv$G_USERID <- D_USERID
  rfenv$G_DICTION <- R_DICTION
  rfenv$G_MACDIRS <- R_MACDIRS
  rfenv$G_DDDATA  <- R_DDDATA
  rfenv$G_OTHERDATA <- R_OTHERDATA
  rfenv$G_INPUTDATA <- R_INPUTDATA
  rfenv$G_RAWDATA <- R_RAWDATA
  rfenv$G_SDTMDATA <- R_SDTMDATA
  rfenv$G_ADAMDATA <- R_ADAMDATA
  rfenv$G_RFMTDIR <- R_RFMTDIR
  rfenv$G_FOOT1  <- D_FOOT1
  rfenv$G_FOOT2  <- D_FOOT2
  rfenv$G_FOOT3  <- D_FOOT3
  rfenv$G_FOOT4  <- D_FOOT4
  rfenv$G_FOOT5  <- D_FOOT5
  rfenv$G_FOOT6  <- D_FOOT6
  rfenv$G_FOOT7  <- D_FOOT7
  rfenv$G_FOOT8  <- D_FOOT8
  rfenv$G_FOOT9  <- D_FOOT9
  rfenv$G_TITLE1 <- D_TITLE1
  rfenv$G_TITLE2 <- D_TITLE2
  rfenv$G_TITLE3 <- D_TITLE3
  rfenv$G_TITLE4 <- D_TITLE4
  rfenv$G_TITLE5 <- D_TITLE5
  rfenv$G_TITLE6 <- D_TITLE6
  rfenv$G_TITLE7 <- D_TITLE7
  rfenv$G_RTFYN <- D_RTFYN
  rfenv$G_DEBUG <- D_DEBUG

  rfenv$G_ABORT <- 0
  rfenv$G_STUDYID <- D_STUDYID
  rfenv$G_STUDY_DESC <- D_STUDY_DESC

  #==============================
  # Needed functions.
  #==============================
  ru_loadlocalfunctions <- function(...) {
    argg <- c(as.list(environment()), list(...))
    op <- options(); on.exit(options(op))
    for (j in 1:length(argg)) {
      this.file <- argg[[j]]
      f.local <- list.files(this.file, pattern="(^ru_.*\\.R$)", full.names=TRUE)
      # loop through each line in your script..
      if (length(f.local) > 0) {
        k <- 0
        while ( TRUE ) {
          k <- k+1
          # print(paste(this.file, f.local[k], sep="/"))
          base::tryCatch(base::source(f.local[k], local=FALSE),
                         error = function(e) message("Oops!  ", as.character(e)))
          options(op)
          if (k + 1 > length(f.local)) break
        }
      }
    }
  }

  if (! is.null(rfenv$G_MACDIRS)) for (i in 1:length(rfenv$G_MACDIRS)) {
    ru_loadlocalfunctions(rfenv$G_MACDIRS[i])
  }

  rfenv$adamdata <- NULL
  rfenv$othdata <- NULL
  rfenv$indata <- NULL
  rfenv$rawdata <- NULL
  rfenv$sdtmdata <- NULL
  rfenv$diction <- NULL
  rfenv$rfmtdata <- NULL

  if (! is.null(rfenv$G_OTHERDATA)) rfenv$othdata <- repfun::ru_libname(rfenv$G_OTHERDATA)
  if (! is.null(rfenv$G_INPUTDATA)) rfenv$indata <- repfun::ru_libname(rfenv$G_INPUTDATA)
  if (! is.null(rfenv$G_RAWDATA)) rfenv$rawdata <- repfun::ru_libname(rfenv$G_RAWDATA)
  if (! is.null(rfenv$G_SDTMDATA)) rfenv$sdtmdata <- repfun::ru_libname(rfenv$G_SDTMDATA)
  if (! is.null(rfenv$G_ADAMDATA)) rfenv$adamdata <- repfun::ru_libname(rfenv$G_ADAMDATA)
  if (! is.null(rfenv$G_DICTION)) rfenv$diction <- repfun::ru_libname(rfenv$G_DICTION)
  if (! is.null(rfenv$G_RFMTDIR)) rfenv$rfmtdata <- repfun::ru_libname(rfenv$G_RFMTDIR)

  # base::force(D_POPDATA)
  rfenv$G_POPDATA <- D_POPDATA
  rfenv$G_FORMATS <- NULL

  if (is.function(rfenv$rfmtdata$formtdat) && is.function(rfenv$diction$formtdat)) {
    d.dict <- rfenv$diction$formtdat()
    d.rfmt <- rfenv$rfmtdata$formtdat()
    names(d.dict) <- toupper(names(d.dict))
    names(d.rfmt) <- toupper(names(d.rfmt))

    d.fmt.1 <- d.dict[,"FMTNAME"] %>% dplyr::distinct()
    d.fmt.2 <- d.rfmt[,"FMTNAME"] %>% dplyr::distinct()
    s.fmt.1 <- unlist(d.fmt.1)
    s.fmt.2 <- unlist(d.fmt.2)
    s.fmt.3 <- base::intersect(s.fmt.1, s.fmt.2)
    if (length(s.fmt.3) > 0) {
      d.fmt.1 <-subset(d.dict, ! d.dict$FMTNAME %in% s.fmt.3)
      d.fmt <- d.fmt <- merge(d.rfmt, d.fmt.1, by="FMTNAME", all.x=FALSE, all.y=FALSE)
      d.fmt <- ru_setdata(d.rfmt, d.fmt, keeprownames = FALSE)
    } else {
      d.fmt <- ru_setdata(d.dict, d.rfmt, keeprownames = FALSE)
    }
    rfenv$fmtlist <- ru_data2codelist(d.fmt, codelistvarname="FMTNAME", codevarname="START",
                                 decodevarname="LABEL", typevarname="TYPE")
  } else if (is.function(rfenv$diction$formtdat)) {
    d.dict <- rfenv$diction$formtdat()
    names(d.dict) <- toupper(names(d.dict))
    rfenv$fmtlist <- ru_data2codelist(d.dict, codelistvarname="FMTNAME", codevarname="START",
                                 decodevarname="LABEL", typevarname="TYPE")
  } else if (is.function(rfenv$rfmtdata$formtdat)) {
    d.rfmt <- rfenv$rfmtdata$formtdat()
    names(d.rfmt) <- toupper(names(d.rfmt))
    rfenv$fmtlist <- ru_data2codelist(d.rfmt, codelistvarname="FMTNAME", codevarname="START",
                                 decodevarname="LABEL", typevarname="TYPE")
  } else { rfenv$fmtlist <- NULL }

  #===================================================================
  # Derive global variables for page size in inches (height & width)
  #===================================================================
  G_NTITLES <- 2
  G_NFOOTERS <- 1
  for (i in 1:7) {
    if (! is.null(get(paste0('D_TITLE',i)))) G_NTITLES <- G_NTITLES + 1
  }
  for (i in 1:9) {
    if (! is.null(get(paste0('D_FOOT',i)))) G_NFOOTERS <- G_NFOOTERS + 1
  }
  G_LINESAPCE <- 11.25
  G_TMARGIN <- 1.2
  G_BMARGIN <- 1.0
  G_LMARGIN <- 1.0
  G_RMARGIN <- 1.0

  frac <- 4.0 - (G_NTITLES+G_NFOOTERS-3)/10 - 0.05
  rfenv$G_HEIGHT <- (8.5 - G_TMARGIN - G_BMARGIN - (G_NFOOTERS + G_NTITLES + 1)*((rfenv$G_FONTSIZE + frac)/72))
  rfenv$G_WIDTH <- (11 - G_LMARGIN - G_RMARGIN)

  if (rfenv$G_JUSTIFICATION == "portrait"){
    rfenv$G_WIDTH <- 6.5
    rfenv$G_HEIGHT <- 11 - (8.5-rfenv$G_HEIGHT)
  }

  #====================================================================
  # Write rfenv variables to file GLOBALS.txt in temporary directory.
  #====================================================================
  globs <- paste0(tempdir(),'/GLOBALS.txt')
  if (file.exists(globs)){file.remove(globs)}
  writeLines("",globs)
  for (f in ls(rfenv)){
    if (typeof(rfenv[[f]]) == "list" & !is.data.frame(rfenv[[f]])){
       write(paste0(f," <- new.env()"),file=globs,append=TRUE)
       for (n in names(rfenv[[f]])){
         write(paste0(f,'$',n,' <- ',paste(deparse(rfenv[[f]][[n]]),collapse='\n')),file=globs,append=TRUE)
       }
    } else {
       if (is.data.frame(rfenv[[f]]) & f=='G_POPDATA'){
           G_POPDATA <- data.frame()
           assign(f,rfenv[[f]])
           save(G_POPDATA, file = paste0(tempdir(),'/',f))
           write(paste0('assign("',f,'",get(load("',gsub('\\','/',tempdir(),fixed=TRUE),'/',f,'")))'),file=globs,append=TRUE)
       } else {
           if (is.null(rfenv[[f]])){
               write(paste0(f,' <- NULL'),file=globs,append=TRUE)
           } else if (is.list(rfenv[[f]]) | is.vector(rfenv[[f]])){
               write(paste0(f,' <- ',deparse(rfenv[[f]])),file=globs,append=TRUE)
           } else if (is.numeric(rfenv[[f]])){
               write(paste0(f,' <- ',as.character(rfenv[[f]])),file=globs,append=TRUE)
           } else {
               write(paste0(f,' <- "',gsub('"','\\"',gsub("\\",'/',as.character(rfenv[[f]]),fixed=TRUE),fixed=TRUE),'"'),file=globs,append=TRUE)
           }
       }
    }
  }
}
