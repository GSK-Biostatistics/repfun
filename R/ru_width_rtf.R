#' Create a List of Relative Widths of Columns for RTF Outputs
#'
#' Pass in a data set and identify the columns for reporting to have estimated relative column width returned.
#'
#' @param dsetin A dataframe containing columns for reporting.
#' @param varsin A vector of variables on the reporting dataframe that will be displayed in the output.
#' @param widths Provide a set of default widths for some or all variables as desired. (These will be used.)
#' @param type Specify the type of width to be computed and returned.  (Currently supports PCT, which refers to relative widths not percentages.)
#'
#' @return A list of widths of the same size as the list specified by varsin.
#'
#' @author Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com} \cr
#'         Chris Rook, \email{christopher.x.rook@gsk.com}
#'
#' @examples
#' library(repfun)
#' library(dplyr)
#' datdir <- file.path(gsub("\\","/",tempdir(),fixed=TRUE),"datdir");
#' dir.create(datdir,showWarnings=FALSE)
#' repfun::copydata(datdir)
#' repfun::rs_setup(D_POPDATA=repfun::adsl %>% dplyr::filter(SAFFL =='Y'),
#'          D_SUBJID=c("STUDYID","USUBJID"),
#'          R_DICTION=NULL,
#'          R_OTHERDATA=NULL,
#'          R_INPUTDATA=NULL,
#'          R_RAWDATA=NULL,
#'          R_SDTMDATA=NULL,
#'          R_ADAMDATA=datdir)
#' G_POPDATA <- repfun:::rfenv$G_POPDATA %>%
#'     dplyr::mutate(
#'          TRT01AN=ifelse(TRT01A=='Placebo',1,
#'                  ifelse(TRT01A=='Xanomeline Low Dose',2,3))) %>%
#'     repfun::ru_labels(varlabels=list('TRT01AN'='Actual Treatment for Period 01 (n)'))
#' adae <- repfun:::rfenv$adamdata$adae.rda() %>%
#'         dplyr::inner_join(G_POPDATA, by=c('STUDYID','USUBJID','SAFFL','TRT01A'))
#' aesum_p <- repfun::ru_freq(adae,
#'                    dsetindenom=G_POPDATA,
#'                    countdistinctvars=c('STUDYID','USUBJID'),
#'                    groupbyvarsnumer=c('TRT01AN','TRT01A','AEBODSYS','AEDECOD'),
#'                    anyeventvars = c('AEBODSYS','AEDECOD'),
#'                    anyeventvalues = c('ANY EVENT','ANY EVENT'),
#'                    groupbyvarsdenom=c('TRT01AN'),
#'                    resultstyle="NUMERPCT",
#'                    totalforvar=c('TRT01AN'),
#'                    totalid=99,
#'                    totaldecode='Total',
#'                    codedecodevarpairs=c("TRT01AN", "TRT01A"),
#'                    varcodelistpairs=c(""),
#'                    codelistnames=list(),
#'                    resultpctdps=0) %>%
#'                    repfun::ru_denorm(varstodenorm=c("tt_result", "PERCENT"),
#'                              groupbyvars=c("tt_summarylevel", "AEBODSYS", "AEDECOD"),
#'                              acrossvar="TRT01AN", acrossvarlabel="TRT01A",
#'                              acrossvarprefix=c("tt_ac", "tt_p")) %>%
#'                    dplyr::mutate(ord1=ifelse(tt_summarylevel==0,0,1)) %>%
#'                    dplyr::rename(ord2=tt_summarylevel) %>%
#'                    dplyr::arrange(ord1,AEBODSYS,ord2,AEDECOD) %>%
#'                    dplyr::select(-c(starts_with('tt_p'),starts_with('ord'))) %>%
#'                    repfun::ru_addpage(grpvars=c('AEBODSYS'),rowsprbdy=35,nosplitvars=TRUE)
#'
#' widths1 <- repfun::ru_width_rtf(aesum_p,
#'            c('AEBODSYS','AEDECOD','tt_ac01','tt_ac02','tt_ac03','tt_ac99'))
#' print(widths1)
#' widths2 <- repfun::ru_width_rtf(aesum_p,
#'            c('AEBODSYS','AEDECOD','tt_ac01','tt_ac02','tt_ac03','tt_ac99'),
#'            list('AEBODSYS'=35, 'AEDECOD'=30))
#' print(widths2)
#'
#' @export
#'
ru_width_rtf <- function (dsetin, varsin=list(), widths=list(), type="PCT") {

  #if (G_DEBUG>0) print(paste0("RU_WIDTH_RTF: ", "Start of RU_WIDTH_RTF"))

  n.totalwidth <- 0
  n.totaldefaultwidth <- 0
  s.labels <- labels(dsetin)

  for (i in 1:length(varsin)) {
    v.thisvar <- varsin[i]
    if (is.numeric(dsetin[varsin[i]])) {
      n.this_width <- 8
    } else {
      n.this_width <- 1
      for (j in 1:nrow(dsetin)) {
        v.thisvar.v <- dsetin[j, varsin[i]]
        if (! (is.na(v.thisvar.v) || is.null(v.thisvar.v) )) {
          n.this_width.1 <- nchar(v.thisvar.v)
          if (n.this_width < n.this_width.1) n.this_width <- n.this_width.1
        }
      }
    }
    s.this_label <- unlist(s.labels[varsin[i]])
    if (! (is.na(s.this_label) || is.null(s.this_label) )) {
      n.label_width <- nchar(s.this_label)
    } else { n.label_width <- 1}

    if (n.this_width < n.label_width) n.this_width <- n.label_width

    n.default_width <- unlist(widths[varsin[i]])
    if ((is.null(n.default_width)|| is.na(n.default_width[1]))) {
      n.default_width <- 0
    } else {n.this_width <- n.default_width}

    n.totalwidth <- n.this_width + n.totalwidth
    n.totaldefaultwidth <- n.default_width + n.totaldefaultwidth

    if (i == 1) n.default_widths <- n.default_width
    else n.default_widths <- c(n.default_widths, n.default_width)

    if (i == 1) n.widths <- n.this_width
    else n.widths <- c(n.widths, n.this_width)
  }
  #print(c("RU_WIDTH_RTF: TYPE: ", type))
  if (base::toupper(type) == "PCT") {
    for (k in 1:length(n.widths)) {
      if (n.totaldefaultwidth >= n.totalwidth ) {
        if (k == 1) n.widths.1 <- base::round(100 * n.widths[k]/n.totalwidth , 0)
        else n.widths.1 <- c(n.widths.1, base::round(100 * n.widths[k]/n.totalwidth, 0))
      } else if (n.default_widths[k] == 0 ) {
        if (k == 1) n.widths.1 <- base::round((100- n.totaldefaultwidth) * n.widths[k]/(n.totalwidth - n.totaldefaultwidth), 0)
        else n.widths.1 <- c(n.widths.1, base::round((100 - n.totaldefaultwidth) * n.widths[k]/(n.totalwidth - n.totaldefaultwidth), 0))
      } else {
        if (k == 1) n.widths.1 <- n.default_widths[k]
        else n.widths.1 <- c(n.widths.1, n.default_widths[k])
      }
    }
  } else {
    n.widths.1 <- n.widths
  }
  names(n.widths.1) <- varsin
  #if (G_DEBUG>0) print(paste0("RU_WIDTH_RTF: ", "End of RU_WIDTH_RTF"))
  return(n.widths.1)
}
