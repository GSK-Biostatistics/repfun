#' Assign Big N to Data Frame.
#'
#' Pass in a data frame along with identification options and have Big N added to it.
#'
#' @param dsetintoaddbign The data set that will hold the derived big N value.
#' @param dsetintocount The data set that will be counted to generate big N.
#' @param countdistinctvars Variable(s) that contain values to be counted uniquely within any output grouping.
#' @param groupbyvars Variables in DSETINTOCOUNT to group the data by when counting to deriving the big N.
#' @param totalforvar Variable for which overall totals are required within all other grouped class variables.
#' @param totalid Value(s) used to populate the variable(s) specified in totalforvar.
#' @param totaldecode Value(s) used to populate the variable(s) of the decode variable(s) of the totalforvar.
#' @param codedecodevarpairs Specifies code and decode variable pairs. Those variables should be in parameter GROUPBYVARSNUMER.
#'                           One variable in the pair will contain the code, which is used in counting and ordering, and the other
#'                           will contain decode, which is used for presentation.
#' @param varcodelistpairs List of code/decode pairs of variables.
#' @param codelistnames List of decodes for use with decoding code/decode pairs.
#' @param addbigntovarvalue Place big N in a new variable or append to an existing variable (last groupbyvars value)?
#' @param splitchar Text to insert between existing string and big N.
#'
#' @return A data frame based on the incoming data frame but collapsed by groups with descriptive statistics added.
#'
#' @author Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com} \cr
#'         Chris Rook, \email{christopher.x.rook@gsk.com}
#'
#' @examples
#' library(repfun)
#' library(dplyr)
#' library(tibble)
#' #rfenv <- if (exists('rfenv') && is.environment(get('rfenv'))){
#' #              rfenv
#' #          } else {
#' #              rfenv <- new.env(parent = emptyenv())
#' #              rfenv$G_DEBUG <- 0
#' #              rfenv
#' #          }
#' datdir <- file.path(gsub("\\","/",tempdir(),fixed=TRUE),"datdir");
#' dir.create(datdir,showWarnings=FALSE)
#' repfun::copydata(datdir)
#' repfun::rs_setup(D_POPDATA=repfun::adsl %>% dplyr::filter(SAFFL =='Y'),
#'                  D_SUBJID=c("STUDYID","USUBJID"),
#'                  R_DICTION=NULL,
#'                  R_OTHERDATA=NULL,
#'                  R_INPUTDATA=NULL,
#'                  R_RAWDATA=NULL,
#'                  R_SDTMDATA=NULL,
#'                  R_ADAMDATA=datdir)
#' G_POPDATA <- repfun:::rfenv$G_POPDATA %>% dplyr::mutate(TRT01AN=
#'                                          ifelse(TRT01A=='Placebo',1,
#'                                          ifelse(TRT01A=='Xanomeline Low Dose',2,3)))
#' attr(G_POPDATA$TRT01AN,"label") <- 'Actual Treatment for Period 01 (n)'
#' adae <- tibble::as_tibble(repfun:::rfenv$adamdata$adae.rda()) %>%
#'         dplyr::inner_join(G_POPDATA,
#'                           by=c('STUDYID','USUBJID','SAFFL','TRT01A')) %>%
#'         dplyr::filter(TRTEMFL=='Y')
#' addbign <- repfun::ru_addbignvar(adae,
#'                          G_POPDATA,
#'                          groupbyvars=c("TRT01AN", "TRT01A"),
#'                          countdistinctvars=c("STUDYID", "USUBJID"),
#'                          totalforvar=c("TRT01AN"),
#'                          totalid = 99,
#'                          totaldecode = 'Total',
#'                          codedecodevarpairs=c("TRT01AN", "TRT01A"),
#'                          varcodelistpairs=c(""),
#'                          codelistnames=list(),
#'                          addbigntovarvalue=TRUE,
#'                          splitchar="~") %>%
#'            dplyr::select(STUDYID, USUBJID, TRT01AN, TRT01A, AEBODSYS, AEDECOD)
#'
#' @export
#'
ru_addbignvar <- function (dsetintoaddbign,
                           dsetintocount,
                           countdistinctvars=c("STUDYID", "USUBJID"),
                           groupbyvars=NULL,
                           totalforvar=NULL,
                           totalid=NULL,
                           totaldecode=c('Total'),
                           codedecodevarpairs=NULL,
                           varcodelistpairs=NULL,
                           codelistnames=list(),
                           addbigntovarvalue=TRUE,
                           splitchar=" ") {

  #print(paste0("RU_ADDBIGNVAR: ", "Start of RU_ADDBIGNVAR"))

  bignvarname <- "tt_bnnm"
  df_sub_2 <- ru_freq(dsetintocount, dsetindenom=NULL, countdistinctvars=countdistinctvars, groupbyvarsnumer=groupbyvars,
                      groupbyvarsdenom=NULL, resultstyle="NUMER", totalforvar=totalforvar, totalid=totalid, totaldecode=totaldecode,
                      anyeventvars=NULL, anyeventvalues=NULL, codedecodevarpairs=codedecodevarpairs, varcodelistpairs=varcodelistpairs,
                      codelistnames=codelistnames, groupminmaxvar=NULL, resultpctdps=0) %>% dplyr::rename(tt_bnnm=NUMERCNT) %>%
    dplyr::select(dplyr::all_of(c(groupbyvars, "tt_bnnm")))
  if (addbigntovarvalue) {
    str_var <- groupbyvars[length(groupbyvars)]
    df_test <- merge(x=dsetintoaddbign, y=df_sub_2, by=c(groupbyvars), all.x=TRUE, all.y=FALSE)
    df_out <- merge(x=dsetintoaddbign, y=df_sub_2, by=c(groupbyvars), all.x=TRUE, all.y=FALSE) %>%
      dplyr::mutate(!! str_var := paste0(!!! rlang::syms(str_var), !! splitchar, "(N=", tt_bnnm, ")"))
  } else {df_out <- merge(x=dsetintoaddbign, y=df_sub_2, by=c(groupbyvars), all.x=TRUE, all.y=FALSE)}

  #this_labels <- base::labels(dsetintoaddbign)
  this_labels <- lapply(dsetintoaddbign,function(x){attr(x,"label")})
  this_labels[["tt_bnnm"]] <- "N"
  df_out <- ru_labels(df_out, this_labels)
  #print(paste0("RU_ADDBIGNVAR: ", "End or RU_ADDBIGNVAR"))

  as.data.frame(df_out)
}
