#' Transpose a Data Frame
#'
#' Pass in a data frame along with identification options and have it transposed (denormalized, long to wide) to display treatment columns.
#'
#' dsetin, varstodenorm=NULL, groupbyvars=NULL, acrossvar=NULL, acrossvarlabel=NULL, acrossvarprefix="tt_", acrossvarsuffix=NULL
#'
#' @param dsetin The data set to transpose.
#' @param varstodenorm The variables to transpose.
#' @param groupbyvars Definition of one row in the output data frame.
#' @param acrossvar Variable to define the columns in the transposed data frame.
#' @param acrossvarlabel Variable to define the labels in the transposed data frame.
#' @param acrossvarprefix Add to the beginning of each value in the across variable in the output data frame.
#' @param acrossvarsuffix Add to the end of each value in the across variable in the output data frame.
#'
#' @return A data frame based on the incoming data frame transposed from long to wide.
#'
#' @author Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com} \cr
#'         Chris Rook, \email{cr883296@gmail.com}
#'
#' @examples
#' #====================
#' # AEs: N and Percent
#' #====================
#' library(repfun)
#' library(dplyr)
#' datdir <- file.path(gsub("\\","/",tempdir(),fixed=TRUE),"datdir")
#' dir.create(datdir,showWarnings=FALSE)
#' repfun::copydata(datdir)
#' repfun::rs_setup(D_POP="SAFFL",
#'                  D_POPLBL="Safety",
#'                  D_POPDATA=repfun::adsl %>% dplyr::filter(SAFFL =='Y'),
#'                  D_SUBJID=c("STUDYID","USUBJID"),
#'                  R_DICTION=NULL,
#'                  R_OTHERDATA=NULL,
#'                  R_INPUTDATA=NULL,
#'                  R_RAWDATA=NULL,
#'                  R_SDTMDATA=NULL,
#'                  R_ADAMDATA=datdir)
#' G_POPDATA <- repfun:::rfenv$G_POPDATA %>%
#'   dplyr::mutate(TRT01AN=ifelse(TRT01A=='Placebo',1,
#'                  ifelse(TRT01A=='Xanomeline Low Dose',2,3))) %>%
#'   repfun::ru_labels(varlabels=list('TRT01AN'='Actual Treatment for Period 01 (n)'))
#' adae <- repfun:::rfenv$adamdata$adae.rda() %>% select(-SAFFL) %>%
#'         repfun::ru_getdata(G_POPDATA, c("STUDYID", "USUBJID"),
#'                    keeppopvars=c("TRT01AN", "TRT01A"))
#' aesum_t <- repfun::ru_freq(adae,
#'               dsetindenom=G_POPDATA,
#'               countdistinctvars=c('STUDYID','USUBJID'),
#'               groupbyvarsnumer=c('TRT01AN','TRT01A','AEBODSYS','AEDECOD'),
#'               anyeventvars = c('AEBODSYS','AEDECOD'),
#'               anyeventvalues = c('ANY EVENT','ANY EVENT'),
#'               groupbyvarsdenom=c('TRT01AN'),
#'               resultstyle="NUMERPCT",
#'               totalforvar=c('TRT01AN'),
#'               totalid=99,
#'               totaldecode='Total',
#'               codedecodevarpairs=c("TRT01AN", "TRT01A"),
#'               varcodelistpairs=c(""),
#'               codelistnames=list(),
#'               resultpctdps=0) %>%
#'   repfun::ru_denorm(varstodenorm=c("tt_result", "PERCENT"),
#'             groupbyvars=c("tt_summarylevel", "AEBODSYS", "AEDECOD"),
#'             acrossvar="TRT01AN",
#'             acrossvarlabel="TRT01A",
#'             acrossvarprefix=c("tt_ac", "tt_p"))
#'
#' #======================================
#' # Demography Statistics: N and Percent
#' #======================================
#' repfun::rs_setup(D_POP="SAFFL",
#'                  D_POPLBL="Safety",
#'                  D_POPDATA=repfun::adsl %>% dplyr::filter(SAFFL =='Y'),
#'                  D_SUBJID=c("STUDYID","USUBJID"),
#'                  R_DICTION=NULL,
#'                  R_OTHERDATA=NULL,
#'                  R_INPUTDATA=NULL,
#'                  R_RAWDATA=NULL,
#'                  R_SDTMDATA=NULL,
#'                  R_ADAMDATA=datdir)
#' G_POPDATA <- repfun:::rfenv$G_POPDATA %>%
#'               dplyr::mutate(TRT01AN=
#'                      ifelse(TRT01A=='Placebo',1,
#'                      ifelse(TRT01A=='Xanomeline Low Dose',2,3))) %>%
#'   repfun::ru_labels(varlabels=list('TRT01AN'='Actual Treatment for Period 01 (n)'))
#' demstats_t <- repfun::ru_sumstats(G_POPDATA,
#'                  analysisvars=c("AGE","TRTDURD"),
#'                  groupbyvars=c("STUDYID","TRT01AN"),
#'                  codedecodevarpairs=c("TRT01AN", "TRT01A"),
#'                  totalforvar="TRT01AN", totalid=99,
#'                  totaldecode="Total",
#'                  statsinrowsyn = "Y",
#'                  analysisvardps=list("AGE"=1,"TRTDURD"=2),
#'                  statslist=c("n", "mean", "median", "sd", "min", "max")) %>%
#'   repfun::ru_denorm(varstodenorm=c("tt_result"),
#'             groupbyvars=c("tt_avid", "tt_avnm", "tt_svid", "tt_svnm"),
#'             acrossvar="TRT01AN", acrossvarlabel="TRT01A",
#'             acrossvarprefix=c("tt_ac"))
#'
#' @export
#'
ru_denorm <- function (dsetin,
                       varstodenorm=NULL,
                       groupbyvars=NULL,
                       acrossvar=NULL,
                       acrossvarlabel=NULL,
                       acrossvarprefix="tt_",
                       acrossvarsuffix=NULL) {

  # print(paste0("RU_DENORM: ", "Start of RU_DENORM"))
  # dsetin <- df_art.5; varstodenorm <- c("CMDECOD"); groupbyvars <- c("STUDYID", "USUBJID", "SVISIT");  acrossvar <- NULL
  # acrossvarlabel <- NULL; acrossvarprefix <- "TT_AC"

  if (nrow(dsetin) == 0) {
    return(dsetin)
  }

  if (is.null(acrossvar)) {
    df_across <- dsetin %>% dplyr::select(dplyr::all_of(varstodenorm)) %>% dplyr::distinct() %>% sort() %>% dplyr::mutate(SEQA__=dplyr::row_number())

    df_dsetin <- base::merge(x = dsetin, y = df_across, by = c(varstodenorm), all.x = TRUE, all.y=TRUE)
    acrossvar <- "SEQA__"
  } else df_dsetin <- dsetin

  df_check_1 <- df_dsetin %>% dplyr::group_by(df_dsetin[c(groupbyvars, varstodenorm, acrossvar, acrossvarlabel)]) %>%
    dplyr::filter(dplyr::n() > 1) %>% dplyr::select(dplyr::all_of(c(groupbyvars, varstodenorm, acrossvar, acrossvarlabel)))

  if (nrow(df_check_1)) {
    print("RTERROR: RU_DENORM: Duplicated records are found for each denominazation group")
    print(df_check_1)
    return()
  }


  for (i in 1:length(acrossvar)) {
    if (i > length(acrossvarlabel)) acrossvarlabel <- c(acrossvarlabel, acrossvar[i])
  }

  var_thisacrossvar <- df_dsetin %>% dplyr::select(!!! rlang::syms(acrossvar)) %>% dplyr::distinct() %>% dplyr::arrange(!!! rlang::syms(acrossvar)) %>% as.data.frame()
  df_thisacrossvarlabel <- df_dsetin %>% dplyr::select(!!! rlang::syms(c(acrossvar, acrossvarlabel))) %>% dplyr::distinct() %>%
    dplyr::arrange(!!! rlang::syms(c(acrossvar, acrossvarlabel))) %>%
    dplyr::mutate(name__ := base::paste(!!! glue::trim(rlang::syms(acrossvarlabel)), sep="~")) %>% dplyr::select("name__")

  # print(var_thisacrossvar)
  for (i in 1:ncol(var_thisacrossvar)) {
    if (is.numeric(var_thisacrossvar[[i]])) this_len <- base::nchar(base::max(var_thisacrossvar[[i]], na.rm=TRUE))
    else this_len <- 0
    if (is.na(this_len)) this_len <- 0

    if (i == 1) n_acrossvarlen <- this_len
    else n_acrossvarlen <- c(n_acrossvarlen, this_len)
  }

  for (i in 1:nrow(var_thisacrossvar)) {
    for (j in 1:ncol(var_thisacrossvar)) {
      if (n_acrossvarlen[j] > 0 & typeof(var_thisacrossvar[i, j]) %in% c("double", "integer")) this_name <- base::sprintf(paste0("%.", n_acrossvarlen[j], "d"), var_thisacrossvar[i, j])
      else this_name <- unlist(var_thisacrossvar[i, j])

      if (j == 1) this_acrossvarname <- this_name
      else this_acrossvarname <- paste(this_acrossvarname, this_name, sep="_")
    }
    if (i == 1) s.acrossvarname <- this_acrossvarname
    else s.acrossvarname <- c(s.acrossvarname, this_acrossvarname)
  }

  df_d1 <- NULL
  newvarlabels <- list()
  for (i in 1:nrow(var_thisacrossvar)) {
    df_thisdset_1 <- as.data.frame(var_thisacrossvar[i, ])
    names(df_thisdset_1) <- names(var_thisacrossvar)
    df_thisdset <- base::merge(df_dsetin, df_thisdset_1, by=c(acrossvar), x.all=FALSE, y.all=FALSE)
    str_thisname <- s.acrossvarname[i]
    var_allvars <- groupbyvars
    for (j in 1:length(varstodenorm)) {
      if (length(acrossvarprefix) >= j) str_thisname_1 <- paste0(acrossvarprefix[j], str_thisname)
      else str_thisname_1 <- paste0(varstodenorm[j], str_thisname)
      if (length(acrossvarsuffix) >= j) str_thisname_1 <- paste0(str_thisname_1, acrossvarsuffix[j])
      df_thisdset <- df_thisdset %>% dplyr::mutate(!! str_thisname_1 := base::get({varstodenorm[j]}))
      newvarlabels[[str_thisname_1]] <- unlist(df_thisacrossvarlabel[i, ])
      var_allvars <- c(var_allvars, str_thisname_1)
    }

    df_thisdset <- df_thisdset %>% dplyr::select(dplyr::all_of(var_allvars))

    if (is.null(df_d1)) df_d1 <- df_thisdset
    else df_d1 <- base:: merge(df_d1, df_thisdset, by=c(groupbyvars), all.x=TRUE, all.y=TRUE)
  }

  df_d1 <- ru_labels(df_d1, newvarlabels)
  #df_d1 <- ru_labels(df_d1, base::labels(dsetin))
  df_d1 <- ru_labels(df_d1, lapply(dsetin,function(x){attr(x,"label")}))

  df_dsetout <- as.data.frame(df_d1)
  #print(paste0("RU_DENORM: ", "End of RU_DENORM"))
  return(df_dsetout)
}
