#' Create Percentage based on Numerator and Denominator Data
#'
#' Pass in a data frame along with identification options and have descriptive statistics derived.
#'
#' @param dsetin The data set that will be counted to generate descriptive statistics.
#' @param dsetindenom Input dataset containing data to be counted to obtain the denominator.
#' @param countdistinctvars Variable(s) that contain values to be counted uniquely within any output grouping.
#' @param groupbyvarsnumer Variables in DSETINNUMER to group the data by when counting to obtain the numerator.
#' @param groupbyvarsdenom Variables in DSETINDENOM to group the data by when counting to obtain the denominator.
#' @param resultstyle The appearance style of the result columns that will be displayed in the report.
#' @param totalforvar Variable for which overall totals are required within all other grouped class variables.
#' @param totalid Value(s) used to populate the variable(s) specified in totalforvar.
#' @param totaldecode Value(s) used to populate the variable(s) of the decode variable(s) of the totalforvar.
#' @param anyeventvars Set of variables for which total rows will be added.
#' @param anyeventvalues Set of text values for total rows generated above.
#' @param codedecodevarpairs Specifies code and decode variable pairs. Those variables should be in parameter GROUPBYVARSNUMER.
#'                           One variable in the pair will contain the code, which is used in counting and ordering, and the other
#'                           will contain decode, which is used for presentation.
#' @param varcodelistpairs List of code/decode pairs of variables.
#' @param codelistnames List of decodes for use with decoding code/decode pairs.
#' @param groupminmaxvar Specify if frequency of each group should be from first or last value of a variable in format MIN(variables).
#' @param resultpctdps The reporting precision for percentages.
#'
#' @return A data frame based on the incoming data frame but collapsed by groups with descriptive statistics added.
#'
#' @author Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com} \cr
#'         Chris Rook, \email{cr883296@gmail.com}
#'
#' @examples

#' library(repfun)
#' library(dplyr)
#' library(tibble)
#' datdir <- file.path(gsub("\\","/",tempdir(),fixed=TRUE),"datdir")
#' dir.create(datdir,showWarnings=FALSE)
#' outdir <- file.path(gsub("\\","/",tempdir(),fixed=TRUE),"outdir")
#' dir.create(outdir,showWarnings=FALSE)
#' fmtdir <- file.path(gsub("\\","/",tempdir(),fixed=TRUE),"fmtdir")
#' dir.create(fmtdir,showWarnings=FALSE)
#' repfun::copydata(datdir)
#' repfun::rs_setup(D_POPDATA=repfun::adsl %>% dplyr::filter(SAFFL =='Y'),
#'                  D_SUBJID=c("STUDYID","USUBJID"),
#'                  R_DICTION=NULL,
#'                  R_OTHERDATA=NULL,
#'                  R_INPUTDATA=NULL,
#'                  R_RAWDATA=NULL,
#'                  R_SDTMDATA=NULL,
#'                  R_ADAMDATA=datdir)
#' G_POPDATA <- repfun:::rfenv$G_POPDATA %>%
#'   dplyr::mutate(TRT01AN=ifelse(TRT01A=='Placebo',1,
#'                  ifelse(TRT01A=='Xanomeline Low Dose',2,3)))
#' attr(G_POPDATA$TRT01AN,"label") <- 'Actual Treatment for Period 01 (n)'
#' adae <- repfun:::rfenv$adamdata$adae.rda() %>%
#'   dplyr::inner_join(G_POPDATA, by=c('STUDYID','USUBJID','SAFFL','TRT01A'))
#' aesum <- repfun::ru_freq(adae,
#'                  dsetindenom=G_POPDATA,
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
#'                  resultpctdps=0)
#'
#' @export
#'
ru_freq <- function (dsetin, dsetindenom=NULL, countdistinctvars=NULL, groupbyvarsnumer=NULL,
                     groupbyvarsdenom=NULL, resultstyle="NUMERPCT", totalforvar=NULL, totalid=NULL, totaldecode=c('Total'),
                     anyeventvars=NULL, anyeventvalues=NULL,
                     codedecodevarpairs=NULL, varcodelistpairs=NULL, codelistnames=list(),
                     groupminmaxvar=NULL, resultpctdps=0) {

  #print(paste0("RU_FREQ: ", "Start of RU_FREQ"))

  resultvarname <- "tt_result"
  resultstyle <- toupper(resultstyle)

  groupbyvarsnumer <- ru_groupbyvars(groupbyvarsnumer, codedecodevarpairs, adddecode=TRUE)
  groupbyvarsdenom <- ru_groupbyvars(groupbyvarsdenom, codedecodevarpairs, adddecode=TRUE)

  if (is.null(dsetindenom)) {this_dsetindenom <- dsetin
  } else {this_dsetindenom <- dsetindenom}

  str_groupbyvarsnumer <- base::intersect(groupbyvarsnumer, names(dsetin))
  str_groupbyvarsdenom <- base::intersect(groupbyvarsdenom, names(this_dsetindenom))

  if (! is.null(anyeventvars)) {
    anyeventvars <-base::intersect(str_groupbyvarsnumer, anyeventvars)
  }

  #print(paste0("RU_FREQ: ", "Check GROUPBYVARSDENOM"))
  if (is.null(str_groupbyvarsdenom) || str_groupbyvarsdenom[1] == "") {
    if (is.null(countdistinctvars) || countdistinctvars[1] == "") {v_vars2 <- ""}
    else {v_vars2 <- c(countdistinctvars)}
  } else {
    if (is.null(countdistinctvars) || countdistinctvars[1] == "") { v_vars2 <- unique(c(str_groupbyvarsdenom))}
    else {v_vars2 <- unique(c(countdistinctvars, str_groupbyvarsdenom))}

    groupbyvarsnumer <- unique(c(groupbyvarsnumer, groupbyvarsdenom))
    str_groupbyvarsnumer <- unique(c(str_groupbyvarsnumer, str_groupbyvarsdenom))
  }
  v_vars2 <- unlist(v_vars2)

  #print(paste0("RU_FREQ: ", "Calculate Denorm"))
  if (is.null(countdistinctvars) || countdistinctvars[1] == "") {
    df_sub_2 <- this_dsetindenom
  } else {
    df_sub_2 <- this_dsetindenom %>% dplyr::select(dplyr::all_of(v_vars2)) %>% dplyr::distinct()
  }

  df_count_2 <- ru_sumstats(df_sub_2, analysisvars=NULL, analysisvarlabels="", groupbyvars=str_groupbyvarsdenom, statslist=c("n"),
                            statsinrowsyn="N", analysisvardps=0, statsdps=NULL, codedecodevarpairs=codedecodevarpairs,
                            varcodelistpairs=varcodelistpairs, codelistnames=codelistnames, totalforvar=totalforvar, totalid=totalid,
                            totaldecode=totaldecode) %>% dplyr::select(dplyr::all_of(c(groupbyvarsdenom, "n"))[!grepl("^\\s*$", c(groupbyvarsdenom, "n"))]) %>%
    dplyr::rename(DENOMCNT=n)
  if (is.null(str_groupbyvarsnumer) || str_groupbyvarsnumer[1] == "") {
    if (is.null(countdistinctvars) || countdistinctvars[1] == "") { v_vars1 <- ""}
    else {v_vars1 <- c(countdistinctvars) }
  } else {
    if (is.null(countdistinctvars) || countdistinctvars[1] == "") {v_vars1 <- unique(c(str_groupbyvarsnumer))}
    else {v_vars1 <- unique(c(countdistinctvars, str_groupbyvarsnumer))}
  }
  v_vars1 <- unlist(v_vars1)

  df_count_1 <- NULL
  df_dsetin <- dsetin
  k <- 0
  while (TRUE) {
    k <- k + 1
    if (is.null(groupminmaxvar) || groupminmaxvar[1] == "") {
      v_vars3 <- v_vars1
    } else {
      str_minmaxvar <- unlist(base::strsplit(groupminmaxvar, "[()]"))
      v_vars3 <- base::setdiff(v_vars1, ru_groupbyvars(str_minmaxvar[2], codedecodevarpairs, adddecode=TRUE))
    }
    if (v_vars3[1] == "") {
      if (is.null(groupminmaxvar) || groupminmaxvar[1] == "") {
        df_sub_1 <- dsetin
      } else {
        if (toupper(str_minmaxvar[1]) == "MIN") {
          df_sub_1 <- df_dsetin %>% 
            dplyr::arrange(!! rlang::sym(str_minmaxvar[2])) %>%
            dplyr::slice_head(n=1) 
        } else {
          df_sub_1 <- df_dsetin %>% 
            dplyr::arrange(!! rlang::sym(str_minmaxvar[2])) %>%
            dplyr::slice_tail(n=1) 
        }
      }
    } else {
      if (is.null(groupminmaxvar) || groupminmaxvar[1] == "") {
        df_sub_1 <- df_dsetin %>% dplyr::select(dplyr::all_of(v_vars1)) %>% dplyr::distinct()
      } else {
        if (toupper(str_minmaxvar[1]) == "MIN") {
          df_sub_1 <- df_dsetin %>% dplyr::select(dplyr::all_of(unique(c(v_vars1, str_minmaxvar[2])))) %>% dplyr::distinct() %>%
            dplyr::group_by(!!! rlang::syms(v_vars3)) %>%
            dplyr::arrange(!! rlang::sym(str_minmaxvar[2]), .by_group = TRUE) %>%
            dplyr::slice_head(n=1) %>%
            dplyr::ungroup()
        } else {
          df_sub_1 <- df_dsetin %>% dplyr::select(dplyr::all_of(unique(c(v_vars1, str_minmaxvar[2])))) %>% dplyr::distinct() %>%
            dplyr::group_by(!!! rlang::syms(v_vars3)) %>%
            dplyr::arrange(!! rlang::sym(str_minmaxvar[2]), .by_group = TRUE) %>%
            dplyr::slice_tail(n=1) %>%
            dplyr::ungroup()
        }
      }
    }

    n_slevels <- length(anyeventvars) - k + 1
    df_count_1_1 <- ru_sumstats(df_sub_1, analysisvars=NULL, analysisvarlabels="", groupbyvars=str_groupbyvarsnumer, statslist=c("n"),
                                statsinrowsyn="N", analysisvardps=0, statsdps=NULL, codedecodevarpairs=codedecodevarpairs,
                                varcodelistpairs=varcodelistpairs, codelistnames=codelistnames, totalforvar=totalforvar, totalid=totalid,
                                totaldecode=totaldecode) %>% dplyr::select(dplyr::all_of(c(groupbyvarsnumer, "n"))[!grepl("^\\s*$", c(groupbyvarsnumer, "n"))]) %>%
      dplyr::rename(NUMERCNT=n) %>% dplyr::mutate(tt_summarylevel := !! n_slevels)

    if (is.null(df_count_1)) {
      df_count_1 <- df_count_1_1
    }
    else {
      df_count_1 <- rbind(df_count_1, df_count_1_1)
    }

    if (k <= length(anyeventvars)) {
      df_dsetin <- df_dsetin %>% dplyr:: mutate(!! anyeventvars[length(anyeventvars) - k + 1] := !! anyeventvalues[length(anyeventvars) - k + 1])
    } else {break}
  }

  df_all <- merge(x = df_count_1, y = df_count_2, by = c(groupbyvarsdenom), all.x = TRUE)

  df_all_2 <- df_all %>% dplyr::mutate(PERCENT=as.numeric(ifelse((! (is.na(DENOMCNT) | is.na(NUMERCNT) | DENOMCNT ==0)), 100 * NUMERCNT / DENOMCNT, NA_real_)))
  if (nrow(df_all_2) < 1) {
    df_all_3 <- df_all_2 %>% dplyr::mutate(tt_result = "")
  } else {
    df_all_3 <- df_all_2 %>% dplyr::mutate(tt_result = dplyr::case_when(
      !! resultstyle == "NUMER" ~ sprintf(base::sprintf("%.0f", NUMERCNT)),
      !! resultstyle == "NUMERPCT" & is.na(PERCENT) ~ sprintf(base::sprintf("%.0f", NUMERCNT)),
      !! resultstyle == "NUMERPCT" & ! is.na(PERCENT) ~ paste0(NUMERCNT, " (", base::sprintf(paste0("%.", resultpctdps, "f"), PERCENT), "%)"),
      !! resultstyle == "NUMERDENOMPCT" & is.na(PERCENT) ~ paste0(NUMERCNT, "/", DENOMCNT),
      !! resultstyle == "NUMERDENOMPCT" & ! is.na(PERCENT) ~ paste0(NUMERCNT, "/", DENOMCNT, " (", base::sprintf(paste0("%.", resultpctdps, "f"), PERCENT), "%)"),
      !! resultstyle == "NUMERDENOM" ~ paste0(NUMERCNT, "/", DENOMCNT),
      !! resultstyle == "PCT" ~ ifelse(is.na(PERCENT), " ", paste0(sprintf(base::sprintf(paste0("%.", resultpctdps, "f"), PERCENT)), "%"))
    )
    )
  }
  newvarlabels <- list("DENOMCNT"="Denominator Count", "NUMERCNT"="Numerator Count", "PERCENT"="Percent",
                       "tt_summarylevel"="Summary Level", "tt_result"="Result")

  df_all_3 <- ru_labels(df_all_3, newvarlabels)
  this_vars <- base::intersect(names(dsetin), names(df_all_3))
  if (length(this_vars) > 0 && ! (this_vars[1] == "")) {
    #this_labels <- base::labels(dsetin[, this_vars])
    this_labels <- lapply(dsetin[, this_vars],function(x){attr(x,"label")})
    df_all_3 <- ru_labels(df_all_3, this_labels)
  }
  #print(paste0("RU_FREQ: ", "End of RU_FREQ"))
  return(df_all_3)
}
