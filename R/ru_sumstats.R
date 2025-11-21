#' Calculate Descriptive Statistics
#'
#' Pass in a data frame along with identification options and have descriptive statistics derived.
#'
#' @param dsetin The data set that will be ounted to generate descriptive statistics.
#' @param analysisvars The variables to be analysed.
#' @param analysisvarlabels Specify a label statement which will be used to defined labels for statistics analysis variables defined in parameter ANALYSISVARS.
#' @param groupbyvars Specifies the variables whose values define the subgroup combinations for the analysis. The variables can be divided by statements inside
#'                    of ( and ) to represent different levels of subgroup.
#' @param statslist Specifies a list of summary statistics to be produced.
#' @param statsinrowsyn Place resulting descriptive statistics in rows or columns.
#' @param analysisvardps Base precision of descriptive statistics prior to incorporating STATSDPS details.
#' @param statsdps List of additional statistic-specific precision values to add to ANALYSISVARDPS.
#' @param codedecodevarpairs Specifies code and decode variable pairs. Those variables should be in parameter GROUPBYVARSNUMER.
#'                           One variable in the pair will contain the code, which is used in counting and ordering, and the other
#'                           will contain decode, which is used for presentation.
#' @param varcodelistpairs List of code/decode pairs of variables.
#' @param codelistnames List of decodes for use with decoding code/decode pairs.
#' @param totalforvar Variable for which overall totals are required within all other grouped class variables.
#' @param totalid Value(s) used to populate the variable(s) specified in totalforvar.
#' @param totaldecode Value(s) used to populate the variable(s) of the decode variable(s) of the totalforvar.
#'
#' @return A data frame based on the incoming data frame but collapsed by groups with descriptive statistics added.
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
#' rs_setup(D_POPDATA=repfun::adsl %>% dplyr::filter(SAFFL =='Y'),
#'          D_SUBJID=c("STUDYID","USUBJID"),
#'          R_DICTION=NULL,
#'          R_OTHERDATA=NULL,
#'          R_INPUTDATA=NULL,
#'          R_RAWDATA=NULL,
#'          R_SDTMDATA=NULL,
#'          R_ADAMDATA=datdir)
#' G_POPDATA <- repfun:::rfenv$G_POPDATA %>% dplyr::mutate(
#'    TRT01AN=ifelse(TRT01A=='Placebo',1,
#'            ifelse(TRT01A=='Xanomeline Low Dose',2,3))) %>%
#'            ru_labels(varlabels=list('TRT01AN'='Actual Treatment for Period 01 (n)'))
#' ru_sumstats(G_POPDATA,
#'             analysisvars=c("AGE","TRTDURD"),
#'             groupbyvars=c("STUDYID","TRT01AN"),
#'             codedecodevarpairs=c("TRT01AN", "TRT01A"),
#'             totalforvar="TRT01AN", totalid=99,
#'             totaldecode="Total",
#'             statsinrowsyn = "Y",
#'             analysisvardps=list("AGE"=1,"TRTDURD"=2),
#'             statslist=c("n", "mean", "median", "sd", "min", "max")) %>% head(10)
#'
#' @export
#'
ru_sumstats <- function (dsetin, analysisvars=NULL,
                         analysisvarlabels="",
                         groupbyvars=NULL,
                         statslist=c("n", "mean", "median", "min", "max", "sd", "q1", "q3"),
                         statsinrowsyn="N",
                         analysisvardps=0,
                         statsdps=list("mean"=1, "median"=1, "sd"=2, "se"=2),
                         codedecodevarpairs=c(""),
                         varcodelistpairs=c(""),
                         codelistnames=list(),
                         totalforvar=NULL,
                         totalid=NULL,
                         totaldecode=c('Total')
) {

  if (rfenv$G_DEBUG>0) {print(paste0("RU_SUMSTATS: ", "Start or RU_SUMSTATS"))}

  if (is.null(analysisvars)) analysisvars <- NA

  resultvarname <- "tt_result"
  resultvarname_n <- "tt_result_num"
  analysisvarname <- "tt_avnm"
  analysisvarordervarname <- "tt_avid"
  statslistvarname <- "tt_svid"
  statslistvarordervarname <- "tt_svid"

  if (! is.null(groupbyvars) && ! is.null(codedecodevarpairs) && ! codedecodevarpairs[1] == "") {
    groupbyvars <- ru_groupbyvars(groupbyvars, codedecodevarpairs, adddecode=TRUE)
  }

  l.statslabels <- list("n"="n", "mean"="Mean", "median"="Median", "min"="Min", "max"="Max", "sd"="SD", "q1"="Q1", "q3"="Q3", "se"="Standard Error",
                        "lclm"="LCLM", "uclm"="UCLM", "clm"="(LCLM, UCLM)", "meanclm"="Mean (LCLM, UCLM)", "geomean"="Geometric Mean",
                        "selog"="Standard Error (Log)", "sdlog"="SD (Log)",
                        "uclmlog"="LCUM (Log)", "uclm"="UCLM (Log)", "clmlog"="(LCOL, UCLM) (LOG))", "geomeanclm"="Geometric Mean (LCLM, UCLM)",
                        "n.c"="n", "mean.c"="Mean", "median.c"="Median", "min.c"="Min", "max.c"="Max", "sd.c"="SD", "q1.c"="Q1", "q3.c"="Q3", "se.c"="Standard Error",
                        "lclm.c"="LCLM", "uclm.c"="UCLM", "clm.c"="(LCLM, UCLM)", "meanclm.c"="Mean (LCLM, UCLM)", "geomean.c"="Geometric Mean",
                        "selog.c"="Standard Error (Log)", "sdlog.c"="SD (Log)",
                        "uclmlog.c"="LCUM (Log)", "uclm.c"="UCLM (Log)", "clmlog.c"="(LCOL, UCLM) (LOG))", "geomeanclm.c"="Geometric Mean (LCLM, UCLM)"
  )

  l.newvarlabels <- list("tt_avid"="Analysis Variable ID", "tt_avnm"="Analysis Variable Name", "tt_svid"="Statistical Parameter ID",
                         "tt_svnm"="Statistical Parameter Name", "tt_result"="Result", "tt_result_num"="Result (Numeric)")


  str_statsdpsnames <- base::names(statsdps)
  if (length(str_statsdpsnames) > 0 || ! (is.null(str_statsdpsnames) || is.na(str_statsdpsnames[1]))) { base::names(statsdps) <- toupper(base::names(statsdps))}

  str_statslist_u <- toupper(statslist)
  str_ori_statslist <- statslist

  s.logstats <- c("SDLOG", "SELOG", "GEOMEAN", "CVB", "CVBLOG", "LCLMLOG", "UCLMLOG", "GEOMEANCLM", "CLMLOG")

  if ("MEANCLM" %in% str_statslist_u || "CLM" %in% str_statslist_u) {
    if (! ("UCLM" %in% str_statslist_u)) str_statslist_u <- c(str_statslist_u, "UCLM")
    if (! ("LCLM" %in% str_statslist_u)) str_statslist_u <- c(str_statslist_u, "LCLM")
  }
  if ("LCLM" %in% str_statslist_u || "UCLM" %in% str_statslist_u) {
    if (! ("N" %in% str_statslist_u)) str_statslist_u <- c(str_statslist_u, "N")
    if (! ("SD" %in% str_statslist_u)) str_statslist_u <- c(str_statslist_u, "SD")
  }

  if (length(base::intersect(str_statslist_u, s.logstats)) > 0) {
    str_statslist_u <- c(str_statslist_u, "LOGV")
  }

  if ("GEOMEANCLM" %in% str_statslist_u) {
    if (! ("UCLMLOG" %in% str_statslist_u)) str_statslist_u <- c(str_statslist_u, "UCLMLOG")
    if (! ("LCLMLOG" %in% str_statslist_u)) str_statslist_u <- c(str_statslist_u, "LCLMLOG")
    if (! ("GEOMEAN" %in% str_statslist_u)) str_statslist_u <- c(str_statslist_u, "GEOMEAN")
  }
  if ("LCLMLOG" %in% str_statslist_u || "UCLMLOG" %in% str_statslist_u) {
    if (! ("N" %in% str_statslist_u)) str_statslist_u <- c(str_statslist_u, "N")
  }

  df_sumall <- NULL
  for (j in 1:length(analysisvars)) {
    if (length(analysisvardps) == 1) this_analysisvardps <- analysisvardps
    else {
      if (analysisvars[j] %in% names(analysisvardps)) this_analysisvardps <- analysisvardps[[analysisvars[j]]]
      else this_analysisvardps <- 0
    }
    str_statement <- ""
    str_statements <- ""
    str_mutates0 <- ""
    str_mutates1 <- ""
    str_this_mutate <- ""

    if (is.na(analysisvars[j])) {df_dsetin <- dsetin}
    else {df_dsetin <- dsetin %>% dplyr::filter(!is.na(!! rlang::sym(analysisvars[j])))}

    var_this_var <- analysisvars[j]
    if ("LOGV" %in% str_statslist_u) {
      str_mutates0 <- paste0("LOGV_=log(", var_this_var, ")")
    }
    for (i in 1:length(str_statslist_u)) {
      str_statement <- dplyr::case_when (
        str_statslist_u[i] == "N"      ~ paste0("n=n()"),
        str_statslist_u[i] == "MEAN"   ~ paste0("mean=mean(", var_this_var, ", na.rm=TRUE)"),
        str_statslist_u[i] == "MEDIAN" ~ paste0("median=stats::median(", var_this_var, ", na.rm=TRUE)"),
        str_statslist_u[i] == "MIN"    ~ paste0("min=min(", var_this_var, ", na.rm=TRUE)"),
        str_statslist_u[i] == "MAX"    ~ paste0("max=max(", var_this_var, ", na.rm=TRUE)"),
        str_statslist_u[i] == "SD"     ~ paste0("sd=stats::sd(", var_this_var, ", na.rm=TRUE)"),
        str_statslist_u[i] == "SE"     ~ paste0("se=stats::sd(", var_this_var, ", na.rm=TRUE)/n()"),
        str_statslist_u[i] == "Q1"     ~ paste0("q1=quantile(", var_this_var, ", probs=0.25, type=2, na.rm=TRUE)"),
        str_statslist_u[i] == "Q2"     ~ paste0("q2=quantile(", var_this_var, ", probs=0.5, type=2, na.rm=TRUE)"),
        str_statslist_u[i] == "Q3"     ~ paste0("q3=quantile(", var_this_var, ", probs=0.75, type=2, na.rm=TRUE)"),
        str_statslist_u[i] == "LOGV"   ~ paste0("SDLOGV_=sd(LOGV_), GEOMEAN_=mean(LOGV_)"),
        TRUE ~ ""
      )
      if (str_statement == "") {}
      else if (str_statements == "") str_statements <- str_statement
      else str_statements <- paste(str_statements, str_statement, sep=", ")

      str_this_mutate <- dplyr::case_when (
        # stats::qt(0.975,df=n-1)*s/sqrt(n)
        str_statslist_u[i] == "GEOMEAN" ~ paste0("geomean=exp(GEOMEAN_)"),
        str_statslist_u[i] == "SDLOG"  ~ paste0("sdlog=exp(SDLOGV_)"),
        str_statslist_u[i] == "SELOG"   ~ paste0("selog=exp(SDLOGV_/n)"),
        # cvblog=((exp(sdlog ** 2) - 1) ** (1/2)) * 100
        # cvb=(sdlog / meanlog) * 100
        str_statslist_u[i] == "CVBLOG"  ~ paste0("cvblog=(exp((SDLOGV_** 2) - 1) ** (1/2)) * 100,"),
        str_statslist_u[i] == "CVB"     ~ paste0("cvb=SDLOGV_/mean(LOGV_)"),
        TRUE ~ ""
      )
      if (str_this_mutate == "") {}
      else if (str_mutates1 == "") str_mutates1 <- str_this_mutate
      else str_mutates1 <- paste(str_mutates1, str_this_mutate, sep=", ")
      str_this_mutate <- ""

      str_this_mutate <- dplyr::case_when (
        # stats::qt(0.975,df=n-1)*s/sqrt(n)
        str_statslist_u[i] == "LCLM"   ~ paste0("lclm=mean - stats::qt(0.975, df=n - 1) * sd/(n**(1/2))"),
        str_statslist_u[i] == "UCLM"   ~ paste0("uclm=mean + stats::qt(0.975, df=n - 1) * sd/(n**(1/2))"),
        str_statslist_u[i] == "LCLMLOG"   ~ paste0("lclmlog=exp(GEOMEAN_ - stats::qt(0.975, df=n - 1) * SDLOGV_/(n**(1/2)))"),
        str_statslist_u[i] == "UCLMLOG"   ~ paste0("uclmlog=exp(GEOMEAN_ + stats::qt(0.975, df=n - 1) * SDLOGV_/(n**(1/2)))"),
        TRUE ~ ""
      )
      if (str_this_mutate == "") {}
      else if (str_mutates1 == "") str_mutates1 <- str_this_mutate
      else str_mutates1 <- paste(str_mutates1, str_this_mutate, sep=", ")
      str_this_mutate <- ""

      if (! (str_statslist_u[i] %in% c("GEOMEANCLM", "CLM", "LOGV", "CLMLOG"))) {
        str_statsdpsnames <- base::names(statsdps)
        if (length(str_statsdpsnames) < 1 && ((is.null(str_statsdpsnames) || is.na(str_statsdpsnames[1])))) {
          if (length(statsdps) <= j) { str_this_dps <- this_analysisvardps + statsdps[j]} else {str_this_dps <- this_analysisvardps}
        } else if (str_statslist_u[i] %in% str_statsdpsnames) {
          str_this_dps <- base::as.numeric(unlist(statsdps[[str_statslist_u[i]]]))
          if (is.null(str_this_dps) || is.na(str_this_dps[1])) {str_this_dps <- this_analysisvardps} else str_this_dps <- this_analysisvardps + str_this_dps
        } else {str_this_dps <- this_analysisvardps}

        if (str_statslist_u[i] == "N") str_this_dps <- 0
        str_this_mutate <- paste0(tolower(str_statslist_u[i]), ".c=base::sprintf('%.",str_this_dps, "f', round(", tolower(str_statslist_u[i]), ", ", str_this_dps, "))")
        if (str_this_mutate == "") {}
        else if (str_mutates1 == "") str_mutates1 <- str_this_mutate
        else str_mutates1 <- paste(str_mutates1, str_this_mutate, sep=", ")
        str_this_mutate <- ""
      }
    }

    for (i in 1:length(str_statslist_u)) {
      str_this_mutate <- dplyr::case_when (
        str_statslist_u[i] == "CLM"             ~ paste0("clm=paste0('(', lclm.c, ', ', uclm.c, ')'), clm.c=clm"),
        str_statslist_u[i] == "CLMLOG"          ~ paste0("clmlog=paste0('(', lclmlogc, ', ', uclmlog.c, ')'), clmlog.c=clmlog"),
        str_statslist_u[i] == "MEANCLM"         ~ paste0("meanclm=paste0(mean.c, ' (', lclm.c, ', ', uclm.c, ')'), meanclm.c=meanclm"),
        str_statslist_u[i] == "GEOMEANCLM"      ~ paste0("geomeanclm=paste0(geomean.c, ' (', lclmlog.c, ', ', uclmlog.c, ')'), geomeanclm.c=geomeanclm"),
        TRUE ~ ""
      )
      if (str_this_mutate == "") {}
      else if (str_mutates1 == "") str_mutates1 <- str_this_mutate
      else str_mutates1 <- paste(str_mutates1, str_this_mutate, sep=", ")
      str_this_mutate <- ""
    }

    str_statements=paste0("dplyr::summarise(", str_statements, ")")
    str_groupbyvars <- groupbyvars
    str_groupbyvars <- base::intersect(str_groupbyvars, names(dsetin))
    k <- 1
    if (is.null(str_groupbyvars) || str_groupbyvars[1] == "") {
      if (str_mutates0 == "") {
        str_statements_1 <- paste0("df_dsetin %>% ", str_statements, " %>% mutate(", str_mutates1, ")")
      } else {
        str_statements_1 <- paste0("df_dsetin %>% dplyr::mutate(", str_mutates0, ") %>% ", str_statements, " %>% mutate(", str_mutates1, ")")
      }
    } else {
      if (str_mutates0 == "") {
        str_statements_1 <- paste0("df_dsetin %>% group_by( !!! rlang::syms(str_groupbyvars)) %>% ", str_statements, " %>% dplyr::mutate(",  str_mutates1, ") %>% ungroup()")
      } else {
        str_statements_1 <- paste0("df_dsetin %>% dplyr::mutate(", str_mutates0, ") %>% group_by(!!! rlang::syms(str_groupbyvars)) %>% ", str_statements, " %>% dplyr::mutate(",  str_mutates1, ") %>% ungroup()")
      }
    }
    n <- 0
    df_sumall_1 <- NULL
    b.skiptotal <- FALSE
    while(TRUE) {
      if (! b.skiptotal) {
        df_sumall_0 <- base::eval(base::parse(text=str_statements_1))

        if (is.null (df_sumall_1[1])) df_sumall_1 <- df_sumall_0
        else df_sumall_1 <- ru_setdata(df_sumall_1, df_sumall_0, keeprownames = FALSE)
      }

      if (is.null(totalforvar) || totalforvar[1] == "") {
        break
      }  else if (length(totalforvar) < k) {
        break
      } else {
        s.totalforvar <- unlist(base::strsplit(totalforvar[k], "[*]"))
        b.skiptotal <- FALSE

        str_condition <- NULL
        str_groupbyvars <- groupbyvars
        str_groupbyvars <- base::intersect(str_groupbyvars, names(dsetin))

        str_mutates2 <- NULL
        for (l in 1:length(s.totalforvar)) {
          n <- n + 1

          var_idvarname <- unlist(base::strsplit(s.totalforvar[l], "[()]"))
          var_forvar <- var_idvarname[1]
          if (length(var_idvarname) > 1) {
            if (is.null(str_condition) || is.na(str_condition[1])) str_condition <- paste0(var_forvar, " %in% c(", var_idvarname[2], ")")
            else str_condition <- paste0(str_condition, " && ", var_forvar, " %in% c(", var_idvarname[2], ")")
          }
          str_groupbyvars <- base::setdiff(str_groupbyvars, var_forvar)

          if (! (var_forvar %in% names(df_dsetin)) ) {
            b.skiptotal=TRUE
            break
          } else if (is.character(df_dsetin[[var_forvar]])) {
            if (is.null(str_mutates2)) str_mutates2 <- paste0(", ", var_forvar, "='", totalid[n], "'")
            else str_mutates2 <- paste0(str_mutates2, ", ", var_forvar, "='", totalid[n], "'")
          } else  {
            if (is.null(str_mutates2)) str_mutates2 <- paste0(", ", var_forvar, "=", totalid[n])
            else str_mutates2 <- paste0(str_mutates2, ", ", var_forvar, "=", totalid[n])
          }

          var.decodevarname <- NULL
          if (! b.skiptotal) for (m in 1:(length(codedecodevarpairs)/2)) {
            if ( var_forvar == codedecodevarpairs[m*2 - 1] ) {
              var.decodevarname <- codedecodevarpairs[m*2]
              str_groupbyvars <- base::setdiff(str_groupbyvars, var.decodevarname)
              if (is.character(df_dsetin[[var.decodevarname]])) {
                if (is.null(str_mutates2)) str_mutates2 <- paste0(", ", var.decodevarname, "='", totaldecode[n], "'")
                else str_mutates2 <- paste0(str_mutates2, ", ", var.decodevarname, "='", totaldecode[n], "'")
              } else {
                if (is.null(str_mutates2)) str_mutates2 <- paste0(", ", var.decodevarname, "=", totaldecode[n])
                else str_mutates2 <- paste0(str_mutates2, ", ", var.decodevarname, "=", totaldecode[n])
              }
              break
            }
          }
        }
        if (! b.skiptotal) {
          if (is.null(str_condition) || is.na(str_condition[1])) str_statements_1 <- paste0("df_dsetin")
          else str_statements_1 <- paste0("df_dsetin %>% dplyr::filter(", str_condition, ")")

          if (is.null(str_groupbyvars) || length(str_groupbyvars) < 1 || str_groupbyvars[1] == "") {
            str_statements_1 <- paste0(str_statements_1, " %>% dplyr::mutate(", str_mutates0, ") %>% ", str_statements, " %>% mutate(", str_mutates1, str_mutates2, ")")
          } else {
            str_statements_1 <- paste0(str_statements_1, " %>% dplyr::mutate(", str_mutates0, ") %>% group_by(!!! rlang::syms(str_groupbyvars)) %>% ", str_statements,
                                       " %>% dplyr::mutate(",  str_mutates1, str_mutates2, ") %>% ungroup()")
          }
        }
        k <- k+1
      }
    }

    if (rfenv$G_DEBUG>0) {print(paste0("RU_SUMSTATS: ", "Fill missing categories"))}
    if ("n" %in% base::names(df_sumall_1))  {
      #message('MADE IT HERE ... 2')
      df_sumall_1 <- ru_fillcodedcode(df_sumall_1, codedecodevarpairs=codedecodevarpairs, varcodelistpairs=varcodelistpairs,
                                      codelistnames=codelistnames, groupbyvars=groupbyvars)

      df_sumall_1 <- ru_fillna(df_sumall_1, vars=c("n", "n.c"), fills=c(0, "0"))
    }

    str_ori_statslist <- base::tolower(str_ori_statslist)
    str_ori_statslist_c <- ""
    for (i in 1:length(str_ori_statslist)) {
      str_ori_statslist_c[i] <- paste0(str_ori_statslist[i], ".c")
    }

    if (length(analysisvarlabels) >= j && j > 1 ) str_this_label <- analysisvarlabels[j]
    else if (length(analysisvarlabels) == 1 && j == 1 &&! analysisvarlabels[1] == "") {
      str_this_label <- unlist(analysisvarlabels[j])
    } else {
      str_this_label <- unlist(labels(df_dsetin)[var_this_var])
      if (is.null(str_this_label) || is.na(str_this_label[1])) {
        str_this_label <- var_this_var}
    }

    if (toupper(statsinrowsyn) == "Y") {
      if (rfenv$G_DEBUG>0) {print(paste0("RU_SUMSTATS: ", "Transform summary results from column to rows"))}
      for (i in 1:length(str_ori_statslist)) {
        df_sumall_1.1 <- dplyr::mutate(df_sumall_1, tt_svid=!! i, tt_svnm=!!  unlist(l.statslabels[str_ori_statslist[i]]),
                                       tt_result := !! unlist(df_sumall_1[str_ori_statslist_c[i]]), tt_result_num := !! unlist(df_sumall_1[str_ori_statslist[i]]))
        if (is.null(groupbyvars) || is.na(groupbyvars[1]) || groupbyvars[1] == "") {
          df_sumall_1.2 <- dplyr::select(df_sumall_1.1, dplyr::all_of("tt_result", "tt_result_num", "tt_svid", "tt_svnm")) %>% dplyr::mutate(tt_avid=!! j, tt_avnm=!! str_this_label)
        } else {
          df_sumall_1.2 <- dplyr::select(df_sumall_1.1, dplyr::all_of(c(groupbyvars, "tt_result", "tt_result_num", "tt_svid", "tt_svnm"))) %>% dplyr::mutate(tt_avid=!! j, tt_avnm=!! str_this_label)
        }

        if (i == 1) { df_sumall_2 <- df_sumall_1.2}
        else {df_sumall_2 <- rbind(df_sumall_2, df_sumall_1.2)}
      }
    } else {
      if (rfenv$G_DEBUG>0) {print(paste0("RU_SUMSTATS: ", "Add tt_avid and tt_avnm"))}
      if (is.null(groupbyvars) || is.na(groupbyvars[1]) || groupbyvars[1] == "") {
        df_sumall_2 <- dplyr::select(df_sumall_1, dplyr::all_of(c(str_ori_statslist, str_ori_statslist_c))) %>% dplyr::mutate(tt_avid=!! j, tt_avnm=!! str_this_label)
      } else {
        df_sumall_2 <- dplyr::select(df_sumall_1, dplyr::all_of(c(groupbyvars, str_ori_statslist, str_ori_statslist_c))) %>% dplyr::mutate(tt_avid=!! j, tt_avnm=!! str_this_label)
      }
    }
    if (is.null(df_sumall)) df_sumall <- df_sumall_2
    else df_sumall <- base::rbind(df_sumall, df_sumall_2)

    if (toupper(statsinrowsyn) == "Y") {df_sumall <- ru_labels(df_sumall, l.newvarlabels)}
    else {
      df_sumall <- ru_labels(df_sumall, l.statslabels)
      df_sumall <- ru_labels(df_sumall, l.newvarlabels)
    }
  }
  this_vars <- base::intersect(names(dsetin), names(df_sumall))
  if (length(this_vars) > 0 && ! (this_vars[1] == "")) {
    #this_labels <- base::labels(dsetin[, this_vars])
    this_labels <- lapply(dsetin[, this_vars],function(x){attr(x,"label")})
    df_sumall <- ru_labels(df_sumall, this_labels)
  }
  if (rfenv$G_DEBUG>0) {print(paste0("RU_SUMSTATS: ", "End of RU_SUMSTATS"))}
  df_sumall
}
