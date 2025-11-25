#' Fill missing code/decode records
#'
#' Pass in a data frame with along code/decode variables and values to have missings populated.
#'
#' @param dsetin The data set that will be counted to generate numerators for counts and percents.
#' @param codedecodevarpairs Specifies code and decode variable pairs. Those variables should be in parameter GROUPBYVARSNUMER.
#'                           One variable in the pair will contain the code, which is used in counting and ordering, and the other
#'                           will contain decode, which is used for presentation.
#' @param varcodelistpairs List of code/decode pairs of variables.
#' @param codelistnames List of decodes for use with decoding code/decode pairs.
#' @param groupbyvars Set of by-variables used to merge the incoming data set with the decode data set.
#' @param completetypes Keep all code/decode pairs even it not present on the incoming data set?
#'
#' @return A data frame based on the incoming data frame but with decode values added along with records when completetypes is true.
#'
#' @author Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com} \cr
#'         Chris Rook, \email{cr883296@gmail.com}
#'
#' @examples
#' library(repfun)
#' fmtlist <- list('SEXS'=list('START'=list('M','F'),
#'                 'LABEL'=c('Male','Female')))
#' adsl <- repfun::adsl
#' adsl2 <- repfun::ru_fillcodedcode(adsl, codedecodevarpairs=c("SEX", "SEXDCD"),
#'                           varcodelistpairs=c("SEX", "SEXS"),
#'                           codelistnames=fmtlist)
#' unique(adsl2[c("SEX","SEXDCD")])
#'
#' @export
#'
ru_fillcodedcode <- function (dsetin,
                              codedecodevarpairs=NULL,
                              varcodelistpairs=NULL,
                              codelistnames=list(),
                              groupbyvars=NULL,
                              completetypes=TRUE) {
  if (rfenv$G_DEBUG>0) {print(paste0("RU_FILLCODEDECODE: ", "Start of RU_FILLCODEDECODE"))}

  if (nrow(dsetin) < 1) return(as.data.frame(dsetin))
  df_out <- dsetin

  if (is.character(codedecodevarpairs) && all(grepl("^ *$", codedecodevarpairs))) codedecodevarpairs <- NULL
  if (is.character(varcodelistpairs) && all(grepl("^ *$", varcodelistpairs))) varcodelistpairs <- NULL
  if (is.character(groupbyvars) && all(grepl("^ *$", groupbyvars))) groupbyvars <- NULL

  var_dvarnames <- names(dsetin)

  bln_CheckParam <- FALSE

  if (is.null(varcodelistpairs)) {
    if (rfenv$G_DEBUG>0) {message(paste0("RTN", "OTE: RU_FILLCODEDECODE: VARCODELISTPAIRS is NULL"))}
    bln_CheckParam <- TRUE
  }

  if (is.null(codedecodevarpairs)) {
    if (rfenv$G_DEBUG>0) {message(paste0("RTE", "NOTE: RU_FILLCODEDECODE: Given CODEDECODEVARPAIRS is NULL"))}
    bln_CheckParam <- TRUE
  }

  if (length(varcodelistpairs) > 0 && (length(varcodelistpairs) %% 2 != 0)) {
    if (rfenv$G_DEBUG>0) {message(paste0("RTE", "RROR: RU_FILLCODEDECODE: Given VARCODELISTPAIRS are not in pairs"))}
    bln_CheckParam <- TRUE
  }

  if (length(codedecodevarpairs) > 0 && (length(codedecodevarpairs) %% 2 != 0)) {
    if (rfenv$G_DEBUG>0) {message(paste0("RTE", "RROR: RU_FILLCODEDECODE: Given CODEDECODEVARPAIRS are not in pairs"))}
    bln_CheckParam <- TRUE
  }

  if (length(varcodelistpairs) > 0 && is.null(codelistnames)) {
    if (rfenv$G_DEBUG>0) {message(paste0("RTN", "OTE: RU_FILLCODEDECODE: VARCODELISTPAIRS are given, but CODELISTNAMES are not"))}
    bln_CheckParam <- TRUE
  }

  if (bln_CheckParam && is.null(codedecodevarpairs)) return(as.data.frame(dsetin))

  if (length(codedecodevarpairs) > 0) for (i in 1:(length(codedecodevarpairs)/2)) {
    var_codevarname <- codedecodevarpairs[i * 2 - 1]
    var_decodevarname <- codedecodevarpairs[i * 2]

    var_this_codelistname <- ""
    if (length(varcodelistpairs) > 0) for (j in 1:(length(varcodelistpairs)/2)) {
      var_varname <- varcodelistpairs[j * 2 - 1]
      var_codelistname <- varcodelistpairs[j * 2]
      if (toupper(var_codevarname) == toupper(var_varname)) {
        var_this_codelistname <- toupper(var_codelistname)
        break
      }
    }

    if  (! (var_codevarname %in% names(dsetin))) {
      if (rfenv$G_DEBUG>0) {message(paste0("RTN", "OTE: RU_FILLCODEDECODE: Code variable ", var_codevarname, " given in CODEDECODEVARPAIRS is not in DSETIN"))}
      bln_CheckParam <- TRUE
    }

    if  (var_this_codelistname == "" && ! (var_decodevarname %in% names(dsetin))) {
      if (rfenv$G_DEBUG>0) {message(paste0("RTN", "OTE: RU_FILLCODEDECODE: Decode variable ", var_decodevarname, " given in CODEDECODEVARPAIRS is not in DSETIN and CODE variable is not in VARCODELISTPAIRS"))}
      bln_CheckParam <- TRUE
    }

    if (var_this_codelistname != "" && (length(codelistnames) == 0 || ! (var_this_codelistname %in% toupper(names(codelistnames))))) {
      if (rfenv$G_DEBUG>0) {message(paste0("RTN", "OTE: RU_FILLCODEDECODE: Code", var_this_codelistname, " given in VARCODELISTPAIRS is not in CODELISTNAMES"))}
      bln_CheckParam <- TRUE
    }
  }

  if (bln_CheckParam && is.null(codedecodevarpairs)) return(as.data.frame(dsetin))

  l_codelistnames <- codelistnames
  if (length(l_codelistnames) > 0) {
    names(l_codelistnames) <- toupper(names(codelistnames))
    var_codelistnames <- toupper(names(l_codelistnames))
  }

  str_groupbyvars <- NULL
  str_groupbydecodevars <- NULL
  if (length(groupbyvars) > 0) for (i in 1:(length(groupbyvars))) {
    b.include.group <- TRUE
    if (length(codedecodevarpairs) > 0) for (j in 1:(length(codedecodevarpairs)/2)) {
      var_codevarname <- codedecodevarpairs[j*2 - 1]
      var_decodevarname <- codedecodevarpairs[j*2]
      if (toupper(groupbyvars[i]) == toupper(var_codevarname) || toupper(groupbyvars[i]) == toupper(var_decodevarname)) {
        b.include.group <- FALSE
        break
      }
    }
    if (b.include.group) {
      if (length(str_groupbyvars) == 0) str_groupbyvars <- groupbyvars[i] else
        str_groupbyvars <- c(str_groupbyvars, groupbyvars[i])
    }
  }

  if (length(codedecodevarpairs) > 0) {
    if (is.null(str_groupbyvars)) df_decode <- NULL else
      df_decode <- df_out %>% dplyr::select(dplyr::all_of(str_groupbyvars)) %>% dplyr::distinct()

    for (i in 1:(length(codedecodevarpairs)/2)) {
      var_codevarname <- codedecodevarpairs[i * 2 - 1]
      var_decodevarname <- codedecodevarpairs[i * 2]
      var_this_codelistname <- ""
      if (var_codevarname == var_decodevarname) bln_nodecode <- TRUE
      else bln_nodecode <- FALSE

      if (! (base::toupper(var_codevarname) %in% base::toupper(var_dvarnames) )) next

      if (length(varcodelistpairs) > 0) for (j in 1:(length(varcodelistpairs)/2)) {
        var_varname <- varcodelistpairs[j * 2 - 1]
        var_codelistname <- varcodelistpairs[j * 2]
        if (toupper(var_codevarname) == toupper(var_varname)) {
          var_this_codelistname <- toupper(var_codelistname)
          break
        }
      }
      for (j in 1:length(var_dvarnames)) {
        if (base::toupper(var_dvarnames[j]) == var_codevarname) var_codevarname <- var_dvarnames[j]
        if (base::toupper(var_dvarnames[j]) == var_decodevarname) var_decodevarname <- var_dvarnames[j]
      }

      if (var_decodevarname %in% names(dsetin)) df_decode_1 <- dsetin %>% dplyr::select(dplyr::all_of(c(var_codevarname, var_decodevarname))) %>% dplyr::distinct() else
        df_decode_1 <- dsetin %>% dplyr::select(dplyr::all_of(c(var_codevarname))) %>% dplyr::distinct()

      if (length(l_codelistnames) == 0 || var_this_codelistname == "") {
        if (rfenv$G_DEBUG>0) {print(paste0("RU_FILLCODEDECODE: ", "Get Code-Decode Var Pair Values from Data. ", var_codevarname, "-", var_decodevarname))}
        if (var_decodevarname != var_codevarname) str_groupbydecodevars <- c(str_groupbydecodevars, var_decodevarname)
      } else {
        l_codelistname <- l_codelistnames[[var_this_codelistname]]
        df_decode_2 <- base::as.data.frame(utils::unstack(utils::stack(l_codelistnames[[var_this_codelistname]])))
        if (bln_nodecode) var_decodevarname <- paste0(var_decodevarname, "__")
        names(df_decode_2) <- c(var_codevarname, var_decodevarname)
        df_decode_1 <- merge(x=df_decode_1, y=df_decode_2, by=var_codevarname, all.x = TRUE, all.y=TRUE)

        if (bln_nodecode ) {
          df_decode_1 <- df_decode_1 %>% dplyr::mutate(!! rlang::sym(var_decodevarname) := ifelse(is.na(!! rlang::sym(var_decodevarname)), !! rlang::sym(var_codevarname), !! rlang::sym(var_decodevarname)))
        } else if (var_decodevarname %in% var_dvarnames ) {
          var_x_decodevarname <- paste0(var_decodevarname, ".x")
          var_y_decodevarname <- paste0(var_decodevarname, ".y")
          df_decode_1 <- df_decode_1 %>% dplyr::mutate(!! rlang::sym(var_decodevarname) := ifelse(is.na(!! rlang::sym(var_y_decodevarname)), !! rlang::sym(var_x_decodevarname), !! rlang::sym(var_y_decodevarname)))
        }
        var_dvarnames <- c(var_dvarnames, var_decodevarname)
        if (var_decodevarname %in% names(df_out) && var_decodevarname != var_codevarname) str_groupbydecodevars <- c(str_groupbydecodevars, var_decodevarname)
      }

      if (is.null(df_decode)) df_decode <- df_decode_1 else
        df_decode <- merge(df_decode, df_decode_1, all.x=TRUE, all.y=TRUE)

      str_groupbyvars <- c(str_groupbyvars, var_codevarname)
    }
    if (is.null(df_decode)) {
      df_out <- dsetin
    } else {
      if (! is.null(str_groupbydecodevars)) {df_out <- df_out %>% dplyr::select(-all_of(c(str_groupbydecodevars)))}
      df_out <- merge(x=df_out, y=df_decode, all.x = TRUE, by=str_groupbyvars, all.y=completetypes)
    }
  }
  df_out <- df_out %>% dplyr::select(dplyr::all_of(base::intersect(var_dvarnames, base::names(df_out))))
  #df_out <- ru_labels(df_out, base::labels(dsetin))
  df_out <- ru_labels(df_out, lapply(dsetin,function(x){attr(x,"label")}))

  if (rfenv$G_DEBUG>0) {print(paste0("RU_FILLCODEDECODE: ", "End of RU_FILLCODEDECODE"))}
  return(as.data.frame(df_out))
}
