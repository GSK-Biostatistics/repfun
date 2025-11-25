#' Return a list with codelist, code and label based on input codelist dataset.
#'
#' Pass in a data set from a SAS format catalog (or similar) and have a list returned in the structure of a SAS format for decoding variables.
#'
#' @param dsetin Name of incoming data set structured as a SAS format catalog saved as a data set.
#' @param codelistvarname Name of the variable containing the SAS format or similar.
#' @param codevarname Name of the variable that holds the code value.
#' @param decodevarname Name of the variable that holds the decode value.
#' @param typevarname Type of format (character or numeric).
#'
#' @return A data frame based on the incoming data frame but with decode values added along with records when completetypes is true.
#'
#' @author Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com} \cr
#'         Chris Rook, \email{cr883296@gmail.com}
#'
#' @examples
#' library(repfun)
#' datdir <- file.path(gsub("\\","/",tempdir(),fixed=TRUE),"datdir")
#' dir.create(datdir,showWarnings=FALSE)
#' repfun::copydata(datdir)
#' repfun::rs_setup(R_RFMTDIR=datdir)
#' list <- repfun::ru_data2codelist(repfun:::rfenv$rfmtdata$formats.rda())
#' list$SEXS$START[[1]] # Code value 1
#' list$SEXS$LABEL[[1]] # Decode value 1
#' list$SEXS$START[[2]] # Code value 2
#' list$SEXS$LABEL[[2]] # Decode value 2
#
#' @export
#'
ru_data2codelist <- function (dsetin,
                              codelistvarname="FMTNAME",
                              codevarname="START",
                              decodevarname="LABEL",
                              typevarname="TYPE") {
  #print(paste0("RU_DATA2CODELIST: ", "Start of RU_DATA2CODELIST"))

  # df_dsetin <- data.table::data.table(dsetin)
  # data.table::setkeyv(df_dsetin, codelistvarname)
  df_dsetin <- dsetin

  names(df_dsetin) <- base::toupper(names(dsetin))
  if (! (is.null(typevarname) || typevarname == "")) typevarname <- base::toupper(typevarname)
  if (! (is.null(codelistvarname) || codelistvarname == "")) codelistvarname <- base::toupper(codelistvarname)
  if (! (is.null(codevarname) || codevarname == "")) codevarname <- base::toupper(codevarname)
  if (! (is.null(decodevarname) || decodevarname == "")) decodevarname <- base::toupper(decodevarname)

  if (is.null(typevarname) || typevarname == "") {
    v_types <- "C"
  } else {
    v_types <- base::levels(base::factor(df_dsetin[[typevarname]]))
  }

  l_thiscodelists <- list()
  for (j in 1:length(v_types)) {
    if (is.null(typevarname) || typevarname == "") {
      d_sub_1 <- df_dsetin
    } else {
      df_sub_1 <- subset(df_dsetin, df_dsetin[[typevarname]] == v_types[j])
    }
    v_nlevels <- base::levels(base::factor(df_sub_1[[codelistvarname]]))

    for (i in 1:length(v_nlevels)) {
      df_sub_2 <- subset(df_sub_1, df_sub_1[[codelistvarname]] == v_nlevels[i])

      l_thiscodelist <- list()
      l_thisdecodelist <- list()

      s_var_name <- v_nlevels[i]
      if (v_types[j] == "C") {
        l_thiscodelist <- c(df_sub_2[[codevarname]])
        l_thisdecodelist <- c(df_sub_2[[decodevarname]])
      } else if (v_types[j] == "I") {
        l_thiscodelist <- c(df_sub_2[[codevarname]])
        l_thisdecodelist <- c(stringr::str_trim(df_sub_2[[decodevarname]]))
        s_var_name <- paste0("I", s_var_name)
      } else {
        l_thiscodelist <- c(as.numeric(df_sub_2[[codevarname]]))
        l_thisdecodelist <- c(df_sub_2[[decodevarname]])
      }

      l_thiscodelists[[s_var_name]] <- list("START"=l_thiscodelist, "LABEL"=l_thisdecodelist)
    }
  }
  return(l_thiscodelists)
}
