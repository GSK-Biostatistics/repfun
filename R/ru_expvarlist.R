#' Expand SAS Style Variable/Column List
#'
#' Pass in a data frame along with column/variable identifiers formatted with SAS Style (i.e., using colon) and it will be expanded to the actual variable list.
#'
#' @param dsetin A dataframe holding columns whose names will be expanded.
#' @param varsin A SAS style list of variable names.
#' @param keepnotexist If the variable does not exist on the dataframe it will be excluded from the list.
#'
#' @return A list of column/variable names.
#'
#' @author Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com} \cr
#'         Chris Rook, \email{christopher.x.rook@gsk.com}
#'
#' @examples
#' library(repfun)
#' df <- data.frame(tt_ac01=c('1','2','3'),
#'                  tt_ac02=c('a','b','b'),
#'                  tt_ac03=c('10','11','12'))
#' chk <- repfun::ru_expvarlist(df, varsin="tt_ac:")
#' print(chk)
#'
#' @export
#'
ru_expvarlist <- function (dsetin, varsin=NULL, keepnotexist=FALSE) {
  # print(paste0("RU_EXPVARLIST: ", "Start or RU_EXPVARLIST"))
  d.names <- sort(names(dsetin))
  d.cnames <- d.names[sapply(dsetin, is.character)]
  d.nnames <- d.names[sapply(dsetin, is.numeric)]

  rtn <- vector()
  for (this.var in varsin) {
    d.names.1 <- NULL
    if (base::toupper(this.var) == "_NUMERIC_") d.names.1 <- d.nnames
    else if (base::toupper(this.var) == "_CHARACTER_") d.names.1 <- d.cnames
    if (base::toupper(this.var) == "_ALL_") d.names.1 <- d.names
    else if (base::any(grep("-\\w*numeric\\w*-", this.var, ignore.case = TRUE))) {
      s.thisvars <- unlist(base::strsplit(base::gsub("-\\w*numeric\\w*-", ",", this.var), ","))
      d.names.1 <- base::subset(d.nnames, d.nnames <= s.thisvars[2] & d.nnames >= s.thisvars[1])
    } else if (base::any(grep("-\\w*character\\w*-", this.var, ignore.case = TRUE))) {
      s.thisvars <- unlist(base::strsplit(base::gsub("-\\w*character\\w*-", ",", this.var), ","))
      d.names.1 <- base::subset(d.cnames, d.cnames <= s.thisvars[2] & d.cnames >= s.thisvars[1])
    } else if (base::any(grep("-w*-", this.var, ignore.case = TRUE))) {
      s.thisvars <- unlist(base::strsplit(base::gsub("-\\w*-", ",", this.var), ","))
      d.names.1 <- base::subset(d.names, d.names <= s.thisvars[2] & d.names >= s.thisvars[1])
    } else if (base::any(grep("-", this.var, ignore.case = TRUE))) {
      s.thisvars <- unlist(base::strsplit(base::gsub("-", ",", this.var), ","))
      s.prefix.1 <- base::gsub("\\d+$", "", s.thisvars[1])
      s.prefix.2 <- base::gsub("\\d+$", "", s.thisvars[2])
      if (s.prefix.1 == s.prefix.2) {
        s.suffix.1 <- base::substring(s.thisvars[1], nchar(s.prefix.1) + 1)
        s.suffix.2 <- base::substring(s.thisvars[2], nchar(s.prefix.2) + 1)
        d.names.1 <- NULL
        d.subnames <- d.names[grep(paste0("^", s.prefix.1, "\\d+$"), d.names, ignore.case = TRUE)]
        if (length(d.subnames) > 0) for (j in 1:length(d.subnames)) {
          s.thissuffix <- base::substring(d.subnames[j], nchar(s.prefix.1) + 1)
          if (as.numeric(s.thissuffix) >= as.numeric(s.suffix.1) && as.numeric(s.thissuffix) <= as.numeric(s.suffix.2)) d.names.1 <- c(d.names.1, d.subnames[j])
        }
      } else if (base::toupper(this.var) %in% base::toupper(d.names)) d.names.1 <- this.var
    } else if (base::any(grep(":$", this.var, ignore.case = TRUE))) {
      s.thisvars <- unlist(base::strsplit(base::gsub(":", ",", this.var), ","))
      d.names.1 <- d.names[grep(paste0("^", s.thisvars), d.names, ignore.case = TRUE)]
    } else if (base::toupper(this.var) %in% base::toupper(d.names)) {
      d.names.1 <- this.var
    }

    if (keepnotexist && is.null(d.names.1)) d.names.1 <- this.var
    if (length(rtn) < 1) rtn <- d.names.1
    else rtn <- c(rtn, d.names.1)
  }

  return(unlist(rtn))
}
