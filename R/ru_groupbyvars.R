#' Modify groupbyvars by adding or removing decode vars.
#'
#' Pass in a vector of group-by variables along with a vector of code/decode pairs to have decode variables added or removed.
#'
#' @param groupbyvars Vector of group-by variables.
#' @param codedecodevarpairs Specifies code and decode variable pairs. Those variables should be in parameter GROUPBYVARSNUMER.
#'                           One variable in the pair will contain the code, which is used in counting and ordering, and the other
#'                           will contain decode, which is used for presentation.
#' @param adddecode Add decode variables (true) or remove (false).
#'
#' @return A data frame based on the incoming data frame but collapsed by groups with counts and percents added.
#'
#' @author Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com} \cr
#'         Chris Rook, \email{christopher.x.rook@gsk.com}
#'
#' @examples
#' library(repfun)
#' add_decode <- repfun::ru_groupbyvars(
#'               c("TRTCD", "TRTGRP", "ATOXGRN", "AEDECOD", "AEBODSYS"),
#'               c("TRTCD", "TRTGRP", "ATOXGRN", "ATOXGR"), TRUE)
#' rem_decode <- repfun::ru_groupbyvars(
#'               c("TRTCD", "TRTGRP", "ATOXGRN", "AEDECOD", "AEBODSYS"),
#'               c("TRTCD", "TRTGRP", "ATOXGRN", "ATOXGR"), FALSE)
#'
#' @export
#'
ru_groupbyvars <- function(groupbyvars, codedecodevarpairs, adddecode=TRUE) {
  #print(paste0("RU_GROUPBYVARS: ", "Start off RU_GROUPBYVARS"))

  s.groupbyvars1 <- NULL
  if (adddecode) {
    s.groupbyvars <- ru_groupbyvars(groupbyvars, codedecodevarpairs, adddecode = FALSE)
  } else {s.groupbyvars <- groupbyvars}

  if (! is.null(s.groupbyvars)) for (i in 1:length(s.groupbyvars)) {
    b.add <- TRUE
    s.groupbyvar <- s.groupbyvars[i]
    if (s.groupbyvar %in% codedecodevarpairs) {
      n.index <- which(codedecodevarpairs == s.groupbyvar)
      if (adddecode) {
        if (n.index %% 2 != 0 && n.index + 1 <= length(codedecodevarpairs)) {
          s.groupbyvar <- c(s.groupbyvar, codedecodevarpairs[n.index + 1])
        }
      } else {
        if (n.index %% 2 == 0) b.add <- FALSE
      }
    }
    if (b.add) {
      if (is.null(s.groupbyvars1)) s.groupbyvars1 <- s.groupbyvar
      else s.groupbyvars1 <- c(s.groupbyvars1, s.groupbyvar)
    }
  }
  #print(paste0("RU_GROUPBYVARS: ", "End off RU_GROUPBYVARS"))
  s.groupbyvars1
}
