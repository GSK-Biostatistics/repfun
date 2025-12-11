#' Add numeric datetimes to data frame that only has character versions.
#'
#' Pass in a data frame and variables ending in "DTC" will have corresponding numeric versions created and saved.
#'
#' @param dsetin Incoming data frame to have numeric datetimes added.
#' @param includevars Specify which variables ending in DTC will be processed.
#'
#' @return The incoming data frame with numeric dates and times added will be returned.
#'
#' @author Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com} \cr
#'         Chris Rook, \email{cr883296@gmail.com}
#'
#' @examples
#' library(repfun)
#' library(knitr)
#' ae <- repfun::ru_datetime(repfun::ae)
#' knitr::kable(head(ae[,grepl('(DT$|TM$|DTC$)',names(ae))],5),
#'              caption = "After Invoking ru_labels()")
#'
#' @export
#'
ru_datetime <- function (dsetin, includevars=NULL) {
  #suppressMessages(require(dplyr))
  #print(paste0("RU_DATETIME: ", "Start or RU_DATETIME"))
  s_names <- base::names(dsetin)
  if (is.character(includevars) && all(grepl("^ *$", includevars))) includevars <- NULL
  s_datecolnames <- NULL
  for (i in 1:length(s_names)) {
    if (nchar(s_names[i]) > 3 && base::substring(s_names[i], nchar(s_names[i]) - 2) == "DTC") {
      if (length(includevars) == 0 || (s_names[i] %in% includevars) ) {
        if (is.null(s_datecolnames)) s_datecolnames <- s_names[i]
        else s_datecolnames <- c(s_datecolnames, s_names[i])
      }
    }
  }
  s_statements <- NULL
  d.out <- dsetin
  for (i in 1:length(s_datecolnames)) {
    s_shortname <- base::substring(s_datecolnames[i], 1, nchar(s_datecolnames[i]) - 3)
    s_datevar <- paste0(s_shortname, "DT")
    s_timevar <- paste0(s_shortname, "TM")
    s_datetimevar <- paste0(s_shortname, "DTM")
    s_datecol <- s_datecolnames[i]
    d.out <- d.out %>% dplyr::mutate(
      !!s_datevar := dplyr::case_when(
        is.na(!!sym(s_datecol)) ~ NA,
        nchar(!!sym(s_datecol)) >= 10 ~ data.table::as.IDate(substr(!!sym(s_datecol), 1, 10), format = "%Y-%m-%d"),
        TRUE ~ NA
      ),
      !!s_timevar := dplyr::case_when(
        is.na(!!sym(s_datecol)) ~ NA,
        nchar(!!sym(s_datecol)) >= 19 ~ data.table::as.ITime(substr(!!sym(s_datecol), 12, 19), format = "%H:%M:%S"),
        nchar(!!sym(s_datecol)) >= 16 ~ data.table::as.ITime(substr(!!sym(s_datecol), 12, 16), format = "%H:%M"),
        TRUE ~ NA
      ),
      !!s_datetimevar := dplyr::case_when(
        is.na(!!sym(s_datecol)) ~ NA,
        nchar(!!sym(s_datecol)) >= 19 ~ base::strptime(base::substr(!!sym(s_datecolnames[i]), 1, 19), "%Y-%m-%dT%H:%M:%S"),
        nchar(!!sym(s_datecol)) >= 16 ~ base::strptime(base::substr(!!sym(s_datecolnames[i]), 1, 16), "%Y-%m-%dT%H:%M"),
        TRUE ~ NA
      )
    )
  }
  return(d.out)
}
