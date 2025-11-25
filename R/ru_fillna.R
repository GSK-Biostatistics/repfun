#' Fill NA values with specified values or zeros/blanks by default.
#'
#' Pass in a data frame along with a vector of variables and a vector of fill values.  (Default fill is 0 for numeric and blank " " for character.)
#'
#' @param dsetin Incoming data frame to have labels added to columns.
#' @param vars Vector of variables to replace NA values.
#' @param fills Vector of fill values.
#'
#' @return The incoming data frame with the requested NA values replaced.
#'
#' @author Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com} \cr
#'         Chris Rook, \email{cr883296@gmail.com}
#'
#' @examples
#' library(repfun)
#' repfun::ru_fillna(airquality, vars=c('Ozone','Solar.R'), fills=c(1111,2222)) %>% head(10)
#'
#' @export
#'
ru_fillna <- function(dsetin, vars = NULL, fills = NULL) {
  # message("RU_FILLNA: Start of RU_FILLNA")
  # If vars is not provided, use all column names
  vnames <- if (is.null(vars) || is.na(vars[1]) || vars[1] == "") names(dsetin) else vars

  # Default fill values
  n.fill <- 0
  s.fill <- " "

  # Iterate over specified columns and replace NAs
  for (j in seq_along(vnames)) {
    col_name <- vnames[j]

    # Skip if column doesn't exist in the dataset
    if (!(col_name %in% names(dsetin))) next

    # Determine the fill value
    fill_value <- if (!is.null(fills) && !is.na(fills[1]) && fills[1] != "" && length(fills) >= j) {
      fills[j]
    } else {
      if (is.numeric(dsetin[[col_name]])) n.fill else s.fill
    }

    # Replace NA values
    if (is.numeric(dsetin[[col_name]])){fill_value <- as.numeric(fill_value)}
    dsetin[[col_name]][is.na(dsetin[[col_name]])] <- fill_value
  }

  return(dsetin)
}
