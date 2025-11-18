#' Append data sets even when variables do not match
#'
#' Pass in a collection of data frames separated by commas and they will be appended.
#'
#' @param ... A collection of data frames.
#' @param keeprownames Convert row names on data frame to columns and keep the column.
#'
#' @return The incoming data frames combined.
#'
#' @author Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com} \cr
#'         Chris Rook, \email{christopher.x.rook@gsk.com}
#'
#' @examples
#' library(repfun)
#' repfun::ru_setdata(mtcars,airquality)
#'
#' @export
#'
ru_setdata <- function (..., keeprownames = TRUE) {
  #print(paste0("RU_SETDATA: ", "Start of RU_SETDATA"))
  #suppressMessages(require(dplyr))
  argg <- c(as.list(environment()), list(...))
  for (i in 2:length(argg)) {
    this.data <- argg[[i]] %>% dplyr::mutate(seq___ := !! i, seq2___=dplyr::row_number())
    if (i == 2) {d.out <-this.data
    } else {
      names.1 <- names(this.data)
      names.2 <- names(d.out)
      names.c <- dplyr::intersect(names.1, names.2)
      if (keeprownames) names.c <- c("row.names", names.c)
      d.out <- merge(x = d.out, y = this.data, by = names.c, all.x = TRUE, all.y=TRUE)
    }
  }
  d.out <- d.out %>% dplyr::arrange(seq___, seq2___) %>% dplyr::select(-c(seq___, seq2___))
  as.data.frame(d.out)
}
