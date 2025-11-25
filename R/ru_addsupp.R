#' Cobmine an SDTM domain with its corresponding Supplemental data set
#'
#' Pass in an SDTM data frame along with its Supplemental version and they will be combined.
#'
#' @param dsetin Incoming data frame to have supplemental data added.
#' @param dsetinsupp The supplemental data set to add.
#'
#' @return The original SDTM data set with its supplemental data appended as new variables.
#'
#' @author Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com} \cr
#'         Chris Rook, \email{cr883296@gmail.com}
#'
#' @examples
#' library(repfun)
#' library(knitr)
#' ae <- repfun::ae
#' suppae <- repfun::suppae
#' aesupp <- repfun::ru_addsupp(dsetin=ae,dsetinsupp=suppae)
#' knitr::kable(head(aesupp,5), caption = "SDTM.AE combined with SDTM.SUPPAE")
#'
#' @export
#'
ru_addsupp <- function(dsetin, dsetinsupp) {
  #message("RU_ADDSUPP: Start of RU_ADDSUPP")
  # Get unique QNAM values
  s_qnames <- unique(dsetinsupp$QNAM)
  d_dsetout <- dsetin

  s_label_list <- list()
  for (qname in s_qnames) {
    # Filter and reshape supp dataset
    d_tsupp <- dsetinsupp %>%
      dplyr::filter(QNAM == qname) %>%
      dplyr::select(STUDYID, USUBJID, IDVAR, IDVARVAL, QLABEL, QNAM, QVAL) %>%
      tidyr::pivot_wider(names_from = QNAM, values_from = QVAL)

    s_label_list[[qname]] <- unlist(unique(d_tsupp[["QLABEL"]]))[1]
    for (idvar in unique(d_tsupp$IDVAR)) {
      this_byvars <- c("STUDYID", "USUBJID")
      if (!is.na(idvar) && idvar != "") {
        this_byvars <- c(this_byvars, "IDVARVAL")
        d_dsetout <- d_dsetout %>%
          dplyr::mutate(IDVARVAL = stringr::str_trim(as.character(.data[[idvar]])))  # Assign corresponding IDVARVAL
        d_suppid <- dplyr::filter(d_tsupp, IDVAR == idvar) %>%
          dplyr::select(-dplyr::all_of(c("IDVAR", "QLABEL")))
      } else {
        d_suppid <- dplyr::filter(d_tsupp, is.na(IDVAR) | IDVAR == "") %>%
          dplyr::select(-dplyr::all_of(c("IDVAR", "QLABEL")))
      }

      # Check for duplicate rows
      d_dup_rows <- d_suppid %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(this_byvars))) %>%
        dplyr::filter(dplyr::n() > 1) %>%
        dplyr::ungroup()

      if (nrow(d_dup_rows) > 0) {
        message(paste0("Duplicate rows found for QNAM=", qname, " and IDVAR=", idvar))
        print(d_dup_rows[this_byvars])
      }
      # Merge with main dataset
      d_dsetout <- dplyr::left_join(d_dsetout, d_suppid, by = this_byvars) %>%
        dplyr::select(-dplyr::all_of(c("IDVARVAL")))
    }
  }

  # Apply Labels
  d_dsetout <- ru_labels(d_dsetout, s_label_list, style="BASE")

  return(d_dsetout)
}


