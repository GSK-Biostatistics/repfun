#' Compare 2 data frames and report differences.
#'
#' Pass in a base and compare data frame to find out if they are equal similar to proc compare in SAS.
#'
#' @param dsetinbase First data set.
#' @param dsetincomp Second data set.
#' @param idvars Match on these values prior to comparing records.
#' @param maxprint Maximum number of differences per variable to display.
#'
#' @return An output similar to proc compare will be displayed.
#'
#' @author Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com} \cr
#'         Chris Rook, \email{cr883296@gmail.com}
#'
#' @examples
#' library(repfun)
#' repfun::ru_datacompare(iris,iris,idvars='Species')
#'
#' @export
#'
ru_datacompare <- function (dsetinbase, dsetincomp, idvars, maxprint=50) {
  #print(paste0("RU_DATACOMPARE: ", "Start of RU_DATACOMPARE"))
  #suppressMessages(require(dplyr))
  rtn <- list()
  # Check if column names match
  col_diff_1 <- dplyr::setdiff(names(dsetinbase), names(dsetincomp))
  if (length(col_diff_1) > 0) {
    cat("Columns in BASE, but not in COMP:\n")
    print(col_diff_1)
  }

  col_diff_2 <- dplyr::setdiff(names(dsetincomp), names(dsetinbase))
  if (length(col_diff_2) > 0) {
    cat("Columns in COMP, but not in BASE:\n")
    print(col_diff_2)
  }

  rtn <- base::append(rtn, list(
    NamesOnlyInBase = col_diff_1,
    NamesOnlyInComp = col_diff_2
  ))

  col_common <- dplyr::intersect(names(dsetincomp), names(dsetinbase))

  # Check if column types match
  type_diff <- sapply(col_common, function(x) {
    if (identical(class(dsetinbase[[x]]), class(dsetincomp[[x]]))) {
      return(NA)
    } else {
      return(paste(class(dsetinbase[[x]]), " vs ", class(dsetincomp[[x]])))
    }
  })

  # Report differing column types
  type_diff <- type_diff[!is.na(type_diff)]
  if (length(type_diff) > 0) {
    cat("\nDifferent column types:\n")
    print(type_diff)
  }

  # Check for factor level differences (if any column is a factor)
  factor_diff <- sapply(col_common, function(x) {
    if (is.factor(dsetinbase[[x]]) && is.factor(dsetincomp[[x]])) {
      factor1 <- levels(dsetinbase[[x]])
      factor2 <- levels(dsetincomp[[x]])
      if (!identical(factor1, factor2)) {
        return(paste("Factor levels differ:", paste(factor1, collapse = ", "),
                     "vs", paste(factor2, collapse = ", ")))
      }
    }
    return(NULL)
  })

  factor_diff <- factor_diff[!sapply(factor_diff, is.null)]
  if (length(factor_diff) > 0) {
    cat("\nDifferent factor levels:\n")
    print(factor_diff)
  }

  rtn <- base::append(rtn, list(
    DiffentTypes = type_diff,
    DifferentAttr = factor_diff
  ))

  col_common <- dplyr::setdiff(col_common, idvars)

  # Sort data frames by key columns
  this_idvars <- unique(c(idvars, col_common))
  dsetinbase.1 <- dsetinbase %>% dplyr::select(dplyr::all_of(this_idvars)) %>% dplyr::arrange(dplyr::across(dplyr::all_of(this_idvars)))
  dsetincomp.1 <- dsetincomp %>% dplyr::select(dplyr::all_of(this_idvars)) %>% dplyr::arrange(dplyr::across(dplyr::all_of(this_idvars)))

  # Find extra rows in dsetinbase not in dsetincomp
  extra_rows_dsetinbase <- dplyr::anti_join(dsetinbase.1, dsetincomp.1, by = this_idvars)

  # Find extra rows in dsetincomp not in dsetinbase
  extra_rows_dsetincomp <- dplyr::anti_join(dsetincomp.1, dsetinbase.1, by = this_idvars)

  # Common rows for comparison
  common_rows <- dplyr::inner_join(dsetinbase.1, dsetincomp.1, by = this_idvars)

  diff_nobs <- list(
    NnubmerOfObsInCommon = nrow(common_rows),
    NnubmerOfObsOnlyInBase = nrow(extra_rows_dsetinbase),
    NnubmerOfObsOnlyInComp = nrow(extra_rows_dsetincomp)
  )

  rtn <- append(rtn, diff_nobs)

  if (is.null(idvars)) {
    dsetinbase.1 <- dsetinbase.1 %>% dplyr::mutate(rowNumber = dplyr::row_number())
    dsetincomp.1 <- dsetincomp.1 %>% dplyr::mutate(rowNumber = dplyr::row_number())
    idvars="rowNumber"
  } else {
    dsetinbase.1 <- dsetinbase.1 %>% dplyr::group_by(!!! idvars) %>%
      dplyr::mutate(rowNumber = dplyr::row_number()) %>% dplyr::ungroup()
    dsetincomp.1 <- dsetincomp.1 %>% dplyr::group_by(!!! idvars) %>%
      dplyr::mutate(rowNumber = dplyr::row_number()) %>% dplyr::ungroup()
    idvars=c(idvars, "rowNumber")
  }


  for (i in seq_along(col_common)) {
    this.var.base <- paste0(col_common[i], ".x")
    this.var.comp <- paste0(col_common[i], ".y")

    this_diff <- base::merge(dsetinbase.1[, c(idvars, col_common[i])], dsetincomp.1[, c(idvars, col_common[i])], by=idvars, all.x=TRUE, all.y=TRUE) %>%
      dplyr::filter( (! (!!rlang::sym(this.var.base) == !!rlang::sym(this.var.comp)) |
                        is.na(!!rlang::sym(this.var.base)) | is.na(!!rlang::sym(this.var.comp))) &
                       ! (is.na(!!rlang::sym(this.var.base)) & is.na(!!rlang::sym(this.var.comp))))

    if (nrow(this_diff) > 0) {
      this_name <- paste0("Diff_", col_common[i])
      rtn[[this_name]] <- this_diff[seq(1, min(nrow(this_diff), maxprint)), ]
    }
  }

  #print(paste0("RU_DATACOMPARE: ", "End of RU_DATACOMPARE"))
  return(rtn)
}
