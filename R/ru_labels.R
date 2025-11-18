#' Assign labels to variables in a data frame
#'
#' Pass in a data frame along with a named list of columns with their corresponding labels.
#'
#' @param dsetin Incoming data frame to have labels added to columns.
#' @param varlabels List of variables and their labels.
#' @param style Type of method used to add labels.
#'
#' @return The incoming data frame with labels added.
#'
#' @author Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com} \cr
#'         Chris Rook, \email{christopher.x.rook@gsk.com}
#'
#' @examples
#' library(repfun)
#' ru_labels(mtcars,varlabels=list(mpg='Miles per gallon',
#'                                 cyl='Number of cylinders'))
#'
#' @importFrom Hmisc label
#' @export
#'
ru_labels <- function (dsetin, varlabels=list(), style=c("base", "Hmisc")) {
  # print(paste0("RU_LABESL: ", "Start or RU_LABELS"))
  df_out <- dsetin
  v_var_labels <- list()
  #var_ori_labels <- base::labels(dsetin)
  var_ori_labels <- lapply(dsetin,function(x){attr(x,"label")})
  var_var_names <- base::names(df_out)
  for (i in 1:length(var_var_names)) {
    # If a variable hs both hms and difftime class, only keep hms
    # Keeping both hms and difftime on a time variable crashed after
    # assigning label
    this_class <- class(df_out[[var_var_names[i]]])
    this_strclass <- paste0(this_class, collapse="")
    if (stringr::str_detect(this_strclass, "hms") | stringr::str_detect(this_strclass, "difftime")) {
      for (j in 1:length(this_class)) {
        if (! is.na(this_class[j]) && (paste0(this_class[j], collapse="") == "difftime")) {
          class(df_out[[var_var_names[i]]])[j] <- NA
        }
      }
    }

    if (is.character(varlabels)) str_this_label <- varlabels[i]
    else str_this_label <- unlist(varlabels[var_var_names[i]])

    if (! (is.na(str_this_label) || is.null(str_this_label) )) {
      v_var_labels[var_var_names[i]] <- str_this_label
    } else {
      str_this_label <- unlist(var_ori_labels[var_var_names[i]])
      if (! (is.na(str_this_label) || is.null(str_this_label) )) {
        v_var_labels[var_var_names[i]] <- str_this_label
      } else {
        v_var_labels[var_var_names[i]] <- var_var_names[i]
      }
    }
  }

  if (toupper(style[1]) == "BASE") {
    for (i in 1:length(v_var_labels)) {
      this_var_name <- names(v_var_labels)[[i]]
      base::attr(df_out[[this_var_name]], "label") <- v_var_labels[[this_var_name]]
    }
  } else {
    Hmisc::label(df_out) <- v_var_labels
  }
  #print(paste0("RU_LABESL: ", "End of RU_LABELS"))

  return(as.data.frame(df_out))
}
