#' Remove floating page numbers from RTF files.
#'
#' Pass in an RTF file to have floating page numbers removed and result saved to the same file.
#'
#' @param infile A path to the RTF file as a string.
#'
#' @return No return value, the file is modified.
#'
#' @author Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com} \cr
#'         Chris Rook, \email{cr883296@gmail.com}
#'
#' @examples
#'\dontrun{
#' library(repfun)
#' repfun::rem_pg_nums('/path/to/an/rtf/file.rtf')
#'}
#' @export
#'
rem_pg_nums <- function(infile){

  #=========================
  # Check that file exists.
  #=========================
  if (file.exists(infile)){

    #===================
    # Read in rtf file.
    #===================
    file_content <- readLines(infile)
    matching_lines_index <- grepl("cellxNaN", file_content)
    line_numbers <- which(matching_lines_index)

    #=================================================================================================
    # For above matchex on NaN text, search 2 rows back, 1 row back, and next row for specific text.
    # This will indicate a floating page number and all 4 rows will be removed from the file.
    # The row numbers to be removed are put into the vector remrows.
    #=================================================================================================
    remrows <- c()
    for (n in line_numbers){
      #================================================
      # Retrieve the prior 2 rows and subsequent rows.
      #================================================
      lag1row <- file_content[n-1]
      lag2row <- file_content[n-2]
      nextrow <- file_content[n+1]

      #==========================================
      # Check for matching text before removing.
      #==========================================
      good2rem <- FALSE
      if (grepl('intbl',lag2row)){
        if (grepl('trowd',lag1row)){
          if (grepl('pard',nextrow)){
            good2rem <- TRUE
          }
        }
      }
      if (good2rem){
        remrows <- c(remrows,n-2,n-1,n,n+1)
      }
    }

    #===================================================================================
    # If floating page numbers have been found remove corresponding rows from the file.
    #===================================================================================
    if (length(remrows)>0){
      file_content_updated <- file_content[-remrows]
      writeLines(file_content_updated, infile)
    }

  }
}
