test_that("proc contents works", {

  suppressMessages(library(repfun))
  suppressMessages(library(testthat))

  #=============================================
  # Put contents of mtcars into temporary file.
  #=============================================
  ret <- repfun::ru_contents(mtcars)

  #====================================================
  # Retrieve the number of variables and observations.
  #====================================================
  obs <- as.numeric(gsub('Observations ','',stringr::str_extract(gsub('[ ][ ]*',' ',ret$s_data_info[[6]]),'Observations [0-9][0-9]*')))
  vrs <- as.numeric(gsub('Variables ','',stringr::str_extract(gsub('[ ][ ]*',' ',ret$s_data_info[[7]]),'Variables [0-9][0-9]*')))

  #===============================================================
  # We know there are 11 variables and 32 observations in mtcars.
  #===============================================================
  testthat::expect_equal(c(vrs,obs), c(11,32))

})
