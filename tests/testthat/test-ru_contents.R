test_that("proc contents works", {

  suppressMessages(library(repfun))
  suppressMessages(library(testthat))

  repfun::setpath(paste0(rfenv$PATH,'/tests/testthat'))

  #=============================================
  # Put contents of mtcars into temporary file.
  #=============================================
  fnam <- paste0(base::tempdir(),"/test-ru_contents.txt")
  sink(fnam)
    repfun::ru_contents(mtcars)
  sink()

  #====================================================
  # Retrieve the number of variables and observations.
  #====================================================
  fstring <- gsub('[ ][ ]*',' ',readChar(fnam, file.info(fnam)$size))
  obs <- as.numeric(gsub('Observations ','',stringr::str_extract(fstring,'Observations [0-9][0-9]*')))
  vrs <- as.numeric(gsub('Variables ','',stringr::str_extract(fstring,'Variables [0-9][0-9]*')))

  #============================
  # Remove the temporary file.
  #============================
  if (file.exists(fnam)) {file.remove(fnam)}

  #===============================================================
  # We know there are 11 variables and 32 observations in mtcars.
  #===============================================================
  testthat::expect_equal(c(vrs,obs), c(11,32))

})
