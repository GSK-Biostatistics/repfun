test_that("converting data to codelist works", {

  suppressMessages(library(repfun))
  suppressMessages(library(testthat))

  repfun::setpath(paste0(rfenv$PATH,'/tests/testthat'))

  #==============================================================================
  # Build a codelist style format off of a SAS data set containing a SAS format.
  #==============================================================================
  repfun::rs_setup(R_RFMTDIR=paste0("."))
  list <- repfun::ru_data2codelist(rfenv$rfmtdata$formats(),codelistvarname="FMTNAME",codevarname="START",decodevarname="LABEL",typevarname="TYPE")
  clist <- c(list$SEXS$START[[1]], list$SEXS$LABEL[[1]], list$SEXS$START[[2]], list$SEXS$LABEL[[2]])

  #===================================================
  # Check that the resulting codelist is as expected.
  #===================================================
  testthat::expect_equal(clist,c('F','Female','M','Male'))
})
