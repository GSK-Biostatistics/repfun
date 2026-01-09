test_that("multiplication works", {

  suppressMessages(library(repfun))
  suppressMessages(library(testthat))

  #===================================
  # Add decode variables to the list.
  #===================================
  add_decode <- repfun::ru_groupbyvars(c("TRTCD", "TRTGRP", "ATOXGRN", "AEDECOD", "AEBODSYS"), c("TRTCD", "TRTGRP", "ATOXGRN", "ATOXGR"), TRUE)

  #========================================
  # Remove decode variables from the list.
  #========================================
  rem_decode <- repfun::ru_groupbyvars(c("TRTCD", "TRTGRP", "ATOXGRN", "AEDECOD", "AEBODSYS"), c("TRTCD", "TRTGRP", "ATOXGRN", "ATOXGR"), FALSE)

  #=============================
  # Compare actual vs expected.
  #=============================
  testthat::expect_equal(add_decode, c("TRTCD", "TRTGRP", "ATOXGRN", "ATOXGR", "AEDECOD", "AEBODSYS"))
  testthat::expect_equal(rem_decode, c("TRTCD", "ATOXGRN", "AEDECOD", "AEBODSYS"))
})
