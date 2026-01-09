test_that("expanding variable list works", {

  suppressMessages(library(repfun))
  suppressMessages(library(testthat))

  #========
  # Test 1
  #========
  df <- data.frame(tt_ac01=c('1','2','3'),
                   tt_ac02=c('a','b','b'),
                   tt_ac03=c('10','11','12'))
  prod <- repfun::ru_expvarlist(df, varsin="tt_ac:")
  qc <- c('tt_ac01','tt_ac02','tt_ac03')
  testthat::expect_equal(prod, qc)

  #========
  # Test 2
  #========
  df <- data.frame(tt_ac01=c('1','2','3'),
                   tt_ac03=c('10','11','12'))
  prod <- repfun::ru_expvarlist(df, varsin="tt_ac:")
  qc <- c('tt_ac01','tt_ac03')
  testthat::expect_equal(prod, qc)

  #========
  # Test 3
  #========
  df <- data.frame(tt_ac01=c('1','2','3'),
                   tt_ac03=c('10','11','12'))
  prod <- repfun::ru_expvarlist(df, varsin="tt_ac01-tt_ac10")
  qc <- c('tt_ac01','tt_ac03')
  testthat::expect_equal(prod, qc)

})
