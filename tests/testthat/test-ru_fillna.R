test_that("filling na values works", {

  suppressMessages(library(repfun))
  suppressMessages(library(testthat))

  df <- repfun::ru_fillna(repfun::airquality_updated, vars=c('Ozone','Solar.R','CharVar1','CharVar2'), fills=c(1111,2222,'AAAA','BBBB'))
  testthat::expect_equal(df,repfun::airquality_4test)
})
