test_that("adding labels works", {

  suppressMessages(library(repfun))
  suppressMessages(library(testthat))

  df <- repfun::ru_labels(mtcars,list(mpg='Miles per gallon', cyl='Number of cylinders'),style='hmisc')
  testthat::expect_equal(df,repfun::mtcars_w2lbls)
})
