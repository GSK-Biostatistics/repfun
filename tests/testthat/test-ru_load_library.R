test_that("loading multiple libraries works", {

  suppressMessages(library(repfun))
  suppressMessages(library(testthat))

  lbs <- c("stats","dplyr","tidyr")
  trashbin <- lapply(lbs, function(x) if (x %in% .packages()) {detach(paste0('package:',x), character.only=TRUE)})
  suppressMessages(repfun::ru_load_library(lbs))
  loaded <- lapply(lbs,function(x) x %in% loadedNamespaces())

  testthat::expect_equal(all(unlist(loaded)), TRUE)
})
