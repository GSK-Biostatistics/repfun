test_that("appending data sets works", {

  suppressMessages(library(repfun))
  suppressMessages(library(dplyr))
  suppressMessages(library(testthat))

  repfun::setpath(paste0(rfenv$PATH,'/tests/testthat'))

  #=======================================================
  # Append mtcars and airquality using package function.
  #=======================================================
  fun <- repfun::ru_setdata(mtcars,airquality)
  fun[['Row.names']] <- as.character(fun$Row.names)

  #================================================================================================
  # Use dplyr to append the same data frames and put the row names into the same format as above.
  #================================================================================================
  dpl <- dplyr::bind_rows(mtcars,airquality)
  dpl[['Row.names']] <- rownames(dpl)
  rownames(dpl) <- NULL
  dpl %>% dplyr::mutate(Row.names=trimws(as.character(gsub('...','',Row.names,fixed=TRUE))),
                 Row.names.new=as.character(as.numeric(ifelse(gsub('^[A-Z].*$','',Row.names)=='',NA,gsub('^[A-Z].*$','',Row.names)))-nrow(mtcars)),
                 Row.names=ifelse(!is.na(Row.names.new),Row.names.new,Row.names)) %>%
    dplyr::select(-c(Row.names.new)) %>% dplyr::relocate(Row.names)-> dpl
  testthat::expect_equal(fun,dpl)
})
