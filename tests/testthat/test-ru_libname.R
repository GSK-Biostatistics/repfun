test_that("ru_libname works for rds and rda files", {

  suppressMessages(library(repfun))
  suppressMessages(library(dplyr))
  suppressMessages(library(testthat))

  #==================================================
  # Read ADSL RDA file, restrict to SAFETY subjects.
  #==================================================
  adamdata1 <- repfun::ru_libname('.')
  adsl <- adamdata1$adsl.rda() %>% dplyr::filter(SAFFL=='Y') -> adsl1

  #================================================
  # Save it as an RDS file to a temporary folder.
  #================================================
  dir.create(paste0(base::tempdir(),"/tmp"), showWarnings = FALSE)
  filnam <- paste0(base::tempdir(),'/tmp/adsl.rds')
  base::saveRDS(adsl1,filnam)

  #==========================================
  # Read that RDS file in using ru_libnames.
  #==========================================
  adamdata2 <- repfun::ru_libname(paste0(gsub('\\','/',base::tempdir(),fixed=TRUE),"/tmp"))
  adsl2 <- adamdata2$adsl()

  #======================
  # Compare the results.
  #======================
  testthat::expect_equal(adsl1, adsl2)

})
