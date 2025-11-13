test_that("adding formatted datetimes works", {

  suppressMessages(library(repfun))
  suppressMessages(library(dplyr))
  suppressMessages(library(testthat))

  repfun::setpath(paste0(rfenv$PATH,'/tests/testthat'))

  #=================================================
  # Read SDTM.AE and add time values to the dates.
  #=================================================
  ae <- repfun::ae

  ae %>% dplyr::mutate(AEDTC=ifelse(nchar(AEDTC)==10,paste0(AEDTC,'T12:30:50'),AEDTC),
                AESTDTC=ifelse(nchar(AESTDTC)==10,paste0(AESTDTC,'T00:00:01'),AESTDTC),
                AEENDTC=ifelse(nchar(AEENDTC)==10,paste0(AEENDTC,'T00:00:02'),AEENDTC)) -> ae2

  #============================================================================================
  # Invoke the function ru_datetime() to create formatted datetime variables from the strings.
  #============================================================================================
  ae3 <- repfun::ru_datetime(ae2)
  ae3 <- ae3[,grepl('(DT$|TM$|DTC$)',names(ae3))]

  #======================================================================================================
  # Convert the new datetime versions to strings and compare back with the original strings for a match.
  #======================================================================================================
  ae3 %>% dplyr::mutate(NEW_AEDTC = gsub(' ','T',trimws(as.character(AEDTM))),
                 NEW_AESTDTC = gsub(' ','T',trimws(as.character(AESTDTM))),
                 NEW_AEENDTC = gsub(' ','T',trimws(as.character(AEENDTM))),
                 AEDTC = ifelse(nchar(AEDTC)<10,NA,AEDTC),
                 AESTDTC = ifelse(nchar(AESTDTC)<10,NA,AESTDTC),
                 AEENDTC = ifelse(nchar(AEENDTC)<10,NA,AEENDTC)) -> ae4

  testthat::expect_equal(c(ae4$AEDTC,ae4$AESTDTC,ae4$AEENDTC),
               c(ae4$NEW_AEDTC,ae4$NEW_AESTDTC,ae4$AEENDTC))
})
