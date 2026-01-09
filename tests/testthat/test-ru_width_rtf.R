test_that("generating rtf column widths works", {

  suppressMessages(library(repfun))
  suppressMessages(library(dplyr))
  suppressMessages(library(tidyr))
  suppressMessages(library(tibble))
  suppressMessages(library(testthat))

  #========================================================================================================================
  # Create a dataframe where labels and variable value length are identical for each column and sum to 100 across columns.
  #========================================================================================================================
  df <- data.frame(
    V1=strrep('X',50),
    V2=strrep('Y',30),
    V3=strrep('a',5),
    V4=strrep('b',5),
    V5=strrep('c',5),
    V6=strrep('d',5)
  )

  for (i in 1:10){
    df=bind_rows(df,df)
  }

  df <- ru_labels(df,varlabels=list('V1'=strrep('X',50),'V2'=strrep('Y',30),'V3'=strrep('a',5),'V4'=strrep('b',5),'V5'=strrep('c',5),'V6'=strrep('d',5)))

  #========================================
  # Produce widths using package function.
  #========================================
  widths1 <- repfun::ru_width_rtf(df,c('V1','V2','V3','V4','V5','V6'))
  testthat::expect_equal(widths1,c('V1'=50,'V2'=30,'V3'=5,'V4'=5,'V5'=5,'V6'=5))

  #==================================================================
  # Produce widths using package function when 2 are set by default.
  #==================================================================
  widths2 <- repfun::ru_width_rtf(df,c('V1','V2','V3','V4','V5','V6'), widths=list('V1'=40,'V2'=40))
  testthat::expect_equal(widths2,c('V1'=40,'V2'=40,'V3'=5,'V4'=5,'V5'=5,'V6'=5))

})
