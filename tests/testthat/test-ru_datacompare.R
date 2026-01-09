test_that("proc compare works", {

  suppressMessages(library(repfun))
  suppressMessages(library(dplyr))
  suppressMessages(library(testthat))

  iris1 <- iris %>% dplyr::mutate(ID=dplyr::row_number())
  iris2 <- iris %>% dplyr::mutate(ID=dplyr::row_number()) %>% dplyr::mutate(Petal.Length=Petal.Length+1)
  irisdiffs <- repfun::ru_datacompare(iris1, iris2, idvars=c('Species','ID'))
  testthat::expect_equal(irisdiffs[['Diff_Petal.Length']]$Petal.Length.x, irisdiffs[['Diff_Petal.Length']]$Petal.Length.y-1)
})
