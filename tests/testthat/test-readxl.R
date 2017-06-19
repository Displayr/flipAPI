library(testthat)
library(readxl) # use local files rather instead of from the web

context("Parsing Excel files")

test_that("read_xlsx",
{
    num2d <- read_xlsx("xlsxfiles/numeric-with-headers.xlsx")
    expect_equal("data.frame" %in% class(num2d) , TRUE)
})
