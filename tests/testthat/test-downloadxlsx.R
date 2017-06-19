library(testthat)

context("Parsing Excel files")

test_that("read_xlsx",
{
    capture.output(numAndFactor <- DownloadXLSX("https://github.com/Displayr/flipAPI/blob/master/tests/testthat/xlsxfiles/numeric-and-factor.xlsx?raw=true"), type="message")
    expect_equal("data.frame" %in% class(numAndFactor) , TRUE)
    expect_equal(colnames(numAndFactor), c("X__1", "Age", "Region"))
    expect_equal(unname(sapply(numAndFactor, class)), c("factor", "numeric", "factor"))
    
    capture.output(numAndStr <- DownloadXLSX("https://github.com/Displayr/flipAPI/blob/master/tests/testthat/xlsxfiles/numeric-and-factor.xlsx?raw=true", want.row.names = T, want.factors = F), type="message")
    expect_equal(colnames(numAndStr), c("Age", "Region"))
    expect_equal(unname(sapply(numAndStr, class)), c("numeric", "character"))
    
    capture.output(num2d <- DownloadXLSX("https://github.com/Displayr/flipAPI/blob/master/tests/testthat/xlsxfiles/numeric-with-headers.xlsx?raw=true", want.row.names = T, want.data.frame = F), type="message")
    expect_equal(class(num2d), "matrix")
    expect_equal(is.numeric(num2d), TRUE)
    expect_equal(dim(num2d), c(10, 6))
    
    capture.output(sheet2 <- DownloadXLSX("https://github.com/Displayr/flipAPI/blob/master/tests/testthat/xlsxfiles/numeric-no-headers.xlsx?raw=true", sheet = 2, want.row.names = F, want.col.names = F, want.data.frame = F), type="message")
    expect_equal(dim(sheet2), c(6, 1))
    expect_equal(is.numeric(sheet2), TRUE)
})
