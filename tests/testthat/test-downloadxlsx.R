library(testthat)

context("Parsing Excel files")

test_that("read_xlsx",
{
    capture.output(numAndFactor <- DownloadXLSX("https://github.com/Displayr/flipAPI/blob/master/tests/testthat/xlsxfiles/numeric-and-factor.xlsx?raw=true", want.data.frame = TRUE), type="message")
    expect_equal("data.frame" %in% class(numAndFactor) , TRUE)
    expect_equal(colnames(numAndFactor), c("X__1", "Age", "Region"))
    expect_equal(unname(sapply(numAndFactor, class)), c("factor", "numeric", "factor"))
    
    expect_warning(capture.output(numAndStr <- DownloadXLSX("https://github.com/Displayr/flipAPI/blob/master/tests/testthat/xlsxfiles/numeric-and-factor.xlsx?raw=true"), type="message"), "The entered data could not be interpreted")
    expect_equal(dimnames(numAndStr), NULL)
    expect_equal(dim(numAndStr), c(6,3))
    expect_equal(class(numAndStr), "matrix")
    expect_equal(is.numeric(numAndStr), FALSE)
    
    capture.output(num2d <- DownloadXLSX("https://github.com/Displayr/flipAPI/blob/master/tests/testthat/xlsxfiles/numeric-with-headers.xlsx?raw=true"), type="message")
    expect_equal(class(num2d), "matrix")
    expect_equal(is.numeric(num2d), TRUE)
    expect_equal(dim(num2d), c(10, 6))
    expect_equal(rownames(num2d)[1], "2017-05-31")
    expect_equal(colnames(num2d)[1], "Pre-breakfast")
   
    capture.output(numAndDate <- DownloadXLSX("https://github.com/Displayr/flipAPI/blob/master/tests/testthat/xlsxfiles/numeric-with-headers.xlsx?raw=true", want.data.frame = TRUE), type="message")
    expect_equal("POSIXct" %in% class(numAndDate[,1]), TRUE)
    expect_equal(class(numAndDate[,2]), "numeric")
     
    capture.output(sheet2 <- DownloadXLSX("https://github.com/Displayr/flipAPI/blob/master/tests/testthat/xlsxfiles/numeric-no-headers.xlsx?raw=true", sheet = 2), type="message")
    expect_equal(dim(sheet2), NULL)
    expect_equal(length(sheet2), 6)
    expect_equal(is.numeric(sheet2), TRUE)
    
    expect_error(capture.output(sheet3 <- DownloadXLSX("https://github.com/Displayr/flipAPI/blob/master/tests/testthat/xlsxfiles/numeric-no-headers.xlsx?raw=true", sheet = 3), type="message"))
    
    expect_warning(capture.output(characters <- DownloadXLSX("https://github.com/Displayr/flipAPI/blob/master/tests/testthat/xlsxfiles/characters.xlsx?raw=true"), type="message"), "The entered data could not be interpreted")
    expect_equal(is.numeric(characters), FALSE)
    expect_equal(dim(characters), c(4, 4))
    expect_equal(dimnames(characters), NULL)
    
    capture.output(percentages <- DownloadXLSX("https://github.com/Displayr/flipAPI/blob/master/tests/testthat/xlsxfiles/percentages.xlsx?raw=true"), type="message")
    expect_equal(is.numeric(percentages), TRUE)
    expect_equal(length(percentages), 6)
    expect_equal(dimnames(percentages), NULL)
    expect_equal(sum(percentages < 1), 6)
})
