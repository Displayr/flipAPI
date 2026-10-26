library(testthat)

context("Parsing Excel files")

test_that("read_xlsx",
{
    # Serve the fixtures over HTTP locally, so DownloadXLSX downloads them without needing external network access.
    app <- webfakes::new_app()
    app$use(webfakes::mw_static(root = test_path("xlsxfiles")))
    server <- webfakes::local_app_process(app)
    xlsxURL <- function(file) server$url(paste0("/", file))

    capture.output(numAndFactor <- DownloadXLSX(xlsxURL("numeric-and-factor.xlsx"), want.data.frame = TRUE), type="message")
    expect_equal("data.frame" %in% class(numAndFactor), TRUE)
    expect_equal(colnames(numAndFactor), c("...1", "Age", "Region"))
    expect_equal(unname(sapply(numAndFactor, class)), c("factor", "numeric", "factor"))

    expect_silent(capture.output(numAndStr <- DownloadXLSX(xlsxURL("numeric-and-factor.xlsx")), type="message"))
    expect_equal(dimnames(numAndStr)[[1L]], LETTERS[1:5])
    expect_equal(dimnames(numAndStr)[[2L]], c("Age", "Region"))
    expect_equal(dim(numAndStr), c(5, 2))
    expect_equal(class(numAndStr), "data.frame")
    expect_equal(is.numeric(numAndStr), FALSE)

    capture.output(num2d <- DownloadXLSX(xlsxURL("numeric-with-headers.xlsx")), type="message")
    expect_is(num2d, "matrix")
    expect_equal(is.numeric(num2d), TRUE)
    expect_equal(dim(num2d), c(10, 6))
    expect_equal(rownames(num2d)[1], "2017-05-31")
    expect_equal(colnames(num2d)[1], "Pre-breakfast")

    capture.output(numAndDate <- DownloadXLSX(xlsxURL("numeric-with-headers.xlsx"), want.data.frame = TRUE), type="message")
    expect_equal(class(numAndDate[,2]), "numeric")

    capture.output(sheet2 <- DownloadXLSX(xlsxURL("numeric-no-headers.xlsx"), sheet = 2), type="message")
    expect_equal(dim(sheet2), c(6, 1))
    expect_equal(is.numeric(sheet2), TRUE)

    expect_error(capture.output(sheet3 <- DownloadXLSX(xlsxURL("numeric-no-headers.xlsx"), sheet = 3), type="message"))

    expect_silent(capture.output(characters <- DownloadXLSX(xlsxURL("characters.xlsx")), type="message"))
    expect_equal(is.numeric(characters), FALSE)
    expect_equal(dim(characters), c(3, 3))
    expect_equal(dimnames(characters)[[1L]], paste0("R", 1:3))
    expect_equal(dimnames(characters)[[2L]], paste0("C", 1:3))

    capture.output(percentages <- DownloadXLSX(xlsxURL("percentages.xlsx")), type="message")
    expect_equal(is.numeric(percentages), TRUE)
    expect_equal(length(percentages), 6)
    expect_equal(dimnames(percentages), NULL)
    expect_equal(sum(percentages < 1), 6)

    expect_silent(cola1 <- DownloadXLSX(xlsxURL("Cola_Discriminant_Functions.xlsx"), range="A2:G9"))
    expect_is(cola1, "matrix")
    expect_equal(dim(cola1), c(7, 6))
    expect_silent(cola2 <- DownloadXLSX(xlsxURL("Cola_Discriminant_Functions.xlsx"), want.data.frame = TRUE, want.col.names = TRUE, want.row.names = FALSE, sheet=2, range="AB2:AC330"))
    expect_equal(class(cola2), "data.frame")
    expect_equal(dim(cola2), c(328, 2))

    capture.output(xlsRemovedHeader <- DownloadXLSX(xlsxURL("xls-with-header.xls"), skip = 15), type="message")
    expect_equal("data.frame" %in% class(xlsRemovedHeader), TRUE)
    expect_equal(all(dim(xlsRemovedHeader) == c(240, 9)), TRUE)

})
