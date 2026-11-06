library(testthat)

# These tests check our error messages for failed Cloud Drive requests. The
# HTTP verbs are mocked so that no request leaves the machine.

mockedStatusResponse <- function(status.code) {
    force(status.code)
    function(url = NULL, config = list(), ...) list(status_code = status.code)
}

localOfflineDataMart <- function(status.code, env = parent.frame()) {
    local_mocked_bindings(
        getCompanySecret = function() "test-company-secret",
        GET = mockedStatusResponse(status.code),
        POST = mockedStatusResponse(status.code),
        .env = env
    )
}

test_that("Save/Load Data: bad cases", {
    localOfflineDataMart(404)

    # Not-existent file
    bad_name <- "anamethatdoesnotexistfortesting"
    QFileExists(bad_name) |> expect_false() |> expect_warning("may not exist in the Displayr Cloud Drive or you may not have permission")
    QLoadData(bad_name) |> expect_error("may not exist in the Displayr Cloud Drive or you may not have permission")

    # Invalid filetypes
    # - note that we don't have tests for Content-Types
    QSaveData(mtcars, "mtcars.notrdsorcsv") |> expect_error("Invalid file type specified")

    # 404 Error
    mockedPOST <- function() {
        list(status_code = 404)
    }
    formals(mockedPOST) <- formals(httr::POST)
    with_mocked_bindings(
        POST = mockedPOST,
        QSaveData(mtcars, r"(mtcars\\file.rds)")
    ) |>
        expect_error(
            paste0("QSaveData has encountered an unknown error. ",
                "404: The file could not properly be saved. ",
                "The likely cause was an incorrect path preceding the filename, or insufficient access to the file path."
                ),
            fixed = TRUE
        )

    # 422 Error (Bad path)
    mockedPOST <- function() {
        list(status_code = 422)
    }
    formals(mockedPOST) <- formals(httr::POST)
    with_mocked_bindings(
        POST = mockedPOST,
        QSaveData(mtcars, r"(FolderThatDoesNotExist\\file.rds)")
    ) |>
        expect_error(
            paste0("QSaveData has encountered an unknown error. ",
                "422: The file could not properly be saved. ",
                "The likely cause was an incorrect path preceding the filename, or insufficient access to the file path."
                ),
            fixed = TRUE
        )
})

test_that("QGetSharedUrl: bad cases", {
    localOfflineDataMart(404)

    # Non-existent file should return 404 error
    expect_error(
        QGetSharedUrl("file_that_definitely_does_not_exist_and_if_you_create_it_you_are_silly.rds"),
        "404: No such file exists"
    )
})
