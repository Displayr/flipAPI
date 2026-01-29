library (testthat)

test_that("SaveData/LoadData", {
  skip_if(!nzchar(companySecret), "Not in test environment or no company set up")

  # RDS
  expect_invisible(QSaveData(mtcars, "mtcars.rds"))
  expect_true(QFileExists("mtcars.rds"))
  rds <- QLoadData("mtcars.rds")
  expect_equivalent(mtcars, rds)

  # CSV
  expect_invisible(QSaveData(mtcars, "mtcars.csv"))
  expect_true(QFileExists("mtcars.csv"))
  expect_error(QLoadData("mtcars.csv"), NA)

  # SAV
  expect_invisible(QSaveData(mtcars, "mtcars.sav"))
  expect_true(QFileExists("mtcars.sav"))
  expect_error(QLoadData("mtcars.sav"), NA)

  # XLSX
  expect_invisible(QSaveData(mtcars, "mtcars.xlsx"))
  expect_true(QFileExists("mtcars.xlsx"))
  expect_equal(dim(QLoadData("mtcars.xlsx")), dim(mtcars))

  # GIF
  library(gganimate)
  p <- ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) + geom_point()
  anim <- p + transition_states(Species, transition_length = 2, state_length = 1)
  anim <- animate(anim, renderer = gifski_renderer())

  expect_invisible(QSaveData(anim, "anim.gif"))
  expect_true(QFileExists("anim.gif"))
  expect_error(QLoadData("anim.gif"), "Invalid file type specified.")

  # Compressed SAV
  expect_warning(QSaveData(mtcars, "mtcars.sav",
                             compression.file.size.threshold = 1),
                 "Object compressed into a zip file and uploaded to Displayr Cloud Drive.")
  expect_true(QFileExists("mtcars.zip"))
})

test_that("Save/Load Data: bad cases", {
  skip_if(!nzchar(companySecret), "Not in test environment or no company set up")

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
})

test_that("File Connection: raw", {
  skip_if(!nzchar(companySecret), "Not in test environment or no company set up")

  # Test various file formats
  # raw file
  filename <- "raw_test"
  expect_silent(conn <- QFileOpen(filename, "w"))
  txt_string <- "This is a test line."

  writeLines(txt_string, conn)
  expect_message(expect_invisible(close(conn)))

  expect_silent(conn <- QFileOpen(filename))
  expect_silent(read_lines <- readLines(conn, warn = FALSE))
  expect_equal(txt_string, read_lines)

  expect_silent(expect_invisible(close(conn)))
})

test_that("File Connection: rds", {
  skip_if(!nzchar(companySecret), "Not in test environment or no company set up")

  # csv file
  filename <- "test.rds"

  expect_silent(conn <- QFileOpen(filename, "w"))

  expect_silent(saveRDS(mtcars, conn, ascii = TRUE))
  expect_message(expect_invisible(close(conn)))

  expect_silent(conn <- QFileOpen(filename))
  expect_silent(csv <- readRDS(gzcon(conn)))

  expect_silent(expect_invisible(close(conn)))
})

test_that("File Connection: csv", {
  skip_if(!nzchar(companySecret), "Not in test environment or no company set up")

  # csv file
  filename <- "test.csv"

  expect_silent(conn <- QFileOpen(filename, "w"))

  expect_silent(write.csv(mtcars, conn))
  expect_message(expect_invisible(close(conn)))

  expect_silent(conn <- QFileOpen(filename))
  expect_silent(csv <- read.csv(conn))

  expect_silent(expect_invisible(close(conn)))
})

test_that("File Connection: json", {
  skip_if(!nzchar(companySecret), "Not in test environment or no company set up")
  # json file
  filename <- "test.json"

  expect_silent(conn <- QFileOpen(filename, "w"))

  expect_silent(writeLines(jsonlite::toJSON(mtcars), conn))
  expect_message(expect_invisible(close(conn)))

  expect_silent(conn <- QFileOpen(filename))
  expect_silent(json <- jsonlite::fromJSON(readLines(conn, warn = FALSE)))
  expect_equivalent(json, mtcars)

  expect_silent(expect_invisible(close(conn)))
})

test_that("DS-3269: Data Mart unavailable",
{
    library(httptest)
    skip_if(!nzchar(companySecret), "Not in test environment or no company set up")

    without_internet({
        expect_error(QLoadData("mtcars.rds"),
                     "issue connecting to your Displayr Cloud Drive")
    })
})

test_that("QGetSharedUrl", {
    skip_if(!nzchar(companySecret), "Not in test environment or no company set up")

    # First save a file to share
    expect_invisible(QSaveData(mtcars, "mtcars_share_test.rds"))
    expect_true(QFileExists("mtcars_share_test.rds"))

    # Get the shared URL
    shared.url <- QGetSharedUrl("mtcars_share_test.rds")
    expect_true(is.character(shared.url))
    expect_true(nzchar(shared.url))

    # Calling again should return the same URL (idempotent)
    shared.url.again <- QGetSharedUrl("mtcars_share_test.rds")
    expect_equal(shared.url, shared.url.again)

    # Clean up
    expect_invisible(QDeleteFiles("mtcars_share_test.rds"))
})

test_that("QGetSharedUrl: bad cases", {
    skip_if(!nzchar(companySecret), "Not in test environment or no company set up")

    # Non-existent file should return 404 error
    expect_error(
        QGetSharedUrl("file_that_does_not_exist.rds"),
        "404: No such file exists"
    )

    # 404 error with mocked POST
    mocked.post <- function(...) {
        list(status_code = 404)
    }
    with_mocked_bindings(
        POST = mocked.post,
        QGetSharedUrl("any_file.rds"),
        .package = "httr"
    ) |>
        expect_error(
            paste0(
                "QGetSharedUrl has encountered an unknown error. ",
                "404: No such file exists. ",
                "The likely cause was an incorrect path preceding the filename, ",
                "or insufficient access to the file path."
            ),
            fixed = TRUE
        )
})

test_that("Delete Data",
{
  skip_if(!nzchar(companySecret), "Not in test environment or no company set up")

  local_mocked_bindings(getApiRoot = function(endpoint = "DataMart") paste0("https://master.displayr.com/api/", endpoint, "/"))
  prevClientId <- clientId
  assign("clientId", "-948985", envir = .GlobalEnv)
  on.exit(assign("clientId", prevClientId, envir = .GlobalEnv))

  expect_invisible(QSaveData(mtcars, "mtcars.rds"))
  expect_true(QFileExists("mtcars.rds"))
  expect_invisible(QDeleteFiles(c("mtcars.rds")))
  expect_warning(QFileExists("mtcars.rds"), "may not exist in the Displayr Cloud Drive or you may not have permission")

  expect_invisible(QSaveData(mtcars, "mtcars.csv"))
  expect_invisible(QSaveData(mtcars, "mtcars.sav"))
  expect_true(QFileExists("mtcars.csv"))
  expect_true(QFileExists("mtcars.sav"))

  expect_invisible(QDeleteFiles(c("mtcars.csv", "mtcars.sav")))
  expect_warning(QFileExists("mtcars.csv"), "may not exist in the Displayr Cloud Drive or you may not have permission")
  expect_warning(QFileExists("mtcars.sav"), "may not exist in the Displayr Cloud Drive or you may not have permission")

  # Should still succeed even if files don't exist
  expect_invisible(QDeleteFiles(c("mtcars.csv", "mtcars.sav")))
})
