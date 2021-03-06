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
})

test_that("Save/Load Data: bad cases", {
  skip_if(!nzchar(companySecret), "Not in test environment or no company set up")

  # Not-existent file
  bad_name <- "anamethatdoesnotexistfortesting"
  expect_warning(expect_false(QFileExists(bad_name)))
  expect_error(QLoadData(bad_name))

  # Invalid filetypes
  # - note that we don't have tests for Content-Types
  expect_error(QSaveData(mtcars,"mtcars.notrdsorcsv"))
  expect_error(QLoadData("mtcars.notrdsorcsv"))
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
