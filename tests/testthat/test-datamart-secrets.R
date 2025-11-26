library(testthat)
library(httr)

localGlobal("companySecret", "test_company_secret")

test_env = new.env() # holds HTTP header verification result that is put there by mocked HTTP function and is used by testthat tests

verifyHttpHeaders <- function(headers = list(), expect_header_to_be_equivalent_to_company_secret) 
{
    companySecretHeader = headers["X-Q-Company-Secret"];
    companySecretHeader <- ifelse(is.null(companySecretHeader), "", companySecretHeader)
  
    if (expect_header_to_be_equivalent_to_company_secret && (companySecretHeader != companySecret)) {
        test_env$headersVerificatoinResult <- list(asExpected = FALSE, 
            message = paste0("Expected 'companySecretHeader' ('",companySecretHeader,"') to equal 'companySecret' ('", companySecret,"')"))
    }
    
    if (!expect_header_to_be_equivalent_to_company_secret && (companySecretHeader == companySecret)) {
        test_env$headersVerificatoinResult <- list(asExpected = FALSE, 
            message = paste0("Expected 'companySecretHeader' ('",companySecretHeader,"') to not equal 'companySecret' ('", companySecret,"')"))
    }
    
    test_env$headersVerificatoinResult <- list(asExpected = TRUE, message = "HTTP headers are as expected")
    return(structure(list(status_code = 200, content = "mock"), class = "response"))
}

params <- list(
    list(company.token = NA, expect_header_to_be_equivalent_to_company_secret = TRUE, description = "company.token is not provided (NA)"),
    list(company.token = "some_token", expect_header_to_be_equivalent_to_company_secret = FALSE, description = "company.token is some_token"),
    list(company.token = companySecret, expect_header_to_be_equivalent_to_company_secret = TRUE, description = "company.token is the same as companySecret")
)

for (p in params){
    companyTokenParameter = p$company.token
    clientId <- "1"
 
    mockedHTTPRequest <- function(url = NULL, config = list(), ...) {
        return(verifyHttpHeaders(config$headers, p$expect_header_to_be_equivalent_to_company_secret))
    }
  
    mockedCloseConnection <- function(con) { }
  
    mockedFileExists <- function(filename) { return(FALSE) }

    test_that(paste0("QFileExists correctly passes companySecret and company.token in HTTP header when ", p$description),
    {
        test_env$headersVerificatoinResult <- NULL

        with_mocked_bindings(
            code = {
                result <- ifelse(is.na(companyTokenParameter), 
                    QFileExists("Test.dat", show.warning = FALSE),
                    QFileExists("Test.dat", show.warning = FALSE, company.token = companyTokenParameter))

                expect_true(test_env$headersVerificatoinResult$asExpected, info = test_env$headersVerificatoinResult$message)
                expect_true(result, info = "QFileExists should return TRUE with mocked GET.")
            },
            GET = mockedHTTPRequest,
            .package = "flipAPI"
        )
    })

    test_that(paste0("close.qpostcon correctly passes companySecret and company.token in HTTP header when ", p$description),
    {
        test_env$headersVerificatoinResult <- NULL
      
        orig_close_connection <- close.connection
        orig_file_exists <- file.exists
        on.exit({
            close.connection <- orig_close_connection
            file.exists <- orig_file_exists
        }, add = TRUE)
        close.connection <- NULL
        file.exists <- NULL

        with_mocked_bindings(
            code = {
                with_mocked_bindings(
                    code = {
                        con <- structure(list(url = "http://test/api/DataMart"), class = "connection")
                        result <- close.qpostcon(con)

                        expect_true(test_env$headersVerificatoinResult$asExpected, info = test_env$headersVerificatoinResult$message)
                    },
                    close.connection = mockedCloseConnection,
                    file.exists = mockedFileExists,
                    .package = "base"
                )
            },
            POST = mockedHTTPRequest,
            .package = "flipAPI"
        )
    })
  
    test_that(paste0("QDeleteFiles correctly passes companySecret and company.token in HTTP header when ", p$description),
    {
        test_env$headersVerificatoinResult <- NULL

        with_mocked_bindings(
            code = {
                result <- ifelse(is.na(companyTokenParameter), 
                    QDeleteFiles(c("Test1.dat", "Test2.dat")),
                    QDeleteFiles(c("Test1.dat", "Test2.dat"), company.token = companyTokenParameter))

                expect_true(test_env$headersVerificatoinResult$asExpected, info = test_env$headersVerificatoinResult$message)
            },
            DELETE = mockedHTTPRequest,
            .package = "flipAPI"
        )
    })
}

test_that("getProjectSecret correctly extract project secret from environment", 
{
    projectSecretValueName <- "projectSecret"
    userSecretsValueName <- "userSecrets"
    testProjectSecretValue <- "test_project_secret"

    clearProjectSecret <- function () {
        if (exists(projectSecretValueName, envir = .GlobalEnv)) 
            rm(list = projectSecretValueName, envir = .GlobalEnv)
      
        if (exists(userSecretsValueName, envir = .GlobalEnv)) {
            userSecrets <- get0(userSecretsValueName, envir = .GlobalEnv)
            if (projectSecretValueName %in% names(userSecrets)) {
                userSecrets[[projectSecretValueName]] <- NULL
                assign(userSecretsValueName, userSecrets, envir = .GlobalEnv)
            }
        }
    }
  
    # Case #1: projectSecret is set in environment in projectSecret
    clearProjectSecret()
    assign(projectSecretValueName, testProjectSecretValue, envir = .GlobalEnv)
    expect_equal(getProjectSecret(), testProjectSecretValue)
  
    # Case #2: projectSecret is not set in environment in projectSecret, but is set in environment in userSecrets$projectSecret
    clearProjectSecret()
    userSecretsValue <- list(projectSecret = testProjectSecretValue)
    assign(userSecretsValueName, userSecretsValue, envir = .GlobalEnv)
    expect_equal(getProjectSecret(), testProjectSecretValue)
  
    # Case #3: projectSecret is set in both places, getProjectSecret() should prefer projectSecret
    clearProjectSecret()
    assign(projectSecretValueName, testProjectSecretValue, envir = .GlobalEnv)
    userSecretsValue <- list(projectSecret = paste0(testProjectSecretValue, "_different"))
    assign(userSecretsValueName, userSecretsValue, envir = .GlobalEnv)
    expect_equal(getProjectSecret(), testProjectSecretValue)

    # Case #4: projectSecret is not set in either place
    clearProjectSecret()
    rm(list = userSecretsValueName, envir = .GlobalEnv)
    expect_equal(getProjectSecret(), "")
})
