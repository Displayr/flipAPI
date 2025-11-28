localGlobal("companySecret", "test_company_secret")
localGlobal("projectSecret", "test_project_secret")

test_env = new.env() # holds HTTP header verification result that is put there by mocked HTTP function and is used by the tests below

verifyHttpHeaders <- function(headers = list(), expect_company_secret_header_to_be_equivalent_to_company_secret, expect_project_secret_header_to_be_equivalent_to_project_secret) 
{
    companySecretHeader = headers["X-Q-Company-Secret"];
    companySecretHeader <- ifelse(is.null(companySecretHeader), "", companySecretHeader)
    projectSecretHeader = headers["X-Q-Project-Secret"];
    projectSecretHeader <- ifelse(is.null(projectSecretHeader), "", projectSecretHeader)
    httpResponse <- structure(list(status_code = 200, content = "mock"), class = "response")
    companySecret <- getCompanySecret()
    projectSecret <- getProjectSecret()
  
    test_env$company_secret_header <- companySecretHeader
    test_env$company_secret <- companySecret
    test_env$project_secret_header <- projectSecretHeader
    test_env$project_secret <- projectSecret

    companySecretHeaderIsValid <- TRUE
    projectSecretHeaderIsValid <- TRUE
    errors <- character()
  
    if (expect_company_secret_header_to_be_equivalent_to_company_secret && !identical(companySecretHeader, companySecret)) {
        companySecretHeaderIsValid <- FALSE
        errors <- append(errors, paste0("Expected 'companySecretHeader' ('", companySecretHeader, "') to equal 'companySecret' ('", companySecret, "')"))
    }
    
    if (!expect_company_secret_header_to_be_equivalent_to_company_secret && identical(companySecretHeader, companySecret)) {
        companySecretHeaderIsValid <- FALSE
        errors <- append(errors, paste0("Expected 'companySecretHeader' ('", companySecretHeader, "') to not equal 'companySecret' ('", companySecret, "')"))
    }
    
    if (expect_project_secret_header_to_be_equivalent_to_project_secret && !identical(projectSecretHeader, projectSecret)) {
        projectSecretHeaderIsValid <- FALSE
        errors <- append(errors, paste0("Expected 'projectSecretHeader' ('", projectSecretHeader, "') to equal 'projectSecret' ('", projectSecret, "')"))
    }
  
    if (!expect_project_secret_header_to_be_equivalent_to_project_secret && identical(projectSecretHeader, projectSecret)) {
        projectSecretHeaderIsValid <- FALSE
        errors <- append(errors, paste0("Expected 'projectSecretHeader' ('", projectSecretHeader, "') to not equal 'projectSecret' ('", projectSecret, "')"))
    }
  
    test_env$headersVerificationResult <- list(asExpected = companySecretHeaderIsValid && projectSecretHeaderIsValid,  messages = errors)
    return(httpResponse)
}

expect_successful_headers_verification <- function() {
    if (!test_env$headersVerificationResult$asExpected) {
        fail(test_env$headersVerificationResult$messages)
    } else {
        expect_true(TRUE)
    }
}

params <- list(
    list(company.token = NA, document.token = NA,
      expect_company_secret_header_to_be_equivalent_to_company_secret = TRUE, 
      expect_project_secret_header_to_be_equivalent_to_project_secret = TRUE,
      description = "neither company.token no document.token are provided (NA)"),

    list(company.token = "some_token", document.token = "some_project_token",
      expect_company_secret_header_to_be_equivalent_to_company_secret = FALSE, 
      expect_project_secret_header_to_be_equivalent_to_project_secret = FALSE,
      description = "company.token is 'some_token'; document.token is 'some_project_token'"),

    list(company.token = "test_company_secret", document.token = "test_project_secret",
      expect_company_secret_header_to_be_equivalent_to_company_secret = TRUE, 
      expect_project_secret_header_to_be_equivalent_to_project_secret = TRUE,
      description = "company.token is the same as companySecret; document.token is the same as projectSecret")
)

for (p in params){
    companyTokenParameter = ifelse(is.na(p$company.token), getCompanySecret(), p$company.token) # p$company.token == NA simulates missing company.token parameter
    documentTokenParameter = ifelse(is.na(p$document.token), getProjectSecret(), p$document.token) # p$document.token == NA simulates missing document.token parameter
    clientId <- "1"
 
    mockedHTTPRequest <- function(url = NULL, config = list(), ...) {
        verifyHttpHeaders(config$headers, p$expect_company_secret_header_to_be_equivalent_to_company_secret, p$expect_project_secret_header_to_be_equivalent_to_project_secret)
    }
  
    mockedCloseConnection <- function(con) { }
  
    mockedFileExists <- function(filename) { return(FALSE) }
  
    mockedCurl <- function(url, open, handle) {
        structure(list(url = "http://test/api/DataMart"), class = "connection")
    }

    test_that(paste0("QFileExists correctly passes companySecret and company.token in HTTP header when ", p$description),
    {
        test_env$headersVerificationResult <- NULL

        with_mocked_bindings(
            code = {
                args <- list(filename = "Test.dat", show.warning = FALSE)
                if (!is.na(companyTokenParameter)) {
                    args$company.token <- companyTokenParameter
                }
                if (!is.na(documentTokenParameter)) {
                    args$document.token <- documentTokenParameter
                }
                result <- do.call(QFileExists, args)

                expect_successful_headers_verification()
                expect_true(result, info = "QFileExists should return TRUE with mocked GET.")
            },
            GET = mockedHTTPRequest,
            ##.package = "flipAPI"
        )
    })

    test_that(paste0("close.qpostcon correctly passes companySecret and company.token in HTTP header when ", p$description),
    {
        test_env$headersVerificationResult <- NULL
      
        orig_close_connection <- close.connection
        orig_file_exists <- file.exists
        on.exit({
            close.connection <- orig_close_connection
            file.exists <- orig_file_exists
        }, add = TRUE)
        close.connection <- NULL # this is needed to ensure a binding for the function exists in this package's namespace before mocking
        file.exists <- NULL

        with_mocked_bindings(
            code = {
                with_mocked_bindings(
                    code = {
                        con <- structure(list(url = "http://test/api/DataMart"), class = "connection")
                        # close.qpostcon does not directly accept token parameters, but uses values that were stored with the connection by QFileOpen
                        attr(con, "company.secret") <- companyTokenParameter
                        attr(con, "project.secret") <- documentTokenParameter

                        close.qpostcon(con)

                        expect_successful_headers_verification()
                    },
                    close.connection = mockedCloseConnection,
                    file.exists = mockedFileExists,
                    .package = "base"
                )
            },
            POST = mockedHTTPRequest
        )
    })
  
    test_that(paste0("QDeleteFiles correctly passes companySecret and company.token in HTTP header when ", p$description),
    { 
        test_env$headersVerificationResult <- NULL

        with_mocked_bindings(
            code = {
                args <- list(c("Test1.dat", "Test2.dat"))
                if (!is.na(companyTokenParameter)) args$company.token <- companyTokenParameter
                if (!is.na(documentTokenParameter)) args$document.token <- documentTokenParameter
                
                do.call(QDeleteFiles, args)

                expect_successful_headers_verification()
            },
            DELETE = mockedHTTPRequest,
            .package = "flipAPI"
        )
    })
}

test_that("getProjectSecret correctly extracts project secret from environment", 
{ 
    projectSecretValueName <- "projectSecret"
    userSecretsValueName <- "userSecrets"
    testProjectSecretValue <- "test_project_secret"
    projectSecretValueInUserSecrets <- paste0(testProjectSecretValue, "_from_user_secrets")
    
    # Case #1: projectSecret is set in environment in projectSecret
    {
        localGlobal(projectSecretValueName, testProjectSecretValue)
        expect_equal(getProjectSecret(), testProjectSecretValue)
    }
  
    # Case #2: projectSecret is not set in environment in projectSecret, but is set in environment in userSecrets$projectSecret
    {
        userSecretsValue <- list(projectSecret = testProjectSecretValue)
        localGlobal(userSecretsValueName, userSecretsValue)
        expect_equal(getProjectSecret(), testProjectSecretValue)
    }

    # Case #3: projectSecret is set in both places, getProjectSecret() should prefer projectSecret
    {
        localGlobal(projectSecretValueName, testProjectSecretValue)
        userSecretsValue <- list(projectSecret = testProjectSecretValue)
        localGlobal(userSecretsValueName, userSecretsValue)
        expect_equal(getProjectSecret(), testProjectSecretValue)
    }

    # Case #4: projectSecret is not set in either place
    {
        rm(list = projectSecretValueName, envir = .GlobalEnv)
        rm(list = userSecretsValueName, envir = .GlobalEnv)
        expect_equal(getProjectSecret(), "", info = "Case #4")
    }
})
