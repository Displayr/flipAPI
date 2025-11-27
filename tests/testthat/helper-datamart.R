companySecret <- get0("companySecret", ifnotfound = Sys.getenv("companySecret"))
assign("companySecret", companySecret, envir = .GlobalEnv)
projectSecret <- ""
assign("projectSecret", projectSecret, envir = .GlobalEnv)
clientId <- "-1027046" # This could be anything - we are just using this for metadata
assign("clientId", clientId, envir = .GlobalEnv)
region <- "app"
assign("region", region, envir = .GlobalEnv)

localGlobal <- function(name, value, envir = parent.frame()) {
    if (exists(name, envir = .GlobalEnv)) {
        old.value <- get(name, envir = .GlobalEnv)
        withr::defer(assign(name, old.value, envir = .GlobalEnv), envir = envir)
    } else {
        withr::defer(rm(list = name, envir = .GlobalEnv), envir = envir)
    }
    assign(name, value, envir = .GlobalEnv)
}