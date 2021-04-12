companySecret <- get0("companySecret", ifnotfound = Sys.getenv("companySecret"))
assign("companySecret", companySecret, envir = .GlobalEnv)
clientId <- "123456" # This could be anything - we are just using this for metadata
region <- "master"
