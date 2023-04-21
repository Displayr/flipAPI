companySecret <- get0("companySecret", ifnotfound = Sys.getenv("companySecret"))
assign("companySecret", companySecret, envir = .GlobalEnv)
clientId <- "-1027046" # This could be anything - we are just using this for metadata
assign("clientId", clientId, envir = .GlobalEnv)
region <- "app"
assign("region", region, envir = .GlobalEnv)
