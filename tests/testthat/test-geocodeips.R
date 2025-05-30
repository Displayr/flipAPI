context("Geocode IPs")

skip_if(Sys.getenv("IP2LOCATION_DB_PATH") == "", "No IP2Location database path set. This is required for geocoding.")


test_that("Geocode database file is found", {
    db.path <- Sys.getenv("IP2LOCATION_DB_PATH")
    # File exists
    file.exists(db.path) |> expect_true()
    # Can read the file
    file.access(db.path, 4) |> expect_equal(setNames(0, db.path))
    getIP2LocationDatabasePath() |> expect_equal(db.path)
    tmp.file <- tempfile(fileext = ".bin")
    Sys.setenv(IP2LOCATION_DB_PATH = "/foo/bar/baz")
    getIP2LocationDatabasePath() |> expect_error(
        "The IP2Location database was not found at the path: /foo/bar/baz. Please check the path and try again."
    )
    with_mocked_bindings(
        IsRServer = function() TRUE,
        getIP2LocationDatabasePath(),
        .package = "flipAPI"
    ) |> expect_error(
        paste0(
            "There was a problem with the IP address database. ",
            "Please contact support if this is causing hardship. ",
            "Error details: ",
            "The IP2Location database was not found at the path: ",
            "/foo/bar/baz. ",
            "Please check the path and try again."
        ),
        fixed = TRUE
    )
    file.create(tmp.file)
    Sys.setenv(IP2LOCATION_DB_PATH = tmp.file)
    getIP2LocationDatabasePath() |> expect_equal(tmp.file)
    # Remove read permission
    Sys.chmod(tmp.file, mode = "300")
    getIP2LocationDatabasePath() |> expect_error(
        paste0(
            "The IP2Location database is not readable at the path: ",
            tmp.file, ". ",
            "Please check the file permissions and try again."
        )
    )
    on.exit({
        Sys.setenv(IP2LOCATION_DB_PATH = db.path)
        file.remove(tmp.file)
    })
})

skip_if_not(reticulate::py_module_available("IP2Location"), "IP2Location python module not available")
test_that("Geocoding", {
    input.ips <- c("2001:780:53d2::1", "123.51.111.134", "84.70.75.194", "185.208.152.121")
    expected.output <- data.frame(
        ips = input.ips,
        continent_name = c("Europe", "Oceania", "Europe", "Europe"),
        country_name = c("Germany", "Australia", rep("United Kingdom of Great Britain and Northern Ireland", 2L)),
        country_code = c("DE", "AU", "GB", "GB")
    )
    GeocodeIPs(input.ips) |> expect_equal(expected.output)
    # Handle nonsensical IPs or local network ones
    input.ips.with.invalid <- c("", input.ips, "Nonsense", "192.168.1.123")
    expected.output.with.nonsense <- rbind.data.frame(
        data.frame(ips = "", continent_name = NA_character_,
                   country_name = NA_character_, country_code = NA_character_),
        expected.output,
        data.frame(ips = "Nonsense", continent_name = NA_character_,
                   country_name = NA_character_, country_code = NA_character_),
        data.frame(ips = "192.168.1.123", continent_name = NA_character_,
                   country_name = NA_character_, country_code = NA_character_)
    )
    GeocodeIPs(input.ips.with.invalid) |> expect_equal(expected.output.with.nonsense)
})

test_that("Geocoding: input types", {
    ips <- c("123.51.111.134", "216.27.61.137", "2001:780:53d2::1")
    matrix.1d <- matrix(
        c(
            "159.122.90.193", "159.122.90.195", "159.122.90.199", "159.122.90.207", "159.122.90.223",
            "159.122.90.231", "159.122.90.239", "159.122.90.255", "159.122.91.31", "159.122.91.47"
        ),
        ncol = 1L
    )
    df <- data.frame(ips, nums = seq(3))
    mat <- matrix(c(ips, LETTERS[1:3]), ncol = 2)
    geocodes <- GeocodeIPs(ips)
    GeocodeIPs(matrix.1d) |> expect_error(NA)
    GeocodeIPs(df) |> expect_warning("Only the first column will be geocoded.")
    GeocodeIPs(mat) |> expect_warning("Only the first column will be geocoded.")
    GeocodeIPs(list(LETTERS[1:10])) |> expect_error("Please provide a character vector of IP addresses.")
    GeocodeIPs(seq(10)) |> expect_error("Please provide a character vector of IP addresses.")
})
