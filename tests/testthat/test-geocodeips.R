context("Geocode IPs")

skip_if(Sys.getenv("IP2LOCATION_DB_PATH") == "", "No IP2Location database path set. This is required for geocoding.")

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
    GeocodeIPs(nonsensical.ips) |> expect_equal(expected.output.with.nonsense)
})

test_that("Geocoding: input types",
          {
              ips <- c("123.51.111.134", "216.27.61.137", "2001:780:53d2::1")
              matrix.1d <- structure(c("159.122.90.193", "159.122.90.195", "159.122.90.199",
                                      "159.122.90.207", "159.122.90.223", "159.122.90.231", "159.122.90.239",
                                      "159.122.90.255", "159.122.91.31", "159.122.91.47"), .Dim = c(10L,
                                                                                                    1L))
              df <- data.frame(ips, nums = seq(3))
              mat <- matrix(c(ips, LETTERS[1:3]), ncol = 2)
              geocodes <- GeocodeIPs(ips)
              expect_error(GeocodeIPs(matrix.1d), NA)
              expect_warning(GeocodeIPs(df), "Only the first column will be geocoded.")
              expect_warning(GeocodeIPs(mat), "Only the first column will be geocoded.")
              expect_error(GeocodeIPs(list(LETTERS[1:10])), "Please provide a character vector of IP addresses.")
              expect_error(GeocodeIPs(seq(10)), "Please provide a character vector of IP addresses.")
          }
)
