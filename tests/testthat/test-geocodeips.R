context("Geocode IPs")

    
test_that("Geocoding",
          {
              geo <- GeocodeIPs(c("", "2001:780:53d2::1", "123.51.111.134", "Nonsense"))
              expect_equal(is.na(geo[1, 2]), TRUE)
              expect_equal(geo[2, 4], "DE")
              expect_equal(geo[3, 4], "AU")
          }
)

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