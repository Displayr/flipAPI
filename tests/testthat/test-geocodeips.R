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
              df <- data.frame(ips, nums = seq(3))
              mat <- matrix(c(ips, LETTERS[1:3]), ncol = 2)
              geocodes <- GeocodeIPs(ips)
              expect_warning(GeocodeIPs(df), "Only the first column will be geocoded.")
              expect_warning(GeocodeIPs(mat), "Only the first column will be geocoded.")
              expect_error(GeocodeIPs(list(LETTERS[1:10])), "Please provide a charcater vector of IP addresses.")
              expect_error(GeocodeIPs(seq(10)), "Please provide a charcater vector of IP addresses.")
          }
)