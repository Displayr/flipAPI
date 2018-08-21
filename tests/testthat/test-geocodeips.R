context("Geocode IPs")

test_that("Geocoding",
          {
              geo <- GeocodeIPs(c("", "2001:780:53d2::1", "123.51.111.134", "Nonsense"))
              expect_equal(is.na(geo[1, 2]), TRUE)
              expect_equal(geo[2, 4], "DE")
              expect_equal(geo[3, 4], "AU")
          }
)