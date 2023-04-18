library (testthat)

test_that("The Factbase REST API works", {
  UploadMetricToFactbase(
    data=data.frame(
      `Metric name`=c(1, 2, 3),
      `_When`= as.POSIXct(c("2023-04-18", "2023-04-18", "2023-04-18"), "%Y-%m-%d", tz="UTC"),
      Dimension1=c("Dog", "Car", "Hat")),
    token="fake",
    mode="append_or_update",
    aggregation="sum",
    definition="Our definition",
    hyperlink="https://example.com/",
    update_key="_When",
    test_expected_json="dog"
  )
})
