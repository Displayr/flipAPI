library(testthat)
library(RJSONIO)

expect_json_equal <- function(a, b) {
    if (a != b) {
        # expect_equal()'s output on failure with large strings is very hard to use.
        stop(paste('Mismatched JSON, paste this into the expected json and use git to diff:', a))
    }
}

test_that("UploadMetricToFactbase() produces correct JSON", {
    expected_json = '{
 "metric": {
 "name": "Metric.name",
"valueType": "real",
"aggregation": "sum",
"definition": "Our definition",
"hyperlink": "https://example.com/" 
},
"update": "append_or_update",
"dimensions": [
 {
 "name": "_When",
"dimensionType": "in_data",
"valueType": "datetime",
"unique": true 
},
{
 "name": "Dimension1",
"dimensionType": "in_data",
"valueType": "text",
"unique": false 
} 
],
"data": [
 [
                1,
   1681776000000,
"Dog" 
],
[
                2,
   1681862400000,
"Car" 
] 
] 
}'
    expect_error(
        expect_json_equal(
            UploadMetricToFactbase(
                data=data.frame(
                    `Metric name`=c(1, 2),
                    When= as.POSIXct(c("2023-04-18", "2023-04-19"), "%Y-%m-%d", tz="UTC"),
                    Dimension1=c("Dog", "Car")),
                token="fake",
                mode="append_or_update",
                aggregation="sum",
                definition="Our definition",
                hyperlink="https://example.com/",
                update_key="When",
                test_return_json=T
            ),
            expected_json
        ), NA
    )
})

test_that("UploadRelationshipToFactbase() produces correct JSON", {
    expected_json = '{
 "relationship": {
 "type": "many_to_one" 
},
"update": "append_or_update",
"dimensions": [
 {
 "name": "Dimension1",
"dimensionType": "in_data",
"valueType": "text",
"unique": true 
},
{
 "name": "Dimension2",
"dimensionType": "in_data",
"valueType": "text" 
} 
],
"data": [
 [
 "Dog",
"Canine" 
],
[
 "Cat",
"Feline" 
],
[
 "Lion",
"Feline" 
] 
] 
}'
    expect_error(
        expect_json_equal(
            UploadRelationshipToFactbase(
                data=data.frame(
                    Dimension1=c("Dog", "Cat", "Lion"),
                    Dimension2=c("Canine", "Feline", "Feline")),
                token="fake",
                mode="append_or_update",
                test_return_json=T
            ),
            expected_json
        ), NA
    )
})