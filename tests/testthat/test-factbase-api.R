library(testthat)
library(RJSONIO)

expect_json_equal <- function(a, b) {
    if (a != b) {
        # expect_equal()'s output on failure with large strings is very hard to use.
        stop(paste("Mismatched JSON, paste this into the expected json and use git to diff:", a))
    }
}

test_that("UploadMetricToFactbase() produces correct JSON", {
    expected_json <- '{
 "metric": {
 "name": "Metric.name",
"valueType": "real",
"aggregation": "sum",
"timeAggregation": "last",
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
},
{
 "name": "DimensionWillBeConvertedToText",
"dimensionType": "in_data",
"valueType": "text",
"unique": false 
} 
],
"data": [
 [
                1,
   1681776000000,
"Dog",
"11" 
],
[
                2,
   1681862400000,
"Car",
"22" 
] 
] 
}'
    expect_error(
        expect_json_equal(
            UploadMetricToFactbase(
                data=data.frame(
                    `Metric name`=c(1, 2),
                    When= as.POSIXct(c("2023-04-18", "2023-04-19"), "%Y-%m-%d", tz="UTC"),
                    Dimension1=c("Dog", "Car"),
                    DimensionWillBeConvertedToText=c(11, 22)),
                token="fake",
                mode="append_or_update",
                aggregation="sum",
                time_aggregation="last",
                definition="Our definition",
                hyperlink="https://example.com/",
                update_key="When",
                test_return_json=TRUE
            ),
            expected_json
        ), NA
    )
})

test_that("UploadMetricToFactbase() can handle pre-aggregated data and use the `name` argument", {
    expected_json <- '{
 "metric": {
 "name": "Explicit metric name",
"valueType": "real",
"aggregation": "sum",
"timeAggregation": null 
},
"update": "replace_all",
"dimensions": [
 {
 "name": "_When",
"dimensionType": "period_type_in_table_name",
"valueType": "datetime",
"unique": false,
"valueForTheseObservations": "Day",
"periodType": "day" 
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
    1681776000000,
"Dog" 
],
[
    1681862400000,
"Car" 
] 
] 
}'
    expect_error(
        expect_json_equal(
            UploadMetricToFactbase(
                data=data.frame(
                    When= as.POSIXct(c("2023-04-18", "2023-04-19"), "%Y-%m-%d", tz="UTC"),
                    Dimension1=c("Dog", "Car")),
                token="fake",
                name="Explicit metric name",
                period_type="day",
                aggregation="sum",
                test_return_json=TRUE
            ),
            expected_json
        ), NA
    )
})

test_that("UploadRelationshipToFactbase() produces correct JSON", {
    expected_json <- '{
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
                test_return_json=TRUE
            ),
            expected_json
        ), NA
    )
})

test_that("UpdateFactbasePenetrationFormula() produces correct JSON", {
    expected_json <- '{
 "type": "penetration",
"numeratorMetricName": "query.test.ts Barks",
"denominatorMetricName": "query.test.ts Number of dogs in office",
"dimensionsToCount": [
 "Office dog name" 
] 
}'
    expect_error(
        expect_json_equal(
            UpdateFactbasePenetrationFormula(
                metric_name="query.test.ts Barks penetration vs Number of dogs in office",
                token="fake",
                numerator="query.test.ts Barks",
                denominator="query.test.ts Number of dogs in office",
                dimensions_to_count=c("Office dog name"),
                definition="definition of the new metric",
                hyperlink=NULL,
                test_return_json=TRUE
            ),
            expected_json
        ), NA
    )
})


test_that("UpdateFactbaseRatioFormula() produces correct JSON", {
    expected_json <- '{
 "type": "ratio",
"numeratorMetricName": "HR: Employee Attrition By Team",
"denominatorMetricName": "HR: Employees - Headcount By Team",
"smoothing": {
 "window": "year",
"sum": true 
} 
}'
    expect_error(
        expect_json_equal(
            UpdateFactbaseRatioFormula(
                metric_name="HR: Employee turnover",
                token="fake",
                numerator="HR: Employee Attrition By Team",
                denominator="HR: Employees - Headcount By Team",
                smoothing.window="year",
                smoothing.sum=T,
                definition="definition of the new metric",
                hyperlink=NULL,
                test_return_json=TRUE
            ),
            expected_json
        ), NA
    )
})
