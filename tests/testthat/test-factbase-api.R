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
"hyperlink": "https://example.com/",
"owner": "bob.jones@example.com" 
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
                owner="bob.jones@example.com",
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
"timeAggregation": null,
"definition": "fake definition",
"hyperlink": "https://fake.example.com",
"owner": "bob.jones@example.com" 
},
"update": "replace_all",
"dimensions": [
 {
 "name": "_When",
"dimensionType": "period_type_in_table_name",
"valueType": "datetime",
"unique": false,
"valueForTheseObservations": "Day" 
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
                definition="fake definition",
                hyperlink="https://fake.example.com",
                owner="bob.jones@example.com",
                test_return_json=TRUE
            ),
            expected_json
        ), NA
    )
})

test_that("UploadRelationshipToFactbase() produces correct JSON", {
    expected_json <- '{
 "relationship": {
 "type": "many_to_one",
"definition": "Our definition",
"hyperlink": "https://example.com/",
"owner": "bob.jones@example.com" 
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
                definition="Our definition",
                hyperlink="https://example.com/",
                owner="bob.jones@example.com",
                test_return_json=TRUE
            ),
            expected_json
        ), NA
    )
})

test_that("UploadTableToFactbase() produces correct JSON", {
    expected_json <- '{
 "tableName": "My Table",
"update": "replace_all",
"columnDefinitions": [
 {
 "name": "text",
"valueType": "text",
"mayContainNulls": false 
},
{
 "name": "numbers",
"valueType": "real",
"mayContainNulls": true 
},
{
 "name": "factor",
"valueType": "text",
"mayContainNulls": false 
},
{
 "name": "dates",
"valueType": "datetime",
"mayContainNulls": true 
} 
],
"rows": [
 [
 "Dog",
               1,
"big",
   1693440000000 
],
[
 "Cat",
null,
"big",
null 
],
[
 "Lion",
               3,
"small",
   1693526400000 
] 
],
"definition": "My table upload",
"hyperlink": "https://fake.example.com",
"owner": "bob.jones@example.com" 
}'
    expect_error(
        expect_json_equal(
            UploadTableToFactbase(
                table_name="My Table",
                data=data.frame(
                    text=c("Dog", "Cat", "Lion"),
                    numbers=c(1, NA, 3),
                    factor=factor(c("big", "big", "small")),
                    dates=as.POSIXct(c("2023-08-31", NA, "2023-09-01"), tz="UTC")),
                token="fake",
                definition="My table upload",
                hyperlink="https://fake.example.com",
                owner="bob.jones@example.com",
                na_columns=c("numbers", "dates"),
                test_return_json=TRUE
            ),
            expected_json
        ), NA
    )
})

test_that("UploadTableToFactbase() rejects NAs", {
    expect_error(UploadTableToFactbase("Blah", data.frame(n=c(42, NA)), "fake_token",
                                       definition="fake definition",
                                       hyperlink="https://fake.example.com",
                                       owner="bob.jones@example.com"),
                 regexp="contains NAs")
})

test_that("UploadTableToFactbase() rejects unexpected types with a useful error message", {
    expect_error(UploadTableToFactbase("Blah", data.frame(n=c(T, F)), "fake_token",
                                       definition="fake definition",
                                       hyperlink="https://fake.example.com",
                                       owner="bob.jones@example.com"),
                 regexp="Cannot work out which data type to use for column n containing a logical vector.  Only Date, POSIXt, text or real are accepted")
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
                hyperlink='https://example.com',
                owner='bob.jane@tmart.com',
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
                hyperlink='https://example.com',
                owner='bob.jane@tmart.com',
                test_return_json=TRUE
            ),
            expected_json
        ), NA
    )
})

test_that("AddFactbaseProvenance adds provenance to a new object", {
    x <- AddFactbaseProvenance("dog", "born of two dogs")
    expect_equal(attr(x, "factbase.provenance")$description, "born of two dogs")
})


test_that("AddFactbaseProvenance adds provenance to a new object", {
    x1 <- AddFactbaseProvenance("dog", "born of four dogs")
    x2 <- AddFactbaseProvenance(x1, "born of two dogs")
    expect_equal(attr(x2, "factbase.provenance")$description, c("born of four dogs", "born of two dogs"))
})
