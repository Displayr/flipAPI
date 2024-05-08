test_that("FactBaseMetricWidget", {
    input_table <- data.frame(
        ChurnScore=0.3,
        Current=1,
        RenewalDate=as.Date('2024-04-22'),
        Ignored="ignore datum",
        Team="team name datum",
        ProductSalesStatus="product sales status datum")
    
    local_mocked_bindings(
        UploadMetricToFactbase=function(data, token, name=NULL, mode="replace_all", aggregation="sum",
                                        time_aggregation=NULL, definition=NULL, hyperlink=NULL, owner=NULL,
                                        period_type=NULL, update_key=NULL,
                                        test=list()) {
            if (names(data)[[1]] == "ChurnScore")
                expected <- data.frame(ChurnScore=input_table$ChurnScore, `_When`=input_table$RenewalDate, Team=input_table$Team, ProductSalesStatus=input_table$ProductSalesStatus)
            else
                expected <- data.frame(Current=input_table$Current, `_When`=input_table$RenewalDate, Team=input_table$Team, ProductSalesStatus=input_table$ProductSalesStatus)
            names(expected)[2] <- "_When"  # because R is a bit weird with list names provided as arguments
            expect_equal(data, expected)
            expect_equal(token, "fake-token")
            expect_equal(name, NULL)
            expect_equal(mode, "replace_all")
            expect_equal(aggregation, "sum")
            expect_equal(time_aggregation, "none")
            expect_equal(definition, "blah")
            expect_equal(hyperlink, "https://example.com/")
            expect_equal(owner, "oliver.bock@displayr.com")
            expect_equal(period_type, NULL)
            expect_equal(update_key, NULL)
            data
        }
    )
    FactbaseUploadWidget(
        factbase.token = "fake-token",
        mode = "Replace all",
        aggregation = "Sum",
        time_aggregation = "None",
        period_type = "None",
        definition = "blah",
        hyperlink = "https://example.com/",
        owner = "oliver.bock@displayr.com",
        do.upload = TRUE,
        selection.type = "Table",
        input.table = input_table,
        make.dummy.metric = FALSE,
        metric.variables = NULL,
        metric.column.names = "ChurnScore, Current",
        date.column.name = "RenewalDate",
        date.variable = NULL,
        dimension.column.names = "Team, ProductSalesStatus",
        dimension.variables = NULL,
        output.type = "Data Summaries",
        start.date = "",
        time.zone = "Australia/Sydney",
        update.period = "Days",
        update.frequency = 1,
        us.format = NULL
    )
})
