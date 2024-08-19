test_that("testing the flow_addons.R scripts", {

    #### Testing get_rbi()

    stream_flow <- data.frame(flow = c(seq(30, 60), seq(60, 30, length.out = 60)))
    rbi <- stream_flow %>% get_rbi(flow)

    testthat::expect_equal(round(rbi, 3)$rbi, 0.015)

    # now with two stations and different flows
    stream_flow <- data.frame(flow = c(seq(30, 60), seq(60, 30, length.out = 60),
                                       seq(30, 600, length.out = 2),
                                       seq(600, 40, length.out = 7),
                                       seq(40, 800, length.out = 2),
                                       seq(800, 20, length.out = 8),
                                       seq(20, 800, length.out = 2),
                                       seq(800, 10, length.out = 8),
                                       seq(10, 1200, length.out = 62)),
                              station = c(rep('snowmelt', 91), rep('rain', 91)))

    rbi <- stream_flow %>% get_rbi(flow, station)

    testthat::expect_equal(round(rbi[,2], 3)$rbi, c(0.112, 0.015))

    #### test the `prep_flow()`
    stream_flow <- data.frame(flow = c(seq(30, 60), seq(60, 30, length.out = 60)),
                              date = seq(as.Date('2012-01-01'), by = "day", length.out = 91))

    stream_flow_prepped <- prep_flow(stream_flow, value = flow, wy_month = 10)

    testthat::expect_equal(stream_flow_prepped[1,]$date, as.Date('2012-01-01'))

    testthat::expect_equal(stream_flow_prepped[1,]$wy, 2012)

    # now test with different dates for water year (wy)

    stream_flow <- data.frame(flow = c(seq(30, 60), seq(60, 30, length.out = 60)),
                              date = seq(as.Date('2012-09-01'), by = "day", length.out = 91))

    stream_flow_prepped <- prep_flow(stream_flow, value = flow, wy_month = 10)

    testthat::expect_equal(stream_flow_prepped[31,]$wy, 2013)

})
