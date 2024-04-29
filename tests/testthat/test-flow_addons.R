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



})
