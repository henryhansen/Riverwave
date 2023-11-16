test_that("downloading and writing to csv", {
    name <- smhi_csv(274,getwd())
    path <- paste(getwd(),"/",name,".csv",sep = "")
    data <- read.csv(path)
  expect_equal(ncol(data), 4)
  expect_gt(nrow(data),0)
    unlink(path)
})
