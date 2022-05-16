# Note: all tests use package testthat

if (!require(testthat)) install.packages('testthat')
library(testthat)

source("Scom.R")

NACheck <- matrix(data = NA, nrow = 3, ncol = 3)
NACheck1 <- matrix(data = 1, nrow = 3, ncol = 3)
NACheck1[2,2] <- NA

testthat::test_that("Check NA error handling",{
  testthat::expect_error(Scom(NACheck), "NAs present")
  testthat::expect_error(Scom(NACheck1), "NAs present")
})

ZCase <- matrix(data = 0, nrow = 3, ncol = 3)

OCase <- matrix(data = 1, nrow = 3, ncol = 3)

OneOffCase <- matrix(data = 1, nrow = 3, ncol = 3)
OneOffCase[1,1] <- 0

VariableRCase <- matrix(data = NA, nrow = 3, ncol = 3)
VariableRCase[1,] <- c(1,2,1)
VariableRCase[2,] <- c(1,2,1)
VariableRCase[3,] <- c(1,2,1)

VariableCCase <- matrix(data = NA, nrow = 3, ncol = 3)
VariableCCase[,1] <- c(1,2,1)
VariableCCase[,2] <- c(1,2,1)
VariableCCase[,3] <- c(1,2,1)

VariableCase <- matrix(data = NA, nrow = 3, ncol = 3)
VariableCase[,1] <- c(1,2,1)
VariableCase[,2] <- c(0,1,2)
VariableCase[,3] <- c(2,0,2)

testthat::test_that("Check for correct results",{
  testthat::expect_equal(Scom(ZCase), NaN)
  testthat::expect_equal(Scom(OCase), Inf)
  testthat::expect_gte(Scom(OneOffCase), 4.618802)
  testthat::expect_lte(Scom(OneOffCase), 4.618803)
  testthat::expect_equal(Scom(VariableRCase), Inf)
  testthat::expect_gte(Scom(VariableCCase), 2.309401)
  testthat::expect_lte(Scom(VariableCCase), 2.309402)
  # expect_equal is misbehaving, so I'm going to set a tight bound with lt & gt instead
  testthat::expect_gte(Scom(VariableCase), 3.175426)
  testthat::expect_lte(Scom(VariableCase), 3.175427)
})
