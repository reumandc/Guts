# Note: all tests use package testthat

if (!require(testthat)) install.packages('testthat')
library(testthat)

source("Scomip.R")

NACheck <- matrix(data = NA, nrow = 3, ncol = 3)
NACheck1 <- matrix(data = 1, nrow = 3, ncol = 3)
NACheck1[2,2] <- NA

testthat::test_that("Check NA error handling",{
  testthat::expect_error(Scomip(NACheck), "NAs present")
  testthat::expect_error(Scomip(NACheck1), "NAs present")
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
  testthat::expect_equal(Scomip(ZCase), NaN)
  testthat::expect_equal(Scomip(OCase), Inf)
  testthat::expect_gte(Scomip(OneOffCase), 4.618802)
  testthat::expect_lte(Scomip(OneOffCase), 4.618803)
  testthat::expect_equal(Scomip(VariableRCase), Inf)
  testthat::expect_equal(Scomip(VariableCCase), 4)
  # expect_equal is misbehaving, so I'm going to set a tight bound with lt & gt instead
  testthat::expect_gte(Scomip(VariableCase), 2.245365)
  testthat::expect_lte(Scomip(VariableCase), 2.245366)
})
