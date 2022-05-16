# Note: all tests use package testthat

if (!require(testthat)) install.packages('testthat')
library(testthat)

source("varrat.R")

NACheck <- matrix(data = NA, nrow = 3, ncol = 3)
NACheck1 <- matrix(data = 1, nrow = 3, ncol = 3)
NACheck1[2,2] <- NA

testthat::test_that("Check NA error handling",{
  testthat::expect_error(varrat(NACheck), "NAs present")
  testthat::expect_error(varrat(NACheck1), "NAs present")
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
  testthat::expect_equal(varrat(ZCase), NaN)
  testthat::expect_equal(varrat(OCase), NaN)
  testthat::expect_equal(varrat(OneOffCase), 1)
  testthat::expect_equal(varrat(VariableRCase), NaN)
  testthat::expect_equal(varrat(VariableCCase), 3)
  testthat::expect_equal(varrat(VariableCase), .5)
})

