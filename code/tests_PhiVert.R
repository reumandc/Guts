# Note: all tests use package testthat

if (!require(testthat)) install.packages('testthat')
library(testthat)

source("PhiVert.R")

NACheck <- matrix(data = NA, nrow = 3, ncol = 3)
NACheck1 <- matrix(data = 1, nrow = 3, ncol = 3)
NACheck1[2,2] <- NA

testthat::test_that("Check NA error handling",{
  testthat::expect_error(PhiVert(NACheck), "Error in PhiVert: NAs present")
  testthat::expect_error(PhiVert(NACheck1), "Error in PhiVert: NAs present")
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
  testthat::expect_equal(PhiVert(ZCase), NaN)
  testthat::expect_equal(PhiVert(OCase), NaN)
  testthat::expect_equal(PhiVert(OneOffCase), 1)
  testthat::expect_equal(PhiVert(VariableRCase), NaN)
  testthat::expect_equal(PhiVert(VariableCCase), 1)
  # expect_equal is misbehaving, so I'm going to set a tight bound with lt & gt instead
  testthat::expect_gte(PhiVert(VariableCase), 2.366025)
  testthat::expect_lte(PhiVert(VariableCase), 2.366026)
})
