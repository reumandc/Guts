# Note: all tests use package testthat

if (!require(testthat)) install.packages('testthat')
library(testthat)

source("SAE.R")

NACheck <- matrix(data = NA, nrow = 3, ncol = 3)
NACheck1 <- matrix(data = 1, nrow = 3, ncol = 3)
NACheck1[2,2] <- NA

testthat::test_that("Check NA error handling",{
  testthat::expect_error(SAE(NACheck), "NAs present")
  testthat::expect_error(SAE(NACheck1), "NAs present")
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
  
  testthat::expect_equal(SAE(ZCase), NaN)
  testthat::expect_equal(SAE(OCase), NaN)
  testthat::expect_equal(SAE(OneOffCase), 1)
  testthat::expect_equal(SAE(VariableRCase), NaN)
  
  testthat::expect_gte(SAE(VariableCCase), 1.732050)
  testthat::expect_lte(SAE(VariableCCase), 1.732051)
  # expect_equal is misbehaving, so I'm going to set a tight bound with lt & gt instead
  testthat::expect_gte(SAE(VariableCase), 1.673032)
  testthat::expect_lte(SAE(VariableCase), 1.673033)
})
