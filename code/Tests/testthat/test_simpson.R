# Note: all tests use package testthat

if (!require(testthat)) install.packages('testthat')
library(testthat)

source("simpson.R")

NACheck <- rep_len(NA, 3)
NACheck1 <- NACheck
NACheck1[2] <- NA

testthat::test_that("Check NA error handling",{
  testthat::expect_error(simpson(NACheck), "NAs present")
  testthat::expect_error(simpson(NACheck1), "NAs present")
})

ZCase <- rep_len(0,3)

OCase <- rep_len(1,3)

OneOffCase <- OCase
OneOffCase[1] <- 0

SymCase <- c(1,2,1)

ASymCase <- c(1,2,3)
ASymCase2 <- c(0,2,1)

testthat::test_that("Check for correct results",{
  testthat::expect_equal(simpson(ZCase), NaN)
  testthat::expect_gte(simpson(OCase), 0.3333333)
  testthat::expect_lte(simpson(OCase), 0.3333334)
  testthat::expect_equal(simpson(OneOffCase), .5)
  testthat::expect_equal(simpson(SymCase), .375)
  testthat::expect_gte(simpson(ASymCase), 0.3888888)
  testthat::expect_lte(simpson(ASymCase), 0.3888889)
  testthat::expect_gte(simpson(ASymCase2), 0.5555555)
  testthat::expect_lte(simpson(ASymCase2), 0.5555556)
  # expect_equal is misbehaving, so I'm going to set a tight bound with lt & gt instead
  
})

