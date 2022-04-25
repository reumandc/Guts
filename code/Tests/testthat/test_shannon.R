# Note: all tests use package testthat

if (!require(testthat)) install.packages('testthat')
library(testthat)

source("shannon.R")

NACheck <- rep_len(NA, 3)
NACheck1 <- NACheck
NACheck1[2] <- NA

testthat::test_that("Check NA error handling",{
  testthat::expect_error(shannon(NACheck), "NAs present")
  testthat::expect_error(shannon(NACheck1), "NAs present")
})

ZCase <- rep_len(0,3)

OCase <- rep_len(1,3)

OneOffCase <- OCase
OneOffCase[1] <- 0

SymCase <- c(1,2,1)

ASymCase <- c(1,2,3)
ASymCase2 <- c(0,2,1)

testthat::test_that("Check for correct results",{
  testthat::expect_equal(shannon(ZCase), NaN)
  testthat::expect_gte(shannon(OCase), 1.098612)
  testthat::expect_lte(shannon(OCase), 1.098613)
  testthat::expect_gte(shannon(OneOffCase), 0.6931471)
  testthat::expect_lte(shannon(OneOffCase), 0.6931472)
  testthat::expect_gte(shannon(SymCase), 1.039720)
  testthat::expect_lte(shannon(SymCase), 1.039721)
  testthat::expect_gte(shannon(ASymCase), 1.011404)
  testthat::expect_lte(shannon(ASymCase), 1.011405)
  testthat::expect_gte(shannon(ASymCase2), 0.6365141)
  testthat::expect_lte(shannon(ASymCase2), 0.6365142)
  # expect_equal is misbehaving, so I'm going to set a tight bound with lt & gt instead

})

