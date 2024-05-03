library(testthat)
library(dplyr)

pacta_r_package_path <- "RMI-PACTA/pacta.r.package"

test_that("returns correct structure and data", {
  result <- generate_package_table(pacta_r_package_path)
  expected_names <- c("Repo", "Lifecycle", "Status", "Latest_SHA", "Maintainer")

  expect_true(tibble::is_tibble(result))
  expect_equal(ncol(result), 6)
  expect_true(all(expected_names %in% names(result)))
})

test_that("handles empty input", {
  empty_input <- character(0)
  result <- generate_package_table(empty_input)

  expect_true(tibble::is_tibble(result) && nrow(result) == 0)
})
