library(testthat)
library(dplyr)

pacta_r_package_path <- list(
  c(
    path = "RMI-PACTA/r2dii.data",
    lifecycle = "stable",
    ci_check = "R.yml"
  )
)

test_that("returns correct structure and data", {
  result <- generate_package_table(pacta_r_package_path)
  expected_names <- c(
    "name",
    "lifecycle",
    "status",
    "coverage",
    "version",
    "maintainer"
  )

  expect_true(tibble::is_tibble(result))
  expect_equal(ncol(result), 6)
  expect_true(all(expected_names %in% names(result)))
})
