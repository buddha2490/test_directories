
# Note:
# At the moment, these functions only compare .RDS files
# When I wrote this, I didn't have any sas7bdat files to play with
# It will need to be revised to incorporate different file types
# But the logic should still generally the same.



source("test_functions.R")

# Test_that sections -----------------------------------------git-------------
test_that("Identical datasets and filenames", {


  create_data(N = 5,
              rows = 1e5,
              change_filename = FALSE,
              change_colname = FALSE,
              change_dims = FALSE,
              change_class = FALSE,
              change_values = FALSE)

  expect_success(same_files("SAS Data", "R Data"))
  cat("\n")
  expect_success(same_colnames("SAS Data", "R Data"))
  cat("\n")
  expect_success(same_dims("SAS Data", "R Data"))
  cat("\n")
  expect_success(same_class("SAS Data", "R Data"))
  cat("\n")
  expect_success(same_values("SAS Data", "R Data"))
  cat("\n")

  # drop the test data
  lapply(list.files("SAS Data"), function(x) {
    file.remove(file.path("SAS Data", x))
  })
  lapply(list.files("R Data"), function(x) {
    file.remove(file.path("R Data", x))
  })

})

test_that("Change one filename", {


  create_data(N = 5,
              rows = 1e5,
              change_filename = T,
              change_colname = FALSE,
              change_dims = FALSE,
              change_class = FALSE,
              change_values = FALSE)

  # All should fail because the first step in each of these
  # test functions will test the filenames
  # If the filenames don't match, datasets can't be loaded and compared
  # so it is a logical and necessary first failure.
  expect_failure(same_files("SAS Data", "R Data"))
  cat("\n")
  expect_failure(same_colnames("SAS Data", "R Data"))
  cat("\n")
  expect_failure(same_dims("SAS Data", "R Data"))
  cat("\n")
  expect_failure(same_class("SAS Data", "R Data"))
  cat("\n")
  expect_failure(same_values("SAS Data", "R Data"))
  cat("\n")

  # drop the test data
  lapply(list.files("SAS Data"), function(x) {
    file.remove(file.path("SAS Data", x))
  })
  lapply(list.files("R Data"), function(x) {
    file.remove(file.path("R Data", x))
  })

})

test_that("Change some column names", {


  create_data(N = 5,
              rows = 1e5,
              change_filename = FALSE,
              change_colname = TRUE,
              change_dims = FALSE,
              change_class = FALSE,
              change_values = FALSE)

  expect_success(same_files("SAS Data", "R Data"))
  cat("\n")
  expect_failure(same_colnames("SAS Data", "R Data"))
  cat("\n")
  expect_success(same_dims("SAS Data", "R Data"))
  cat("\n")
  expect_success(same_class("SAS Data", "R Data"))
  cat("\n")
  expect_failure(same_values("SAS Data", "R Data")) # data don't match because colnames don't match
  cat("\n")

  # drop the test data
  lapply(list.files("SAS Data"), function(x) {
    file.remove(file.path("SAS Data", x))
  })
  lapply(list.files("R Data"), function(x) {
    file.remove(file.path("R Data", x))
  })

})

test_that("Change some data frame dimensions", {

  create_data(N = 5,
              rows = 1e5,
              change_filename = FALSE,
              change_colname = FALSE,
              change_dims = TRUE,
              change_class = FALSE,
              change_values = FALSE)

  expect_success(same_files("SAS Data", "R Data"))
  cat("\n")
  expect_success(same_colnames("SAS Data", "R Data"))
  cat("\n")
  expect_failure(same_dims("SAS Data", "R Data"))
  cat("\n")
  expect_success(same_class("SAS Data", "R Data"))
  cat("\n")
  expect_failure(same_values("SAS Data", "R Data"))  # dimensions don't match, so values won't match
  cat("\n")

  # drop the test data
  lapply(list.files("SAS Data"), function(x) {
    file.remove(file.path("SAS Data", x))
  })
  lapply(list.files("R Data"), function(x) {
    file.remove(file.path("R Data", x))
  })

})

test_that("Change some variable classes", {

  create_data(N = 5,
              rows = 1e5,
              change_filename = FALSE,
              change_colname = FALSE,
              change_dims = FALSE,
              change_class = TRUE,
              change_values = FALSE)

  expect_success(same_files("SAS Data", "R Data"))
  cat("\n")
  expect_success(same_colnames("SAS Data", "R Data"))
  cat("\n")
  expect_success(same_dims("SAS Data", "R Data"))
  cat("\n")
  expect_failure(same_class("SAS Data", "R Data"))
  cat("\n")
  expect_failure(same_values("SAS Data", "R Data")) # classes don't match, so values can't match
  cat("\n")

  # drop the test data
  lapply(list.files("SAS Data"), function(x) {
    file.remove(file.path("SAS Data", x))
  })
  lapply(list.files("R Data"), function(x) {
    file.remove(file.path("R Data", x))
  })

})

test_that("Change the actual values of some cells", {

  create_data(N = 5,
              rows = 1e5,
              change_filename = FALSE,
              change_colname = FALSE,
              change_dims = FALSE,
              change_class = FALSE,
              change_values = TRUE)

  expect_success(same_files("SAS Data", "R Data"))
  cat("\n")
  expect_success(same_colnames("SAS Data", "R Data"))
  cat("\n")
  expect_success(same_dims("SAS Data", "R Data"))
  cat("\n")
  expect_success(same_class("SAS Data", "R Data"))
  cat("\n")
  expect_failure(same_values("SAS Data", "R Data"))
  cat("\n")

  # drop the test data
  lapply(list.files("SAS Data"), function(x) {
    file.remove(file.path("SAS Data", x))
  })
  lapply(list.files("R Data"), function(x) {
    file.remove(file.path("R Data", x))
  })

})
