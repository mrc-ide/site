test_that("check_for_nas works correctly", {
  # Test data setup
  clean_data <- data.frame(
    col1 = c(1, 2, 3, 4),
    col2 = c("a", "b", "c", "d"),
    col3 = c(TRUE, FALSE, TRUE, FALSE)
  )

  data_with_nas <- data.frame(
    col1 = c(1, 2, NA, 4),
    col2 = c("a", NA, "c", "d"),
    col3 = c(TRUE, FALSE, TRUE, FALSE),
    col4 = c(NA, NA, NA, NA)
  )

  # Test 1: No error for clean data
  expect_no_error(check_for_nas(clean_data))
  expect_no_error(check_for_nas(clean_data, c("col1", "col2")))
  expect_no_error(check_for_nas(clean_data, "col1"))

  # Test 2: Error when NAs are present (single column)
  expect_error(
    check_for_nas(data_with_nas, "col1"),
    "NA values found in data_with_nas"
  )

  expect_error(
    check_for_nas(data_with_nas, "col1"),
    "col1\\(1NAs \\)"
  )

  # Test 3: Error when NAs are present (multiple columns)
  expect_error(
    check_for_nas(data_with_nas, c("col1", "col2")),
    "col1\\(1NAs \\), col2\\(1NAs \\)"
  )

  # Test 4: Error for all columns when NAs present
  expect_error(
    check_for_nas(data_with_nas), # defaults to all columns
    "col1\\(1NAs \\), col2\\(1NAs \\), col4\\(4NAs \\)"
  )

  # Test 5: Error for missing columns
  expect_error(
    check_for_nas(clean_data, "nonexistent_col"),
    "Column\\(s\\) not found in clean_data: nonexistent_col"
  )

  expect_error(
    check_for_nas(clean_data, c("col1", "missing1", "missing2")),
    "Column\\(s\\) not found in clean_data: missing1, missing2"
  )

  # Test 6: Mixed existing and missing columns
  expect_error(
    check_for_nas(clean_data, c("col1", "nonexistent")),
    "Column\\(s\\) not found in clean_data: nonexistent"
  )

  # Test 7: Empty data frame
  empty_df <- data.frame()
  expect_error(
    check_for_nas(empty_df, "any_col"),
    "Column\\(s\\) not found"
  )

  # Test 8: Single row data frame
  single_row <- data.frame(a = 1, b = NA)
  expect_error(
    check_for_nas(single_row),
    "b\\(1NAs \\)"
  )

  expect_no_error(check_for_nas(single_row, "a"))

  # Test 9: All NA column
  expect_error(
    check_for_nas(data_with_nas, "col4"),
    "col4\\(4NAs \\)"
  )

  # Test 10: Custom data name is captured correctly
  my_special_data <- data_with_nas
  expect_error(
    check_for_nas(my_special_data, "col1"),
    "NA values found in my_special_data"
  )

  # Test 11: Different data types with NAs
  mixed_types <- data.frame(
    numeric_col = c(1.5, 2.7, NA),
    character_col = c("x", "y", NA),
    logical_col = c(TRUE, NA, FALSE),
    factor_col = factor(c("A", "B", NA))
  )

  expect_error(
    check_for_nas(mixed_types),
    "numeric_col\\(1NAs \\), character_col\\(1NAs \\), logical_col\\(1NAs \\), factor_col\\(1NAs \\)"
  )

  # Test 12: Only check specific columns (some clean, some with NAs)
  expect_error(
    check_for_nas(data_with_nas, c("col1", "col3")), # col3 is clean
    "col1\\(1NAs \\)"
  )

  expect_no_error(
    check_for_nas(data_with_nas, "col3") # col3 has no NAs
  )

  # Test 13: Error message structure
  error <- expect_error(check_for_nas(data_with_nas, "col1"))
  error_msg <- conditionMessage(error)

  expect_true(grepl("NA values found in", error_msg))
  expect_true(grepl("Columns with NAs:", error_msg))
  expect_true(grepl("Please remove or handle NA values", error_msg))

  # Test 14: Edge case - no columns specified for empty selection
  expect_no_error(check_for_nas(clean_data, character(0)))

  # Test 15: Large numbers of NAs are formatted correctly
  large_na_data <- data.frame(
    col1 = rep(NA, 1000),
    col2 = c(rep(1, 999), NA)
  )

  expect_error(
    check_for_nas(large_na_data),
    "col1\\(1000NAs \\), col2\\(1NAs \\)"
  )
})

test_that("check_for_nas handles edge cases", {
  # Test with tibble (if using tidyverse)
  if (requireNamespace("tibble", quietly = TRUE)) {
    tbl_data <- tibble::tibble(
      a = c(1, 2, NA),
      b = c("x", "y", "z")
    )

    expect_error(
      check_for_nas(tbl_data, "a"),
      "a\\(1NAs \\)"
    )
  }

  # Test with data.table (if available)
  if (requireNamespace("data.table", quietly = TRUE)) {
    dt_data <- data.table::data.table(
      a = c(1, 2, NA),
      b = c("x", "y", "z")
    )

    expect_error(
      check_for_nas(dt_data, "a"),
      "a\\(1NAs \\)"
    )
  }

  # Test with matrix converted to data.frame
  mat_data <- as.data.frame(matrix(c(1, 2, NA, 4, 5, 6), nrow = 2))
  expect_error(
    check_for_nas(mat_data),
    "V2\\(1NAs \\)"
  )
})

test_that("check_for_nas parameter validation", {
  test_data <- data.frame(a = c(1, 2, 3), b = c(4, 5, NA))

  # Test that function works with different column specifications
  expect_no_error(check_for_nas(test_data, "a"))
  expect_no_error(check_for_nas(test_data, c("a")))
  expect_error(check_for_nas(test_data, "b"), "b\\(1NAs \\)")

  # Test default behavior (all columns)
  expect_error(check_for_nas(test_data), "b\\(1NAs \\)")

  # Test column name edge cases
  weird_names <- data.frame(
    `col with spaces` = c(1, 2, NA),
    `col.with.dots` = c(1, 2, 3),
    check.names = FALSE
  )

  expect_error(
    check_for_nas(weird_names, "col with spaces"),
    "col with spaces\\(1NAs \\)"
  )
})
