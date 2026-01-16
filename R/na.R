check_for_nas <- function(
  data,
  columns = colnames(data),
  data_name = deparse(substitute(data))
) {
  # Check if columns exist
  missing_cols <- setdiff(columns, names(data))
  if (length(missing_cols) > 0) {
    stop(
      "Column(s) not found in ",
      data_name,
      ": ",
      paste(missing_cols, collapse = ", ")
    )
  }

  # Check for NAs in specified columns
  na_results <- sapply(data[columns], function(x) sum(is.na(x)))
  cols_with_nas <- names(na_results)[na_results > 0]

  if (length(cols_with_nas) > 0) {
    na_counts <- na_results[cols_with_nas]
    na_summary <- paste(
      names(na_counts),
      "(",
      na_counts,
      "NAs )",
      sep = "",
      collapse = ", "
    )

    stop(
      "NA values found in ",
      data_name,
      ":\n",
      "  Columns with NAs: ",
      na_summary,
      "\n",
      "  Please remove or handle NA values before proceeding.",
      call. = FALSE
    )
  }
}
