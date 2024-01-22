#' Get Explicit Warnings About Anomalies in a Data Frame
#'
#' `review_warnings()` takes a data frame and returns a list of character vectors
#' containing the warnings about anomalies in the data. The names of the list
#' elements are "duplicates", "missing", and "outliers", each of which contains
#' a character vector with the warnings about the corresponding anomaly (except
#' for "duplicates", which is a single string). If there are no warnings about
#' a particular anomaly, the corresponding list element is `NA`. The warnings
#' are explicit, so they can be printed directly. The warnings are also
#' informative, so they can be used to improve the data. For example, the
#' warnings about missing values can be used to decide whether to impute the
#' missing values or to remove the corresponding rows or columns.
#'
#' @param df A data frame.
#'
#' @return A list of character vectors containing the warnings about anomalies
#' in the data.
#' @export
#'
#' @examples
#' review_warnings(iris)
#' review_warnings(mtcars)
#' review_warnings(airquality)
review_warnings <- function(df) {
  # Define the warnings object.
  warnings <- list()

  # Get warnings about duplicated rows.
  duplicates <- review_duplicates(df)
  if (nrow(duplicates) > 0) {
    warnings$duplicates <- paste0(
      "The data contains ",
      nrow(duplicates),
      " duplicated rows."
    )
  } else {
    warnings$duplicates <- NA
  }

  # Get warnings about missing values.
  missing <- review_missing(df)
  missing_warnings <- vector(mode = "character", length = ncol(df))

  for (i in 1:ncol(df)) {
    if (missing[i, 2] > 0) {
      missing_warnings[i] <- paste0(
        "The data contains ",
        missing[i, 2],
        " missing value(s) in the ",
        missing[i, 1],
        " column."
      )
    } else {
      missing_warnings[i] <- NA
    }
  }

  warnings$missing <- missing_warnings

  # Get warnings about outliers.
  outliers <- review_outliers(df)
  far_outliers <- review_far_outliers(df)
  outliers_warnings <- vector(mode = "character", length = ncol(df))

  for (i in 1:ncol(df)) {
    if (is.numeric(outliers[[i]]) && length(outliers[[i]]) > 0) {
      if (length(far_outliers[[i]]) > 0) {
        outliers_warnings[i] <- paste0(
          "The data contains ",
          length(outliers[[i]]),
          " outlier(s) and ",
          length(far_outliers[[i]]),
          " of them are far outliers in the ",
          names(outliers)[i],
          " column."
        )
      } else {
        outliers_warnings[i] <- paste0(
          "The data contains ",
          length(outliers[[i]]),
          " outlier(s) in the ",
          names(outliers)[i],
          " column."
        )
      }
    } else {
      outliers_warnings[i] <- NA
    }
  }

  warnings$outliers <- outliers_warnings

  return(warnings)
}
