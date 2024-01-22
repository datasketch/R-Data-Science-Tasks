#' Get the Duplicate Rows in a Data Frame
#'
#' `review_duplicates` takes a data frame and returns a filters it to only
#' include the duplicate rows.
#'
#' @param df A data frame.
#'
#' @return A filtered data frame with the duplicate rows.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   x = c(1, 2, 3, 3, 4, 5, 5, 5),
#'   y = c(6, 7, 7, 7, 8, 9, 9, 9),
#'   z = c(11, 11, 13, 13, 14, 15, 15, 15)
#' )
#'
#' review_duplicates(df)
#'
#' # You can also use `review_duplicates` with the data sets that come with R
#' # or with any other data set that you have loaded in your environment.
#' review_duplicates(iris)
#' review_duplicates(mtcars)
#' review_duplicates(airquality)
review_duplicates <- function(df) {
  duplicates <- duplicated(df) | duplicated(df, fromLast = TRUE)
  duplicate_rows <- df[duplicates, ]

  return(duplicate_rows)
}

#' Get the Number of Missing Data in Each Column
#'
#' `review_missing` takes a data frame and returns a data frame with the number
#' and rate of missing data per column.
#'
#' @param df A data frame.
#'
#' @return A data frame with the following columns:
#' \itemize{
#'  \item \code{column}: The name of the column.
#'  \item \code{missing_count}: The number of missing data in the column.
#'  \item \code{missing_rate}: The rate of missing data in the column.
#' }
#' @export
#'
#' @examples
#' df <- data.frame(
#'   x = c(1, 2, 3, NA, 5),
#'   y = c(6, 7, NA, 9, 10),
#'   z = c(11, NA, 13, 14, 15)
#' )
#'
#' review_missing(df)
#'
#' # You can also use `review_missing` with the data sets that come with R or
#' # with any other data set that you have loaded in your environment.
#' review_missing(iris)
#' review_missing(mtcars)
#' review_missing(airquality)
review_missing <- function(df) {
  missing_count <- sapply(df, function(x) sum(is.na(x)))

  missing_rate <- sapply(df, function(x) {
    sum(is.na(x)) / length(x)
  })

  missing <- data.frame(
    column = colnames(df),
    missing_count = missing_count,
    missing_rate = missing_rate,
    row.names = NULL
  )

  # Round the missing rate to 2 decimal places.
  missing[, 3] <- round(missing[, 3], 2)

  return(missing)
}

#' Get the Outliers Values in Each Numeric Column
#'
#' `review_outliers` takes a data frame and returns a list of numeric vectors
#' with the outliers in each numeric column. The outliers are calculated using
#' the interquartile range (IQR) method or commonly known as Tukey's fences
#' technique with a multiplier of 1.5.
#'
#' @param df A data frame.
#'
#' @return A list of numeric vectors with the outliers in each numeric column.
#' @export
#'
#' @examples
#' library(nycflights13)
#'
#' review_outliers(iris)
#' review_outliers(mtcars)
#' review_outliers(airquality)
#' review_outliers(flights)
review_outliers <- function(df) {
  outliers <- sapply(df, function(x) {
    get_stats(x, is.numeric, function(x) {
      lower <- quantile(x, 0.25, na.rm = TRUE) - 1.5 * IQR(x, na.rm = TRUE)
      upper <- quantile(x, 0.75, na.rm = TRUE) + 1.5 * IQR(x, na.rm = TRUE)

      sort(x[which(x < lower | x > upper)])
    })
  })

  return(outliers)
}

#' Get the Far Outliers Values in Each Numeric Column
#'
#' `review_far_outliers` takes a data frame and returns a list of numeric
#' vectors with the far outliers in each numeric column. The far outliers are
#' calculated using the interquartile range (IQR) method or commonly known as
#' Tukey's fences technique with a multiplier of 3.
#'
#' @param df A data frame.
#'
#' @return A list of numeric vectors with the far outliers in each numeric
#' @export
#'
#' @examples
#' library(nycflights13)
#'
#' review_far_outliers(iris)
#' review_far_outliers(mtcars)
#' review_far_outliers(airquality)
#' review_far_outliers(flights)
review_far_outliers <- function(df) {
  outliers <- sapply(df, function(x) {
    get_stats(x, is.numeric, function(x) {
      lower <- quantile(x, 0.25, na.rm = TRUE) - 3 * IQR(x, na.rm = TRUE)
      upper <- quantile(x, 0.75, na.rm = TRUE) + 3 * IQR(x, na.rm = TRUE)

      sort(x[which(x < lower | x > upper)])
    })
  })

  return(outliers)
}
