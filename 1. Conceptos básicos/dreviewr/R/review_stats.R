#' Get Counts for Factor Columns in a Data Frame
#'
#' @param df A data frame.
#'
#' @return A list of data frames containing the counts for each factor column.
#' These data frames have the following columns:
#' \itemize{
#'  \item \code{factor}: The factor value.
#'  \item \code{count}: The number of times the factor value appears in the
#'  column.
#' }
#' @export
#'
#' @examples
#' #
#' library(palmerpenguins)
#'
#' review_factor_counts(iris)
#' review_factor_counts(penguins)
review_factor_counts <- function(df) {
  # Filter the data frame to only include factor columns.
  df <- df[, sapply(df, is.factor)]

  # Get a list with the counts for each factor column.
  factor_counts <- lapply(df, function(x) {
    counts <- data.frame(
      count = table(x),
      row.names = NULL
    )

    names(counts) <- c("factor", "count")
    return(counts)
  })

  return(factor_counts)
}

#' Get Statistics for Factor Columns in a Data Frame
#'
#' `review_factor_stats` takes a data frame and returns a data frame with
#' statistics for the factor columns in the data frame. The columns that
#' don't match the class of the review function are filled with \code{NA}
#' values.
#'
#' @param df A data frame.
#'
#' @return A data frame with the following columns:
#' \itemize{
#'  \item \code{column}: The name of the column.
#'  \item \code{factor_ordered}: Whether it is ordered or not.
#'  \item \code{factor_unique}: The number of unique values in the column.
#' }
#' @export
#'
#' @examples
#' df <- data.frame(
#'   w = c(1, 2, 3),
#'   x = c(4L, 5L, 6L),
#'   y = c("a", "b", "c"),
#'   z = c(TRUE, FALSE, TRUE)
#' )
#'
#' review_factor_stats(df)
#'
#' # You can also use `review_factor_stats` with the data sets that come
#' # with R or with any other data set that you have loaded in your
#' # environment.
#' review_factor_stats(iris)
#' review_factor_stats(mtcars)
#' review_factor_stats(airquality)
review_factor_stats <- function(df) {
  # Get whether each factor column is ordered or not.
  factor_ordered <- sapply(df, function(x) {
    get_stats(x, is.factor, is.ordered)
  })

  # Get the number of unique values for each factor column.
  factor_unique <- sapply(df, function(x) {
    get_stats(x, is.factor, function(x) length(unique(x)))
  })

  factor_stats <- data.frame(
    column = colnames(df),
    factor_ordered = factor_ordered,
    factor_unique = factor_unique,
    row.names = NULL
  )

  return(factor_stats)
}

#' Get Statistics for Numeric Columns in a Data Frame
#'
#' `review_numeric_stats` takes a data frame and returns a data frame with
#' statistics for the numeric columns in the data frame. The columns that
#' don't match the class of the review function are filled with \code{NA}
#' values.
#'
#' @param df A data frame.
#'
#' @return A data frame with the following columns:
#' \itemize{
#'  \item \code{column}: The name of the column.
#'  \item \code{numeric_mean}: The mean of the column.
#'  \item \code{numeric_sd}: The standard deviation of the column.
#'  \item \code{numeric_min}: The minimum value of the column.
#'  \item \code{numeric_q1}: The 1st quartile of the column.
#'  \item \code{numeric_median}: The median of the column.
#'  \item \code{numeric_q3}: The 3rd quartile of the column.
#'  \item \code{numeric_max}: The maximum value of the column.
#' }
#' @export
#'
#' @examples
#' df <- data.frame(
#'   w = c(1, 2, 3),
#'   x = c(4L, 5L, 6L),
#'   y = c("a", "b", "c"),
#'   z = c(TRUE, FALSE, TRUE)
#' )
#'
#' review_numeric_stats(df)
#'
#' # You can also use `review_numeric_stats` with the data sets that come
#' # with R or with any other data set that you have loaded in your
#' # environment.
#' review_numeric_stats(iris)
#' review_numeric_stats(mtcars)
#' review_numeric_stats(airquality)
review_numeric_stats <- function(df) {
  # Get the mean, standard deviation, minimum, 1st quartile, median, 3rd
  # quartile, and maximum for each numeric column.
  numeric_mean <- sapply(df, function(x) {
    get_stats(x, is.numeric, function(x) mean(x, na.rm = TRUE))
  })

  numeric_sd <- sapply(df, function(x) {
    get_stats(x, is.numeric, function(x) sd(x, na.rm = TRUE))
  })

  numeric_min <- sapply(df, function(x) {
    get_stats(x, is.numeric, function(x) min(x, na.rm = TRUE))
  })

  numeric_q1 <- sapply(df, function(x) {
    get_stats(x, is.numeric, function(x) quantile(x, probs = 0.25, na.rm = TRUE))
  })

  numeric_median <- sapply(df, function(x) {
    get_stats(x, is.numeric, function(x) median(x, na.rm = TRUE))
  })

  numeric_q3 <- sapply(df, function(x) {
    get_stats(x, is.numeric, function(x) quantile(x, probs = 0.75, na.rm = TRUE))
  })

  numeric_max <- sapply(df, function(x) {
    get_stats(x, is.numeric, function(x) max(x, na.rm = TRUE))
  })

  numeric_stats <- data.frame(
    column = colnames(df),
    numeric_mean = numeric_mean,
    numeric_sd = numeric_sd,
    numeric_min = numeric_min,
    numeric_q1 = numeric_q1,
    numeric_median = numeric_median,
    numeric_q3 = numeric_q3,
    numeric_max = numeric_max,
    row.names = NULL
  )

  # Round the numeric stats to 2 decimal places.
  numeric_stats[, 2:8] <- round(numeric_stats[, 2:8], 2)

  return(numeric_stats)
}

#' Get Statistics for Character Columns in a Data Frame
#'
#' `review_character_stats` takes a data frame and returns a data frame with
#' statistics for the character columns in the data frame. The columns that
#' don't match the class of the review function are filled with \code{NA}
#' values.
#'
#' @param df A data frame.
#'
#' @return A data frame with the following columns:
#' \itemize{
#'  \item \code{column}: The name of the column.
#'  \item \code{character_unique}: The number of unique values in the column.
#' }
#' @export
#'
#' @examples
#' df <- data.frame(
#'   w = c(1, 2, 3),
#'   x = c(4L, 5L, 6L),
#'   y = c("a", "b", "c"),
#'   z = c(TRUE, FALSE, TRUE)
#' )
#'
#' review_character_stats(df)
#'
#' # You can also use `review_character_stats` with the data sets that come
#' # with R or with any other data set that you have loaded in your
#' # environment.
#' review_character_stats(iris)
#' review_character_stats(mtcars)
#' review_character_stats(airquality)
review_character_stats <- function(df) {
  # Get the number of unique values for each character column.
  character_unique <- sapply(df, function(x) {
    get_stats(x, is.character, function(x) length(unique(x)))
  })

  character_stats <- data.frame(
    column = colnames(df),
    character_unique = character_unique,
    row.names = NULL
  )

  return(character_stats)
}

#' Get Statistics for Logical Columns in a Data Frame
#'
#' `review_logical_stats` takes a data frame and returns a data frame with
#' statistics for the logical columns in the data frame. The columns that
#' don't match the class of the review function are filled with \code{NA}
#' values.
#'
#' @param df A data frame.
#'
#' @return A data frame with the following columns:
#' \itemize{
#'  \item \code{column}: The name of the column.
#'  \item \code{logical_count}: The number of \code{FALSE} and \code{TRUE}
#'  values in the column.
#' }
#' @export
#'
#' @examples
#' df <- data.frame(
#'   w = c(1, 2, 3),
#'   x = c(4L, 5L, 6L),
#'   y = c("a", "b", "c"),
#'   z = c(TRUE, FALSE, TRUE)
#' )
#'
#' review_logical_stats(df)
#'
#' # You can also use `review_logical_stats` with the data sets that come
#' # with R or with any other data set that you have loaded in your
#' # environment.
#' review_logical_stats(iris)
#' review_logical_stats(mtcars)
#' review_logical_stats(airquality)
review_logical_stats <- function(df) {
  logical_count <- sapply(df, function(x) {
    get_stats(x, is.logical, function(x) {
      counts <- table(x)
      count <- paste0("FALSE: ", counts[1], ", TRUE: ", counts[2])
      return(count)
    })
  })

  logical_stats <- data.frame(
    column = colnames(df),
    logical_count = logical_count,
    row.names = NULL
  )

  return(logical_stats)
}

#' Get a Statistical Review of a Data Frame
#'
#' `review_stats` takes a data frame and performs a statistical review of it.
#' It calls a review function for each class of column and merges the results
#' into a single data frame. The columns that don't match the class of the
#' review function are filled with \code{NA} values. At the moment, the
#' following classes of columns are supported:
#' \itemize{
#'  \item \code{factor}
#'  \item \code{numeric}
#'  \item \code{character}
#'  \item \code{logical}
#' }
#'
#' @param df A data frame.
#'
#' @return A data frame with the following columns:
#' \itemize{
#'  \item \code{column}: The name of the column.
#'  \item \code{factor_ordered}: Whether it is ordered or not.
#'  \item \code{factor_unique}: The number of unique values in the column.
#'  \item \code{numeric_mean}: The mean of the column.
#'  \item \code{numeric_sd}: The standard deviation of the column.
#'  \item \code{numeric_min}: The minimum value of the column.
#'  \item \code{numeric_q1}: The 1st quartile of the column.
#'  \item \code{numeric_median}: The median of the column.
#'  \item \code{numeric_q3}: The 3rd quartile of the column.
#'  \item \code{numeric_max}: The maximum value of the column.
#'  \item \code{character_unique}: The number of unique values in the column.
#'  \item \code{logical_count}: The number of \code{FALSE} and \code{TRUE}
#'  values in the column.
#' }
#' @export
#'
#' @examples
#' df <- data.frame(
#'   w = c(1, 2, 3),
#'   x = c(4L, 5L, 6L),
#'   y = c("a", "b", "c"),
#'   z = c(TRUE, FALSE, TRUE)
#' )
#'
#' review_stats(df)
#'
#' # You can also use `review_stats` with the data sets that come with R
#' # or with any other data set that you have loaded in your environment.
#' review_stats(iris)
#' review_stats(mtcars)
#' review_stats(airquality)
review_stats <- function(df) {
  unique_classes <- unique(review_classes(df))

  # Call each of the review functions for each class of column.
  factor_stats <- review_factor_stats(df)
  numeric_stats <- review_numeric_stats(df)
  character_stats <- review_character_stats(df)
  logical_stats <- review_logical_stats(df)

  # Merge the results into a single data frame.
  gruoped_stats <- list(
    factor_stats,
    numeric_stats,
    character_stats,
    logical_stats
  )

  stats <- Reduce(
    function(x, y) {
      merge(x, y, by = "column")
    },
    gruoped_stats
  )

  return(stats)
}
