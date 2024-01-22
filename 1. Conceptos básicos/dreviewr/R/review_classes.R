#' Get the Classes of Each Column in a Data Frame
#'
#' `review_classes` takes a data frame and returns a character vector with the
#' classes of its columns. This is useful to check if the data types of the
#' columns are correct. For example, if a column is supposed to be numeric and
#' when you use `review_classes` you notice that it is a character, then you
#' know that you have to convert it to numeric.
#'
#' @param df A data frame.
#'
#' @return An atomic character vector with the classes of each column in the
#'  data frame.
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
#' review_classes(df)
#'
#' # You can also use `review_classes` with the data sets that come with R
#' # or with any other data set that you have loaded in your environment.
#' review_classes(iris)
#' review_classes(mtcars)
#' review_classes(airquality)
review_classes <- function(df) {
  classes <- sapply(df, class)
  return(classes)
}
