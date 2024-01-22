library(palmerpenguins)

df <- rio::import("https://raw.githubusercontent.com/ismayc/pnwflights14/master/data/flights.csv")

test_that("review returns expected classes", {
  # Data frame with numeric columns
  numeric_df <- data.frame(x = c(1, 2, 3, 4, 5), y = 6:10)

  expect_equal(review(numeric_df)$review$class[1], "numeric")
  expect_equal(review(numeric_df)$review$class[2], "integer")

  # Data frame with character columns
  character_df <- data.frame(
    x = c("a", "b", "c", "d", "e"),
    y = c("f", "g", "h", "i", "j")
  )

  expect_equal(review(character_df)$review$class[1], "character")

  # Data frame with logical columns
  logical_df <- data.frame(
    x = c(TRUE, FALSE, TRUE, FALSE, TRUE),
    y = c(FALSE, TRUE, FALSE, TRUE, FALSE)
  )

  expect_equal(review(logical_df)$review$class[1], "logical")

  # Data frame with factor columns
  factor_df <- data.frame(
    x = factor(c("a", "b", "c", "d", "e")),
    y = factor(c("f", "g", "h", "i", "j"))
  )

  expect_equal(review(factor_df)$review$class[1], "factor")

  # More complex data frame
  expect_equal(review(iris)$review$class, c(rep("numeric", 4), "factor"))
  expect_equal(review(mtcars)$review$class, c(rep("numeric", 11)))
  expect_equal(review(penguins)$review$class, c(
    rep("numeric", 2),
    rep("integer", 2),
    rep("factor", 3),
    "integer"
  ))
  expect_equal(
    review(df)$review$class,
    c(
      rep("integer", 3),
      "character",
      rep("integer", 3),
      "character",
      rep("integer", 5),
      rep("character", 2),
      "integer"
    )
  )
})

test_that("review returns expected missing values", {
  # Simple data frame
  numeric_df <- data.frame(
    x = c(1, 2, NA, 4, NA),
    y = 6:10,
    z = c(NA, NA, NA, NA, NA)
  )

  expect_equal(review(numeric_df)$review$missing_count[1], 2)
  expect_equal(review(numeric_df)$review$missing_count[2], 0)
  expect_equal(review(numeric_df)$review$missing_count[3], 5)

  # More complex data frame
  expect_equal(review(iris)$review$missing_count, c(rep(0, 5)))
  expect_equal(review(mtcars)$review$missing_count, rep(0, 11))
  expect_equal(review(penguins)$review$missing_count, c(
    rep(2, 4),
    0,
    11,
    rep(0, 2)
  ))
  expect_equal(
    review(df)$review$missing_count,
    c(
      rep(1301, 2),
      988,
      rep(0, 2),
      rep(857, 2),
      rep(0, 3),
      rep(857, 2),
      rep(0, 4)
    )
  )
})
