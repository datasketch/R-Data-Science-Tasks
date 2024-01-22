data_to_df <- function(data) {
  if (is.data.frame(data)) {
    return(data)
  } else if (is.character(data)) {
    return(rio::import(data))
  } else {
    stop("The data must be a data frame or a path to an external file.")
  }
}

get_stats <- function(x, class_function, stat_function) {
  if (class_function(x)) {
    return(stat_function(x))
  } else {
    return(NA)
  }
}

print_review <- function(
    df,
    report,
    show_review = TRUE,
    show_warnings = TRUE) {
  if (show_review) {
    # Rename the columns for smoother printing.
    names(report$review) <- c(
      "column",
      "class",
      "missing",
      "missing_rate",
      "ordered",
      "unique",
      "mean",
      "sd",
      "min",
      "q1",
      "median",
      "q3",
      "max",
      "distinct",
      "count"
    )

    # Dataframe with general information
    info <- data.frame(
      data = c("Rows", "Columns", "Duplicate rows"),
      value = c(nrow(df), ncol(df), 2)
    )

    cat("*──────────────────────────────*\n")
    cat("  Data review\n")
    cat("*──────────────────────────────*\n")
    cat("  General information\n\n")
    print(info, row.names = FALSE, right = FALSE)
    cat("*──────────────────────────────*\n")
    cat("  Column information\n\n")
    print(
      report$review[, c("column", "class")],
      right = FALSE
    )
    cat("*──────────────────────────────*\n")

    if ("factor" %in% report$review$class) {
      cat("  Factor columns summary\n\n")
      print(
        report$review[
          report$review$class == "factor",
          c("column", "missing", "ordered", "unique")
        ],
        right = FALSE
      )
      cat("*──────────────────────────────*\n")
    }

    if ("numeric" %in% report$review$class ||
      "integer" %in% report$review$clas) {
      cat("  Numeric columns summary\n\n")
      print(
        report$review[
          report$review$class %in% c("numeric", "integer"),
          c(
            "column",
            "missing",
            "mean",
            "sd",
            "min",
            "q1",
            "median",
            "q3",
            "max"
          )
        ],
        right = FALSE
      )
      cat("*──────────────────────────────*\n")
    }

    if ("character" %in% report$review$class) {
      cat("  Character columns summary\n\n")
      print(
        report$review[
          report$review$class == "character",
          c("column", "missing", "distinct")
        ],
        right = FALSE
      )
      cat("*──────────────────────────────*\n")
    }

    if ("logical" %in% report$review$class) {
      cat("  Date columns summary\n\n")
      print(
        report$review[
          report$review$class == "logical",
          c("column", "missing", "count")
        ],
        right = FALSE
      )
      cat("*──────────────────────────────*\n")
    }
  }
  if (show_warnings) {
    cat("*──────────────────────────────*\n")
    cat("  Warnings\n")
    cat("*──────────────────────────────*\n")

    if (!is.na(report$warnings$duplicates)) {
      cat("  Duplicates:\n\n")
      cat("   - ", report$warnings$duplicates, "\n\n", sep = "")
      cat("*──────────────────────────────*\n")
    }

    if (sum(!is.na(report$warnings$missing)) > 0) {
      cat("  Missing values:\n\n")
      for (i in 1:length(report$warnings$missing)) {
        if (!is.na(report$warnings$missing[i])) {
          cat("   - ", report$warnings$missing[i], "\n", sep = "")
        }
      }
      cat("*──────────────────────────────*\n")
    }

    if (sum(!is.na(report$warnings$outliers)) > 0) {
      cat("  Outliers:\n\n")
      for (i in 1:length(report$warnings$outliers)) {
        if (!is.na(report$warnings$outliers[i])) {
          cat("   - ", report$warnings$outliers[i], "\n", sep = "")
        }
      }
      cat("*──────────────────────────────*\n")
    }
  }
}
