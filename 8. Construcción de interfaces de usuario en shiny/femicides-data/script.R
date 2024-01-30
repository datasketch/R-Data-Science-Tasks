library(dplyr)
library(tidyr)
library(readr)

fix_long <- function(longitud) {
  # longitud: character vector
  # returns: character vector
  no_dots <- gsub("\\.", "", longitud)
  
  fixed_long <- paste0(
    substr(no_dots, 1, 3),
    ".",
    substr(no_dots, 4, nchar(no_dots))
  )
}

fix_lat <- function(latitud, departamento) {
  # latitud: character vector
  # departamento: character vector
  no_dots <- gsub("\\.", "", latitud)
  
  if_else(
    departamento == "Atlántico" | departamento == "Bolívar",
    fixed_lat <- paste0(
      substr(no_dots, 1, 2),
      ".",
      substr(no_dots, 3, nchar(no_dots))
    ),
    fixed_lat <- paste0(
      substr(no_dots, 1, 1),
      ".",
      substr(no_dots, 2, nchar(no_dots))
    )
  )
}

raw_femicides <- read_csv(
  "data/cases_2017.csv",
  show_col_types = FALSE
)

femicides <- raw_femicides |>
  mutate(
    across(
      .cols = where(is.character),
      .fns = function(x) {
        if_else(tolower(x) == "sin información", NA, x)
      }
    )
  ) |>
  mutate(
    longitud = fix_long(longitud),
    latitud = fix_lat(latitud, departamento)
  ) |>
  mutate(
    edad = as.integer(edad),
    latitud = as.numeric(latitud),
    longitud = as.numeric(longitud)
  ) |>
  rename(
    presunto_agresor = `nombre agresor/presunto agresor`,
    fuente_2 = `fuente 2`,
  )

write_csv(femicides, "data/feminicidios.csv")
