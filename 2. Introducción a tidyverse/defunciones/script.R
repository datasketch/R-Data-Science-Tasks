library(dplyr)
library(tidyr)
library(readxl)

# Data source: https://www.dane.gov.co/index.php/estadisticas-por-tema/demografia-y-poblacion/informe-de-seguimiento-defunciones-por-covid-19

# Data import
raw_weeks <- read_xlsx(
  "data/defunciones.xlsx",
  sheet = "Semanas",
  range = "A12:O64",
  col_names = FALSE
)

raw_defunctions <- read_xlsx(
  "data/defunciones.xlsx",
  sheet = "Tabla seguimiento mortalidad",
  range = "A13:O12463",
  col_names = FALSE,
  col_types = c(
    "text",
    rep("guess", 14)
  )
)

# Data wrangling
colnames(raw_weeks) <- c(
  "semana",
  "2015-inicio", "2015-fin",
  "2016-inicio", "2016-fin",
  "2017-inicio", "2017-fin",
  "2018-inicio", "2018-fin",
  "2019-inicio", "2019-fin",
  "2020-inicio", "2020-fin",
  "2021-inicio", "2021-fin"
)

colnames(raw_defunctions) <- c(
  "año", "departamento", "semana",
  "natural-total", "natural-masculino",
  "natural-femenino", "natural-indeterminado",
  "violenta-total", "violenta-masculino",
  "violenta-femenino", "violenta-indeterminado",
  "en estudio-total", "en estudio-masculino",
  "en estudio-femenino", "en estudio-indeterminado"
)

weeks <- raw_weeks %>%
  # Remove text "semana" from the column and convert it to factor
  mutate(
    semana = as.factor(
      as.integer(
        gsub(
          "semana ",
          "",
          semana,
          ignore.case = TRUE
        )
      )
    )
  ) %>%
  # Unite the columns with the dates into one column per year
  unite("2015",c("2015-inicio", "2015-fin"), sep = "_") %>%
  unite("2016", c("2016-inicio", "2016-fin"), sep = "_") %>%
  unite("2017", c("2017-inicio", "2017-fin"), sep = "_") %>%
  unite("2018", c("2018-inicio", "2018-fin"), sep = "_") %>%
  unite("2019", c("2019-inicio", "2019-fin"), sep = "_") %>%
  unite("2020", c("2020-inicio", "2020-fin"), sep = "_") %>%
  unite("2021", c("2021-inicio", "2021-fin"), sep = "_") %>%
  # Enlarge the data frame
  pivot_longer(
    cols = c(
      "2015",
      "2016", "2017",
      "2018", "2019",
      "2020", "2021"
    ),
    names_to = "año",
    values_to = "inicio-fin"
  ) %>%
  # Separate the column with the dates into two columns
  separate(
    "inicio-fin",
    into = c("inicio", "fin"),
    sep = "_"
  ) %>%
  # Convert the columns to the desired data types
  mutate(
    año = as.factor(año),
    inicio = as.Date(inicio, format = "%Y-%m-%d"),
    fin = as.Date(fin, format = "%Y-%m-%d")
  ) %>%
  # Reorder the columns
  select(2, 1, 3, 4) %>%
  # Arrange the data frame by year and week
  arrange(año, semana)

defunctions <- raw_defunctions %>%
  # Fill missing values in the columns "año" and "departamento"
  fill(año, departamento) %>%
  # Remove text "semana" from the column and convert it to factor
  mutate(
    año = as.factor(año),
    departamento = as.factor(departamento),
    semana = as.factor(
      as.integer(
        gsub("semana ",
             "",
             semana,
             ignore.case = TRUE
        )
      )
    )
  ) %>%
  # Remove total rows
  filter(!is.na(semana)) %>%
  # Remove total columns
  select(-c(4, 8, 12)) %>%
  # Unite the columns with deaths by sex into one column per sex
  unite(
    "masculino",
    c(
      "natural-masculino",
      "violenta-masculino",
      "en estudio-masculino"
    ),
    sep = "_"
  ) %>%
  unite(
    "femenino",
    c(
      "natural-femenino",
      "violenta-femenino",
      "en estudio-femenino"
    ),
    sep = "_"
  ) %>%
  unite(
    "indeterminado",
    c(
      "natural-indeterminado",
      "violenta-indeterminado",
      "en estudio-indeterminado"
    ),
    sep = "_"
  ) %>%
  # Enlarge the data frame
  pivot_longer(
    cols = c(
      "masculino",
      "femenino",
      "indeterminado"
    ),
    names_to = "sexo",
    values_to = "causa"
  ) %>%
  # Separate the column with the causes of death into three columns
  separate(
    "causa",
    into = c(
      "natural",
      "violenta",
      "en estudio"
    ),
    sep = "_"
  ) %>%
  # Convert the columns to the desired data types
  mutate(
    sexo = as.factor(sexo),
    natural = as.numeric(natural),
    violenta = as.numeric(violenta),
    `en estudio` = as.numeric(`en estudio`)
  ) %>%
  # Reorder the columns
  select(1, 3, 2, 4, 5, 6, 7) %>%
  # Arrange the data frame by year, week and department
  arrange(año, semana, departamento)
