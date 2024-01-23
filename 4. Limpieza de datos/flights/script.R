# install.packages(c("dplyr", "purrr", "ggplot2", "nycflights13"))

library("dplyr")
library("purrr")
library("ggplot2")
library("nycflights13")

# Group flights by carrier
grouped_flights <- flights %>%
  group_by(carrier)

# Split flights by carrier
flights_by_carrier <- grouped_flights %>%
  split(.$carrier)

# Make a copy of airlines data frame
extended_airlines <- airlines

# Calculate number of flights by carrier
n_flights <- flights_by_carrier %>%
  map_int(function(x) {
    nrow(x)
  })

extended_airlines$flights <- n_flights

# Calculate number of cancelled flights by carrier
n_cancelled <- flights_by_carrier %>%
  map_int(function(x) {
    sum(is.na(x$dep_time))
  })

extended_airlines$canc_flights <- n_cancelled

# Calculate cancellation rate by carrier
cancel_rate <- n_cancelled / n_flights

extended_airlines$canc_rate <- cancel_rate

# Calculate average departure and arrival delay by carrier
avg_dep_delay <- flights_by_carrier %>%
  map_dbl(function(x) {
    mean(x$dep_delay,
         na.rm = TRUE
    )
  })

avg_arr_delay <- flights_by_carrier %>%
  map_dbl(function(x) {
    mean(x$arr_delay,
         na.rm = TRUE
    )
  })

extended_airlines$avg_dep_delay <- avg_dep_delay
extended_airlines$avg_arr_delay <- avg_arr_delay

# Calculate average air time by carrier
avg_air_time <- flights_by_carrier %>%
  map_dbl(function(x) {
    mean(x$air_time,
         na.rm = TRUE)
  })


extended_airlines$avg_air_time <- avg_air_time

# Calculate average distance by carrier
avg_distance <- flights_by_carrier %>%
  map_dbl(function(x) {
    mean(x$distance,
         na.rm = TRUE)
  })

extended_airlines$avg_distance <- avg_distance

# Calculate average speed by carrier
avg_speed_miles_per_minute <- flights_by_carrier %>%
  map_dbl(function(x) {
    mean(x$distance / x$air_time,
         na.rm = TRUE
    )
  })

avg_speed_miles_per_hour <- avg_speed_miles_per_minute * 60
# avg_speed_km_per_hour <- avg_speed_miles_per_hour * 1.609344

extended_airlines$avg_speed <- avg_speed_miles_per_hour

# Calculate most frequent origin airport by carrier
most_frequent_origin <- flights_by_carrier %>%
  map_chr(function(x) {
    names(
      sort(
        table(x$origin),
        decreasing = TRUE
      )
    )[1]
  })

extended_airlines$most_frequent_origin <- most_frequent_origin

# Calculate most frequent destination airport by carrier
most_frequent_dest <- flights_by_carrier %>%
  map_chr(function(x) {
    names(
      sort(
        table(x$dest),
        decreasing = TRUE
      )
    )[1]
  })

extended_airlines$most_frequent_dest <- most_frequent_dest
