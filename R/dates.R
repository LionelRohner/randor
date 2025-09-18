calc_age <- function(birth_date, target_date, floor = TRUE){
  # Convert dates to ymd
  birth_date <- lubridate::ymd(birth_date)
  target_date <- lubridate::ymd(target_date)

  # Calc diff
  diff_years <- lubridate::time_length(
    lubridate::interval(birth_date, target_date),
    "years"
  )

  # Round
  if (floor){
    return(floor(diff_years))
  } else {
    return(diff_years)
  }
}

calc_age(
  birth_date = "1989-01-24",
  target_date = "2009-01-21"
)
