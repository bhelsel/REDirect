# Parse availability data for a single participant
parse_availability <- function(participant_data) {
  # Get all columns matching the pattern hour___day
  avail_cols <- grep(
    "^(eight|nine|ten|eleven|twelve|thirteen|fourteen|fifteen|sixteen|seventeen|eighteen|nineteen|twenty|twentyone|twentytwo)___[1-7]$",
    names(participant_data),
    value = TRUE
  )

  # Map text hours to numeric
  hour_map <- c(
    eight = 8,
    nine = 9,
    ten = 10,
    eleven = 11,
    twelve = 12,
    thirteen = 13,
    fourteen = 14,
    fifteen = 15,
    sixteen = 16,
    seventeen = 17,
    eighteen = 18,
    nineteen = 19,
    twenty = 20,
    twentyone = 21,
    twentytwo = 22
  )

  # Create availability lookup by day of week
  available_times <- list()

  participant_data <- participant_data %>%
    dplyr::select(avail_cols) %>%
    tidyr::drop_na()

  for (col in avail_cols) {
    # Skip if marked as 1 (unavailable)
    if (participant_data[[col]] == 1) {
      next
    }

    # Parse hour and day from column name
    parts <- strsplit(col, "___")[[1]]
    hour <- hour_map[parts[1]]
    day <- as.integer(parts[2]) # 1=Sunday, 2=Monday, etc.

    if (!is.null(available_times[[as.character(day)]])) {
      available_times[[as.character(day)]] <- c(
        available_times[[as.character(day)]],
        hour
      )
    } else {
      available_times[[as.character(day)]] <- hour
    }
  }

  return(available_times)
}
