#' @title generate_prompt_schedule
#' @description Generates a prompt schedule for an EMA study that can be hosted on REDCap
#' @param id The record_id that uniquely identifies each participant
#' @param repeat_instrument The name of the repeating instrument containing the EMA questions, Default: 'ema'
#' @param enrollment_date The participant's enrollment date to generate the dates for the schedule, Default: Sys.Date()
#' @param n_days The number of days included in the prompting schedule. The n_days argument can be a single number
#'   or an integer vector to represent multiple sampling periods, Default: 7
#' @param break_length The length of time in between sampling periods if n_days is an integer vector of
#'   multiple sampling periods, The break_length must be one less than the length of n_days Default: c()
#' @param prompts_per_day The number of prompts per day. The length of the prompts_per_day argument should
#'   match that of n_days, Default: 4
#' @param time_blocks The 24-hour time blocks for controlling when the prompts are sent. Time blocking is used
#'   to ensure the prompts are delivered sequentially throughout the day, but a random time will be generated
#'   within each of the time blocks. The time_blocks argument should be a length one more than n_days, Default: c(8, 11, 14, 18, 22)
#' @param random_surveys A selection of surveys to deliver randomly. This generates a controller variable that delivers different
#'   combinations of the surveys ranging from none to all that are sampled at random_survey_probability, Default: NULL
#' @param random_survey_probability A random sampling probability used to generate the controller to designate a random
#'   delivery schedule for the designated surveys. Single random_survey_probability arguments are repeated to match the
#'   length of random_surveys, Default: 0.5
#' @param event_names The name of the events to inclue in the data set that coincide with each sampling period. If provided, event_names
#'   should be the same length as the n_days argument and map onto redcap_event_name., Default: NULL
#' @return A data frame that can be passed to import_record to set up the prompting schedule.
#' @details Generates a prompt schedule for an EMA study that can be hosted on REDCap
#' @seealso
#'  \code{\link[purrr]{flatten}}, \code{\link[purrr]{map2}}, \code{\link[purrr]{map}}
#' @rdname generate_prompt_schedule
#' @export
#' @importFrom purrr flatten_int map2 map map_chr

generate_prompt_schedule <- function(
  id,
  repeat_instrument = "ema",
  enrollment_date = Sys.Date(),
  n_days = 7,
  break_length = c(),
  prompts_per_day = 4,
  time_blocks = c(8, 11, 14, 18, 22),
  random_surveys = NULL,
  random_survey_probability = 0.5,
  event_names = NULL
) {
  # Active day offsets and labels
  stopifnot(length(break_length) == length(n_days) - 1)
  if (!is.null(event_names)) {
    stopifnot(length(event_names) == length(n_days))
  }
  day_offsets <- c()
  labels <- c()
  start <- 0
  for (i in seq_along(n_days)) {
    burst <- seq(start, start + n_days[i] - 1)
    if (length(prompts_per_day > 1)) {
      stopifnot(length(prompts_per_day) == length(n_days))
      burst <- sort(rep(burst, prompts_per_day[i]))
    }
    day_offsets <- c(day_offsets, burst)
    if (!is.null(event_names)) {
      labels <- c(labels, rep(event_names[i], length(burst)))
    }
    if (i < length(n_days)) start <- max(burst) + 1 + break_length[i]
  }

  stopifnot(length(prompts_per_day) == length(n_days))
  data <- data.frame(
    days = day_offsets,
    prompts = purrr::flatten_int(purrr::map2(
      prompts_per_day,
      n_days,
      ~ rep(seq(1, .x, 1), .y)
    ))
  )

  if (!is.null(event_names)) {
    data$redcap_event_name <- labels
    data$redcap_repeat_instance = purrr::flatten_int(purrr::map(
      rle(labels)$lengths,
      ~ 1:.x
    ))
  } else {
    data$redcap_repeat_instrument <- 1:nrow(data)
  }

  data <- data[order(data$days, data$prompts), ]

  data$date <- as.Date(enrollment_date) + data$days

  data$days <- data$days + 1

  # Assign random time within block
  stopifnot(length(time_blocks) == max(prompts_per_day) + 1)
  data$prompt_time <-
    purrr::map_chr(data$prompt, function(x) {
      sprintf(
        "%02d:%02d",
        sample(time_blocks[x]:(time_blocks[x + 1] - 1), 1), # Hour
        sample(0:59, 1) # Minute
      )
    })

  data$prompt_time <- paste(data$date, data$prompt_time)

  data$date <- NULL

  data$record_id = id

  data$redcap_repeat_instrument = repeat_instrument

  if (!is.null(random_surveys)) {
    data$controller <- sample_controllers(
      nrow(data),
      random_surveys,
      probs = random_survey_probability
    )
  }

  order <- c(
    "record_id",
    "redcap_repeat_instrument",
    "redcap_event_name",
    "redcap_repeat_instance",
    "days",
    "prompts",
    "controller",
    "prompt_time"
  )

  order <- order[order %in% names(data)]

  data <- data[, order]

  return(data)
}
