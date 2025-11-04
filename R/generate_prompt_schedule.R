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
#' @param add_prompt_time Add a random prompt time variable to the data set within the time_blocks, Default: TRUE
#' @param random_surveys A selection of surveys to deliver randomly. This generates a controller variable that delivers different
#'   combinations of the surveys ranging from none to all that are sampled at random_survey_probability, Default: NULL
#' @param random_survey_probability A random sampling probability used to generate the controller to designate a random
#'   delivery schedule for the designated surveys. Single random_survey_probability arguments are repeated to match the
#'   length of random_surveys, Default: 0.5
#' @param event_names The name of the events to inclue in the data set that coincide with each sampling period. If provided, event_names
#'   should be the same length as the n_days argument and map onto redcap_event_name., Default: NULL
#' @param ... Optional arguments for a secondary controller in the form of a named list matching one of the random_surveys. Within the list,
#'   users can use the c() operator to control the sampling strategy in that one value within c() will be picked each time random_surveys
#'   are sampled.
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
  time_blocks = c(8, 11, 14, 18, 21),
  add_prompt_time = TRUE,
  random_surveys = NULL,
  random_survey_probability = 0.5,
  event_names = NULL,
  availability = NULL,
  ...
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

  data$days <- as.integer(data$days + 1)

  order <- c(
    "record_id",
    "redcap_repeat_instrument",
    "redcap_event_name",
    "redcap_repeat_instance",
    "days",
    "prompts"
  )

  # Assign random time within block
  if (add_prompt_time & !is.null(availability)) {
    stopifnot(length(time_blocks) == max(prompts_per_day) + 1)

    # Parse availability data once for this participant
    availability <- parse_availability(availability) # Your single-row data with availability columns

    prompt_times <- rep("", nrow(data))

    for (i in 1:nrow(data)) {
      prompt_date <- as.Date(data$date[i])
      day_of_week <- as.integer(format(prompt_date, "%w")) #+ 1 # 1=Monday, 7=Sunday
      prompt_num <- data$prompt[i]
      time_block <- time_blocks[prompt_num]:(time_blocks[prompt_num + 1] - 1)
      # Get available hours for this day
      avail_hours <- availability[[as.character(day_of_week)]]

      if (is.null(avail_hours) || length(avail_hours) == 0) {
        # No availability data or all times blocked - fall back to time blocks
        hour <- sample(time_block, 1)
      } else if (any(time_block %in% avail_hours)) {
        if (length(time_block[time_block %in% avail_hours]) > 1) {
          hour <- sample(time_block[time_block %in% avail_hours], 1)
        } else {
          hour <- time_block[time_block %in% avail_hours]
        }
      } else {
        addmtb <- sort(c(avail_hours, median(time_block)))
        mtbindx <- which(addmtb == median(time_block))
        diffup <- addmtb[mtbindx + 1] - median(time_block)
        diffdown <- median(time_block) - addmtb[mtbindx - 1]
        direction <- ifelse(diffup >= diffdown, "subtract", "add")
        while (!any(avail_hours %in% time_block)) {
          if (direction == "subtract") {
            time_block <- time_block - 1
          } else if (direction == "add") {
            time_block <- time_block - 1
          }
        }
        possible_hours <- avail_hours[avail_hours %in% time_block]
        if (length(possible_hours) > 1) {
          hour <- sample(possible_hours)
        } else {
          hour <- avail_hours[avail_hours %in% time_block]
        }
      }

      minute <- sample(0:59, 1)
      prompt_times[i] <- sprintf("%s %02d:%02d", data$date[i], hour, minute)

      if (i > 1) {
        if (
          gsub(":[0-9]{2}$", "", prompt_times[i]) ==
            gsub(":[0-9]{2}$", "", prompt_times[i - 1])
        ) {
          time2sub <- as.POSIXct(prompt_times[i - 1], format = "%Y-%m-%d %H:%M")
          prompt_times[i - 1] <- format(time2sub - 3600, "%Y-%m-%d %H:%M")
        }
      }
    }
    data$prompt_time <- prompt_times
    order <- c(order, "prompt_time")
  } else if (add_prompt_time & is.null(availability)) {
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
    order <- c(order, "prompt_time")
  }

  data$date <- NULL

  data$record_id = id

  data$redcap_repeat_instrument = repeat_instrument

  if (!is.null(random_surveys)) {
    data$controller <- sample_controllers(
      nrow(data),
      random_surveys,
      probs = random_survey_probability
    )
    order <- c(order, "controller")
    secondary_features = list(...)
    if (length(secondary_features) > 0) {
      secondary_feature_names = names(secondary_features)
      if (secondary_feature_names %in% random_surveys) {
        secondary_controller <- add_secondary_controller(
          data$controller,
          name = secondary_feature_names,
          secondary_features
        )
        data <- cbind(data, secondary_controller)
        order <- c(order, names(secondary_controller))
      }
    }
  }

  data <- data[, order]

  data$identifier <- sprintf(
    "record=%s&event=%s&instance=%s&days=%s&prompts=%s",
    data$record_id,
    data$redcap_event_name,
    data$redcap_repeat_instance,
    data$days,
    data$prompts
  )

  return(data)
}
