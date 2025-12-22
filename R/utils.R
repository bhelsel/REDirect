#' @keywords internal
get_token <- function(study) {
  server <- Sys.getenv("REDCAP_SERVER")
  token <- Sys.getenv(sprintf("%s_TOKEN", toupper(study)))
  return(list(server = server, token = token))
}

#' @keywords internal
get_array <- function(x, type = c("fields", "forms", "records", "events")) {
  type = match.arg(type)
  if (!is.null(x)) {
    stats::setNames(as.list(x), paste0(type, "[", seq_along(x) - 1, "]"))
  } else {
    list()
  }
}

#' @keywords internal
sample_controllers <- function(n, features, probs = 0.5) {
  if (length(probs) != length(features)) {
    stopifnot(length(probs) <= length(features))
    probs <- rep(probs, length(features))
  }
  mat <- sapply(1:length(features), function(i) stats::rbinom(n, 1, probs[i]))
  colnames(mat) <- features

  # Encode as integer (binary to decimal)
  codes <- apply(mat, 1, function(row) {
    as.integer(sum(row * 2^(seq_along(row) - 1)))
  })

  # Optional: create labels
  labels <- apply(mat, 1, function(row) {
    active <- features[which(row == 1)]
    if (length(active) == 0) "none" else paste(active, collapse = "+")
  })

  attr(codes, "labels") <- labels

  return(codes)
}

#' @keywords internal
add_secondary_controller <- function(controller, name, secondary_features) {
  indx <- grep(name, attr(controller, "label"))
  ncols <- length(unlist(secondary_features))
  features <- unlist(secondary_features)
  data <- data.frame(matrix(data = 0, ncol = ncols, nrow = length(controller)))
  names(data) <- features
  for (i in indx) {
    rsamp <- purrr::map_chr(secondary_features[[1]], ~ sample(.x, size = 1))
    data[i, rsamp] <- 1
  }
  data <- purrr::modify(data, as.integer)
  names(data) <- paste0("secondary_controller___", 1:length(features))
  return(data)
}

#' Check if participant has valid consent
#'
#' @param data A data frame containing consent information
#' @param id The participant record ID to check
#' @param consent_cols Character vector of column names to check for consent (default checks both participant and surrogate)
#' @param instrument_name Name of the consent instrument in REDCap (default: "informed_consent")
#' @return Logical indicating whether valid consent exists
#' @rdname check_consent
#' @export
#' @importFrom dplyr filter select any_of mutate across everything

check_consent <- function(
  data,
  id,
  consent_cols = c("moveidd_consent_yesno", "moveidd_surrogate_consent"),
  instrument_name = "informed_consent"
) {
  data %>%
    dplyr::filter(
      record_id == id &
        redcap_repeat_instrument == instrument_name
    ) %>%
    dplyr::select(dplyr::any_of(consent_cols)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ . == 1)) %>%
    any(na.rm = TRUE)
}

#' Check if all required surveys are complete
#'
#' @param data A data frame containing survey completion status
#' @param id The participant record ID to check
#' @param exclude_pattern Regex pattern for instruments to exclude (default: "enroll|consent")
#' @param expected_complete Expected number of completed surveys (default: 10)
#' @return Logical indicating whether all surveys are complete
#' @rdname check_surveys
#' @export
#' @importFrom dplyr filter select all_of

check_surveys <- function(
  data,
  id,
  exclude_pattern = "enroll|consent",
  expected_complete = 10
) {
  cols <- colnames(data)[grep("_complete$", colnames(data))]

  cols <- cols[-grep("enroll|consent", cols)]

  ncomp <- data %>%
    dplyr::filter(record_id == id & redcap_repeat_instrument == "") %>%
    dplyr::select(dplyr::all_of(cols)) %>%
    rowSums()

  return(any(ncomp == expected_complete, na.rm = TRUE))
}
