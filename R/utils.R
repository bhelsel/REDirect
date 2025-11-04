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
