#' @title import_record
#' @description Imports a record to REDCap through the API
#' @param study The study name or abbreviation that matches the API token stored in a user's R environment.
#'   The token should be stored as the study name or abbreviation followed by _TOKEN.
#' @param format The type of format to be uploaded to REDCap. Choices can be csv or json; however,
#'   only json is implemented at this time., Default: "json"
#' @param type Output is one record per row (flat) or one data point per row (eav), Default: flat
#' @param data A vector or data frame containing data to upload to REDCap, Default: NULL
#' @param ... Individual variables from the REDCap data base to be uploaded in the format of variable = "value".
#'   A user can pass as many variables as needed to ..., but they are only used if data is NULL
#' @return The status of the request
#' @details Imports a record to REDCap through the API
#' @seealso
#'  \code{\link[httr2]{request}}, \code{\link[httr2]{req_body}}, \code{\link[httr2]{req_perform}}
#' @rdname import_record
#' @export
#' @importFrom httr2 request req_body_form req_perform

import_record <- function(
  study,
  format = "json",
  type = c("flat", "eav"),
  data = NULL,
  ...
) {
  format <- match.arg(format)
  type = match.arg(type)
  auth <- get_token(study)

  if (is.null(data)) {
    data <- jsonlite::toJSON(list(list(...)), auto_unbox = TRUE)
  } else if (is.vector(data)) {
    data <- jsonlite::toJSON(list(as.list(data)), auto_unbox = TRUE)
  } else if (is.data.frame(data)) {
    data <- jsonlite::toJSON(data)
  }

  request <- httr2::request(auth$server) |>
    httr2::req_body_form(
      token = auth$token,
      content = "record",
      format = format,
      type = type,
      data = data
    ) |>
    httr2::req_perform()

  return(request)
}
