#' @title export_record
#' @description Retrieves a record from the REDCap API
#' @param study A study acronym or name to select the token from a user's R environment (stored as STUDY_TOKEN)
#' @param records An array of record names specifying specific records you wish to pull (by default, all records are pulled), Default: NULL
#' @param forms An array of form names you wish to pull records for. If the form name has a space in it, replace the space with an underscore
#'   (by default, all records are pulled), Default: NULL
#' @param fields An array of field names specifying specific fields you wish to pull (by default, all fields are pulled) or
#'   alternatively as a string (comma-separated list), Default: NULL
#' @param events An array of unique event names that you wish to pull records for - only for longitudinal projects, Default: NULL
#' @param format The file type to retrieve from REDCap (choices include csv or json), Default: csv
#' @param rawOrLabel Export the raw coded values or labels for the options of multiple choice fields, Default: raw
#' @param rawOrLabelHeaders Export the variable or field names (raw) or the field labels (label). Labels can only
#'   be exported if format is csv, Default: raw
#' @param exportCheckboxLabel Specifies the format of checkbox when exporting the data as labels (i.e., when
#'   rawOrLabel = "label"). Setting exportCheckboxLabel to "true" will add the checkbox value instead of using
#'   'Checked' or 'Unchecked'.
#' @param type Output is one record per row (flat) or one data point per row (eav), Default: flat
#' @param ... Optional parameters that can be passed to the REDCap API when exporting records. These may include exportSurveyFields,
#'   exportDataAccessGroups, filterLogic, dateRangeBegin, and dateRangeEnd. See REDCap API documentation for full details.
#' @return A data frame containing the extracted record
#' @details Retrieves a record from the REDCap API
#' @seealso
#'  \code{\link[httr2]{request}}, \code{\link[httr2]{req_body}}, \code{\link[httr2]{req_perform}}
#' @rdname export_record
#' @export
#' @importFrom httr2 request req_body_form req_perform

export_record <- function(
  study,
  records = NULL,
  forms = NULL,
  fields = NULL,
  events = NULL,
  format = c("csv", "json"),
  rawOrLabel = c("raw", "label"),
  rawOrLabelHeaders = c("raw", "label"),
  exportCheckboxLabel = "false",
  type = c("flat", "eav"),
  ...
) {
  format <- match.arg(format)
  rawOrLabel = match.arg(rawOrLabel)
  rawOrLabelHeaders = match.arg(rawOrLabelHeaders)
  type = match.arg(type)
  auth <- get_token(study)
  request <- httr2::request(auth$server) |>
    httr2::req_body_form(
      token = auth$token,
      content = "record",
      format = format,
      !!!get_array(records, "records"),
      !!!get_array(fields, "fields"),
      !!!get_array(forms, "forms"),
      !!!get_array(events, "events"),
      rawOrLabel = rawOrLabel,
      rawOrLabelHeaders = rawOrLabelHeaders,
      exportCheckboxLabel = exportCheckboxLabel,
      type = type,
      ...
    ) |>
    httr2::req_perform()

  class(request) <- c("httr2_response", paste0("redcap_", format))

  return(as.data.frame(request))
}
