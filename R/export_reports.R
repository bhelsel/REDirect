#' @title export_reports
#' @description Retrieves a report from the REDCap API
#' @param study A study acronym or name to select the token from a user's R environment (stored as STUDY_TOKEN)
#' @param report_id A numeric value representing the REDCap report to be downloaded to the user's R environment
#' @param format The file type to retrieve from REDCap (choices include csv or json), Default: csv
#' @param rawOrLabel Export the raw coded values or labels for the options of multiple choice fields, Default: raw
#' @param rawOrLabelHeaders Export the variable or field names (raw) or the field labels (label). Labels can only
#'   be exported if format is csv, Default: raw
#' @param exportCheckboxLabel Specifies the format of checkbox when exporting the data as labels (i.e., when
#'   rawOrLabel = "label"). Setting exportCheckboxLabel to "true" will add the checkbox value instead of using
#'   'Checked' or 'Unchecked'.
#' @return A data frame containing the extracted report
#' @details Retrieves a report from the REDCap API
#' @seealso
#'  \code{\link[httr2]{request}}, \code{\link[httr2]{req_body}}, \code{\link[httr2]{req_perform}}
#' @rdname export_reports
#' @export
#' @importFrom httr2 request req_body_form req_perform

export_reports <- function(
  study,
  report_id,
  format = c("csv", "json"),
  rawOrLabel = c("raw", "label"),
  rawOrLabelHeaders = c("raw", "label"),
  exportCheckboxLabel = "false"
) {
  format <- match.arg(format)
  rawOrLabel = match.arg(rawOrLabel)
  rawOrLabelHeaders = match.arg(rawOrLabelHeaders)
  auth <- get_token(study)
  request <- httr2::request(auth$server) |>
    httr2::req_body_form(
      token = auth$token,
      content = "report",
      format = format,
      report_id = report_id,
      rawOrLabel = rawOrLabel,
      rawOrLabelHeaders = rawOrLabelHeaders,
      exportCheckboxLabel = exportCheckboxLabel
    ) |>
    httr2::req_perform()

  class(request) <- c("httr2_response", paste0("redcap_", format))

  return(as.data.frame(request))
}
