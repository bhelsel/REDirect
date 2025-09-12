#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param study A study acronym or name to select the token from a user's R environment (stored as STUDY_TOKEN)
#' @param records PARAM_DESCRIPTION, Default: NULL
#' @param forms PARAM_DESCRIPTION, Default: NULL
#' @param fields PARAM_DESCRIPTION, Default: NULL
#' @param events PARAM_DESCRIPTION, Default: NULL
#' @param format PARAM_DESCRIPTION, Default: c("csv", "json")
#' @param rawOrLabel PARAM_DESCRIPTION, Default: c("raw", "label")
#' @param rawOrLabelHeaders PARAM_DESCRIPTION, Default: c("raw", "label")
#' @param exportCheckboxLabel PARAM_DESCRIPTION, Default: 'false'
#' @param type PARAM_DESCRIPTION, Default: c("flat", "eav")
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
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
