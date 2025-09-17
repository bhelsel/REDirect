#' @title export_metadata
#' @description Exports the metadata or codebook from the REDCap API
#' @param study A study acronym or name to select the token from a user's R environment (stored as STUDY_TOKEN)
#' @param format The file type to retrieve from REDCap (choices include csv or json), Default: csv
#' @param forms An array of form names specifying specific data collection instruments for which you wish to pull
#'   metadata (by default, all metadata is pulled), Default: NULL
#' @param fields An array of field names specifying specific fields you wish to pull (by default, all metadata is pulled), Default: NULL
#' @return A data frame containing the codebook from the REDCap project
#' @details Exports the metadata or codebook from the REDCap API
#' @seealso
#'  \code{\link[httr2]{request}}, \code{\link[httr2]{req_body}}, \code{\link[httr2]{req_perform}}
#' @rdname export_metadata
#' @export
#' @importFrom httr2 request req_body_form req_perform

export_metadata <- function(
  study,
  format = c("csv", "json"),
  forms = NULL,
  fields = NULL
) {
  format <- match.arg(format)
  auth <- get_token(study)

  request <- httr2::request(auth$server) %>%
    httr2::req_body_form(
      token = auth$token,
      content = "metadata",
      format = format,
      !!!get_array(fields, "fields"),
      !!!get_array(forms, "forms")
    ) %>%
    httr2::req_perform()

  class(request) <- c("httr2_response", paste0("redcap_", format))

  return(as.data.frame(request))
}
