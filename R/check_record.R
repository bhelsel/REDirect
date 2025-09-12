#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param study PARAM_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @seealso
#'  \code{\link[dplyr]{rename}}, \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{reexports}}, \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{select}}
#' @rdname check_record
#' @export
#' @importFrom dplyr rename distinct across all_of inner_join select any_of %>%

check_record <- function(study, data) {
  cols2check <- c(
    "record_id",
    "redcap_event_name",
    "redcap_repeat_instrument",
    "redcap_repeat_instance"
  )
  exportEAV <- ifelse("redcap_repeat_instrument" %in% names(data), TRUE, FALSE)
  record <- export_record(
    study = study,
    records = unique(data$record_id),
    forms = unique(data$redcap_repeat_instrument),
    format = "csv",
    type = ifelse(exportEAV, "eav", "flat")
  )

  matV <- names(data)[which(cols2check %in% names(data))]
  addV <- names(data)[-which(cols2check %in% names(data))]

  if (exportEAV) {
    duplicates <- record %>%
      dplyr::rename(record_id = record) %>%
      dplyr::distinct(dplyr::across(dplyr::all_of(cols2check))) %>%
      dplyr::inner_join(data[, matV], by = matV)
  } else {
    duplicates <- dplyr::inner_join(
      dplyr::select(data, dplyr::any_of(matV), addV),
      dplyr::select(record, dplyr::any_of(matV), addV),
      by = c(matV, addV)
    )
  }

  if (nrow(duplicates) > 0) {
    return(duplicates)
  } else {
    return(NULL)
  }
}
