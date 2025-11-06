#' @title check_record
#' @description A validation check when importing records to REDCap to ensure no data is being overwritten
#' @param study The study name or abbreviation that matches the API token stored in a user's R environment.
#'   The token should be stored as the study name or abbreviation followed by _TOKEN.
#' @param data A data frame containing the data to upload to REDCap
#' @return A data frame of any duplicates of NULL if no duplicates are found
#' @details A validation check when importing records to REDCap to ensure no data is being overwritten
#' @seealso
#'  \code{\link[dplyr]{rename}}, \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{reexports}}, \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{select}}
#' @rdname check_record
#' @importFrom dplyr rename distinct across all_of inner_join select any_of %>%
#' @keywords internal

check_record <- function(study, data) {
  cols2check <- c(
    "record_id",
    "redcap_event_name",
    "redcap_repeat_instrument",
    "redcap_repeat_instance"
  )
  exportEAV <- ifelse("redcap_repeat_instrument" %in% names(data), TRUE, FALSE)

  record <- tryCatch(
    {
      export_record(
        study = study,
        records = unique(data$record_id),
        forms = unique(data$redcap_repeat_instrument),
        format = "csv",
        type = ifelse(exportEAV, "eav", "flat")
      )
    },
    error = function(e) {
      return(NULL)
    }
  )

  missing_event_name <- !unique(record$redcap_event_name) %in%
    unique(data$redcap_event_name)

  if (
    is.null(record) || missing_event_name || rlang::is_empty(missing_event_name)
  ) {
    return(NULL)
  }

  matV <- names(data)[which(cols2check %in% names(data))]
  addV <- names(data)[-which(cols2check %in% names(data))]

  if (exportEAV) {
    if (!all(data$redcap_repeat_instance == "new")) {
      data <- data[!is.character(data$redcap_repeat_instance), ]
      if (nrow(data) > 0) {
        data$redcap_repeat_instance <- as.numeric(data$redcap_repeat_instance)
        duplicates <- record %>%
          dplyr::rename(record_id = record) %>%
          dplyr::distinct(dplyr::across(dplyr::all_of(cols2check))) %>%
          dplyr::inner_join(data[, matV], by = matV)
      }
    } else {
      duplicates <- data.frame()
    }
  } else {
    duplicates <- dplyr::inner_join(
      dplyr::select(data, dplyr::any_of(matV), addV),
      dplyr::select(record, dplyr::any_of(matV), addV),
      by = matV
    )
    colnames(duplicates) <- gsub(".x$", "_input", colnames(duplicates))
    colnames(duplicates) <- gsub(".y$", "_redcap", colnames(duplicates))
  }

  if (nrow(duplicates) > 0) {
    return(tidyr::drop_na(duplicates))
  } else {
    return(NULL)
  }
}
