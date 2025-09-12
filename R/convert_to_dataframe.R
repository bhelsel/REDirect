#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @seealso
#'  \code{\link[httr2]{resp_body_raw}}
#' @rdname as.data.frame.redcap_csv
#' @export
#' @importFrom httr2 resp_body_string

as.data.frame.redcap_csv <- function(x, ...) {
  resp <- utils::read.csv(
    text = httr2::resp_body_string(x),
    stringsAsFactors = FALSE,
    ...
  )
  class(resp) <- c("tbl_df", "tbl", "data.frame")
  return(resp)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @seealso
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#'  \code{\link[httr2]{resp_body_raw}}
#' @rdname as.data.frame.redcap_json
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom httr2 resp_body_string

as.data.frame.redcap_json <- function(x, ...) {
  resp <- jsonlite::fromJSON(httr2::resp_body_string(x), flatten = TRUE, ...)
  class(resp) <- c("tbl_df", "tbl", "data.frame")
  return(resp)
}
