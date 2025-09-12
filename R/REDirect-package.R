#' @keywords internal
"_PACKAGE"

#' @title REDirect: R Package for Interacting with the REDCap API
#'
#' @description This package provides a streamlined interface for working with REDCap
#'     projects through the API. It allows users to export records and reports,
#'     import new data, and manage repeating instruments and events with built-in
#'     checks to prevent overwriting data In addition to core REDCap data management
#'     functions, the package includes ecological momentary assessment (EMA) utilities
#'     such as random schedule generation and time-based prompts, making it easier to
#'     design, deploy, and manage intensive longitudinal studies in REDCap.
#'
#' @section REDirect REDCap Functions:
#'
#' \code{\link{export_metadata}}
#'
#' \code{\link{export_record}}
#'
#' \code{\link{export_reports}}
#'
#' \code{\link{import_record}}
#'
#' @section REDirect EMA Functions:
#'
#' \code{\link{generate_prompt_schedule}}
#'
#' @name REDirect

NULL
