#' Log in to Cognition.run via Chromote
#'
#' Opens a new Chromote browser session, navigates to the Cognition.run login
#' page, and programmatically enters the provided username and password. The
#' function clicks the "Sign in" button and returns the active session object.
#'
#' @param username Character. The email address used for Cognition.run login.
#' @param password Character. The password associated with the account.
#'
#' @return A `ChromoteSession` object that remains authenticated after login.
#'
#' @details
#' This function automates login using the `chromote` package. It:
#' \enumerate{
#'   \item Opens a new headless Chrome session.
#'   \item Navigates to the Cognition.run login page.
#'   \item Fills in the email and password fields.
#'   \item Clicks the "Sign in" button.
#' }
#'
#' Use the returned session object to perform subsequent automated browser
#' actions, such as downloading tasks with \code{\link{cognition.run_download}}.
#'
#' @examples
#' \dontrun{
#' session <- cognition.run_login("user@example.com", "mypassword")
#' }
#'
#' @importFrom chromote ChromoteSession
#'
#' @export

cognition.run_login <- function(username, password) {
  chrome_session <- chromote::ChromoteSession$new()
  chrome_session$Page$navigate("https://www.cognition.run/login")

  chrome_session$Runtime$evaluate(
    sprintf(
      'document.querySelector("input[type=\'email\']").value = "%s"',
      username
    )
  )

  chrome_session$Runtime$evaluate(
    sprintf(
      'document.querySelector("input[type=\'password\']").value = "%s"',
      password
    )
  )

  chrome_session$Runtime$evaluate(
    '
  Array.from(document.querySelectorAll("button")).find(
    btn => btn.textContent.trim() === "Sign in"
  ).click()
'
  )

  return(chrome_session)
}

#' Download a Cognition.run task export
#'
#' Navigates to a Cognition.run task’s download page using an authenticated
#' Chromote session, determines the expected filename prefix, removes any
#' existing matching files in the output directory, and programmatically
#' triggers the download.
#'
#' @param session A `ChromoteSession` object returned by
#'   \code{\link{cognition.run_login}}.
#' @param task Character. The Cognition.run task ID (the value appearing in the
#'   task URL).
#' @param outdir Character. Path to the directory where the downloaded file
#'   should be saved.
#'
#' @return (Invisibly) A character string containing the normalized expected
#'   filename prefix used by Cognition.run for the export.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Navigates to the task download page and waits for it to load.
#'   \item Extracts the export filename header from the navigation bar.
#'   \item Normalizes the filename (lowercase, underscores to hyphens).
#'   \item Removes any existing file in \code{outdir} that matches this prefix
#'         (to avoid collisions with stale exports).
#'   \item Enables Chrome download behavior and clicks the "Download" button.
#' }
#'
#' The actual downloaded file will appear in \code{outdir} according to the
#' naming conventions of Cognition.run.
#'
#' @examples
#' \dontrun{
#' session <- cognition.run_login("user@example.com", "mypassword")
#' cognition.run_download(session, task = 10234, outdir = "data/")
#' }
#'
#' @export

cognition.run_download <- function(session, task, outdir) {
  p <- session$Page$loadEventFired(wait_ = FALSE)

  session$Page$navigate(
    sprintf(
      "https://www.cognition.run/tasks/%s/download",
      task
    ),
    wait_ = FALSE
  )

  session$wait_for(p)

  result <- session$Runtime$evaluate(
    '
    document.querySelectorAll("li.nav-item.active a.nav-link")[1]?.textContent.replace(/\\s*\\/\\s*$/, "").trim()
    '
  )

  result <- gsub("_", "-", tolower(result$result$value))

  outdir_files <- list.files(outdir, full.names = TRUE)

  if (any(grepl(result, outdir_files))) {
    invisible(file.remove(outdir_files[grepl(result, outdir_files)]))
  }

  session$Browser$setDownloadBehavior(behavior = "allow", downloadPath = outdir)

  session$Runtime$evaluate(
    '
  Array.from(document.querySelectorAll("button")).find(
    btn => btn.textContent.trim() === "Download"
  ).click()
'
  )

  return(invisible(result))
}
