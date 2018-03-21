#' Update Radiant
#'
#' @examples
#' \dontrun{
#' radiant.update::radiant.update()
#' }
#'
#' @importFrom rstudioapi isAvailable restartSession versionInfo
#'
#' @export
radiant.update <- function() {

  ## cleanup old session files
  unlink("~/radiant.sessions/*.rds", force = TRUE)

  ## command to run after R session is restarted
  cmd <- "source('https://raw.githubusercontent.com/radiant-rstats/minicran/gh-pages/update.R')"

  ## check if run from Rstudio
  if (rstudioapi::isAvailable()) {
    message("\nUpdating Radiant. Your R session will now restart ...")
    ## Restarting Rstudio session from http://stackoverflow.com/a/25934774/1974918
    if (rstudioapi::versionInfo()$version >= "1.1.383") {
      rstudioapi::restartSession(cmd)
    } else {
      ret <- .rs.restartR(cmd)
    }
  } else {
    message("Please restart R, copy and paste the 'source' command below into the R console, and press return\n\n", cmd)
  }
}
