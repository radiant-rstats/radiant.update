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
  mess <- paste0("Some packages are already loaded. Please restart R (Rstudio) and run radiant.update::radiant.update() again")

  if (length(search()) < 11) {
    ret <- try(source('https://raw.githubusercontent.com/radiant-rstats/minicran/gh-pages/update.R'), silent = FALSE)
    if (is(ret, "try-error")) {
      message(mess)
    }
  } else {
    ## check if run from Rstudio
    if (rstudioapi::isAvailable()) {
      message("\nUpdating Radiant. Your R session will now restart ...")
      ## Restarting Rstudio session from http://stackoverflow.com/a/25934774/1974918
      if (rstudioapi::versionInfo()$version >= "1.1.383") {
        rstudioapi::restartSession(cmd)
        # rstudioapi::restartSession("radiant.update::radiant.update()")
      } else {
        ret <- .rs.restartR(cmd)
        # ret <- .rs.restartR("radiant.update::radiant.update()")
      }
    } else {
      message(mess)
    }
  }
}
