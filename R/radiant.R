#' Update Radiant
#'
#' @examples
#' \dontrun{
#' radiant.update::radiant.update()
#' }
#'
#' @importFrom rstudioapi isAvailable restartSession versionInfo
#' @importFrom methods is
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
      message("\nUpdating packages. Your R session will now restart ...")
      ## Restarting Rstudio session from http://stackoverflow.com/a/25934774/1974918

      ## Important issue: Restart no longer clears as per issue #95
      ## https://github.com/rstudio/rstudioapi/issues/95
      if (rstudioapi::versionInfo()$version >= "1.1.383") {
        rstudioapi::restartSession(cmd)
      } else {
        ret <- .rs.restartR(cmd)
      }
    } else {
      message(mess)
    }
  }
}

#' Sync packages to the version on the miniCRAN repo
#'
#' @param lib.loc Library to install packages in
#' @param type Package type
#'
#' @examples
#' \dontrun{
#' radiant.update::sync_packages()
#' }
#'
#' @export
sync_packages <- function(lib.loc = .libPaths()[1], type) {

  ## cleanup old session files
  unlink("~/radiant.sessions/*.rds", force = TRUE)
  # repos <- c(
  #   "https://radiant-rstats.github.io/minicran/",
  #   "https://radiant-rstats.github.io/minicran/dev"
  # )
  repos <- "https://radiant-rstats.github.io/minicran/"
  if (length(search()) > 10) {
    message("Some packages are already loaded. Please restart R (Rstudio) and run radiant.update::sync_packages() again")
  } else {
    if (missing(type)) {
      os_type <- Sys.info()["sysname"]
      type <- ifelse(os_type %in% c("Windows", "Darwin"), "binary", "source")
    }

    pkgs_inst <- installed.packages(lib.loc = lib.loc)[, "Version"]
    pkgs_avail <- available.packages(repos = repos, type = type)[, "Version"]
    to_install <- names(pkgs_avail[!names(pkgs_avail) %in% names(pkgs_inst)])
    to_install
    if (length(to_install) > 0) {
      # install.packages(to_install, lib = lib.loc, repos = repos, type = type)
      to_run <- paste0(
        "install.packages(",
        deparse(to_install, control = "keepNA", width.cutoff = 500L), ", lib = ",
        deparse(lib.loc), ", repos = ",
        deparse(repos, control = "keepNA", width.cutoff = 500L), ", type = ",
        deparse(type),
        ")"
      )
      # cat(to_run)
      try(eval(parse(text = to_run)), silent = TRUE)
      # eval(parse(text = to_run))
    }

    ## updating pkgs_installed list
    pkgs_inst <- installed.packages(lib.loc = lib.loc)[, "Version"]
    pkgs_comp <- data.frame(
      pkgs = names(pkgs_avail),
      avail = pkgs_avail,
      inst = pkgs_inst[names(pkgs_avail)],
      stringsAsFactors = FALSE
    )

    to_sync <- apply(pkgs_comp, 1, function(x) compareVersion(x[2], x[3]))
    names(to_sync) <- pkgs_comp$pkgs
    to_sync <- names(to_sync[to_sync != 0])
    if (length(to_sync) > 0) {
      # install.packages(to_sync, lib = lib.loc, repos = repos, type = type)
      to_run <- paste0(
        "install.packages(",
        deparse(to_sync, control = "keepNA", width.cutoff = 500L), ", lib = ",
        deparse(lib.loc), ", repos = ",
        deparse(repos, control = "keepNA", width.cutoff = 500L), ", type = ",
        deparse(type),
        ")"
      )
      # cat(to_run)
      try(eval(parse(text = to_run)), silent = TRUE)
      # eval(parse(text = to_run))
    }
  }
}


