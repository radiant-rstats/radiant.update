#' Update Radiant
#'
#' @param lib.loc Library to install packages in (see .libPaths())
#' @param repos Repo to update from (default is the radiant repo on GitHub)
#' @param type Package type ("binary" or "source")
#' @param dev If TRUE, add the radiant development repo to the repo list
#'
#' @examples
#' \dontrun{
#' radiant.update::radiant.update()
#' }
#'
#' @importFrom utils old.packages
#'
#' @export
radiant.update <- function(
  lib.loc = .libPaths()[1],
  repos = "https://radiant-rstats.github.io/minicran/",
  type,
  dev = FALSE
) {

  ## cleanup old session files
  unlink("~/radiant.sessions/*.rds", force = TRUE)

  if (is.null(Sys.getenv("RSTUDIO")) && (length(search()) > 10)) {
    message("Some packages are already loaded. Please restart R and run radiant.update::sync_packages() again")
  } else {
    if (dev) {
      repo <- c(repo, "https://radiant-rstats.github.io/minicran/dev")
    }
    if (missing(type)) {
      os_type <- Sys.info()["sysname"]
      type <- ifelse(os_type %in% c("Windows", "Darwin"), "binary", "source")
    }
    # ret <- try(source('https://raw.githubusercontent.com/radiant-rstats/minicran/gh-pages/update.R'), silent = FALSE)
    to_install <- old.packages(
      lib.loc = lib.loc,
      repos = repos,
      type = type
    )[, "Package"]

    if (length(to_install) > 0) {
      to_install <- paste0("c(", paste0("\"", to_install, "\"", collapse = ", "), ")")
      ## needed in case Rstudio wants to restart because package is loaded
      to_run <- paste0(
        "install.packages(",
        to_install, ", lib = ",
        deparse(lib.loc), ", repos = ",
        deparse(repos, control = "keepNA", width.cutoff = 500L), ", type = ",
        deparse(type),
        ")"
      )
      to_run <- try(eval(parse(text = to_run)), silent = TRUE)
    } else {
      message("Nothing to update")
    }
  }
}

radiant.check <- function() {
  message('\nTesting if Radiant can be loaded ...')
  success <- "\nRadiant update was successfull\n"
  failure <- "
    Radiant update attempt was unsuccessful. Please run
    the update (radiant.update::radiant.update()) or
    sync (radiant.update::sync_packages()) command again.
    If update (sync) is still not successful, please send
    an email to radiant@rady.ucsd.edu with screen shots
    of the output shown in R(studio)."
  # ret <- suppressPackageStartupMessages(require("radiant"))
  ret <- try(eval(parse(text = "suppressMessages(requireNamespace('radiant'))")), silent = TRUE)
  if (isTRUE(ret)) {
    message(success)
  } else {
    message(failure)
  }
}

#' Sync packages to the version on the miniCRAN repo
#'
#' @param lib.loc Library to install packages in (see .libPaths())
#' @param repos Repo to update from (default is the radiant repo on GitHub)
#' @param type Package type ("binary" or "source")
#' @param dev If TRUE, add the radiant development repo to the repo list
#'
#' @examples
#' \dontrun{
#' radiant.update::sync_packages()
#' }
#'
#' @export
sync_packages <- function(
  lib.loc = .libPaths()[1],
  repos = "https://radiant-rstats.github.io/minicran/",
  type,
  dev = FALSE
) {

  ## cleanup old session files
  unlink("~/radiant.sessions/*.rds", force = TRUE)

  if (is.null(Sys.getenv("RSTUDIO")) && (length(search()) > 10)) {
    message("Some packages are already loaded. Please restart R and run radiant.update::sync_packages() again")
  } else {
    if (dev) {
      repo <- c(repo, "https://radiant-rstats.github.io/minicran/dev")
    }
    if (missing(type)) {
      os_type <- Sys.info()["sysname"]
      type <- ifelse(os_type %in% c("Windows", "Darwin"), "binary", "source")
    }
    pkgs_inst <- installed.packages(lib.loc = lib.loc)[, "Version"]
    pkgs_avail <- available.packages(repos = repos, type = type)[, "Version"]
    to_install <- names(pkgs_avail[!names(pkgs_avail) %in% names(pkgs_inst)])
    if (length(to_install) > 0) {
      ## needed in case Rstudio wants to restart because package is loaded
      to_install <- paste0("c(", paste0("\"", to_install, "\"", collapse = ", "), ")")
      to_run <- paste0(
        "install.packages(", to_install,
        ", lib = ", deparse(lib.loc),
        ", repos = ", deparse(repos, control = "keepNA", width.cutoff = 500L),
        ", type = \"", type, "\")"
      )
      try(eval(parse(text = to_run)), silent = TRUE)
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
      ## needed in case Rstudio wants to restart because package is loaded
      to_sync <- paste0("c(", paste0("\"", to_sync, "\"", collapse = ", "), ")")
      to_run <- paste0(
        "install.packages(",
        to_sync,
        ", lib = ", deparse(lib.loc),
        ", repos = ", deparse(repos, control = "keepNA", width.cutoff = 500L),
        ", type = ", deparse(type), ")"
      )
      try(eval(parse(text = to_run)), silent = TRUE)
    }
  }
}


