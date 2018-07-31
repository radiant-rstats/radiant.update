#' Update Radiant
#'
#' @param lib.loc Library to install packages in (see .libPaths())
#' @param repos Repo to update from (default is the radiant repo on GitHub)
#' @param dev If TRUE, add the radiant development repo to the repo list
#' @param type Package type ("binary" or "source"). If missing, will try infer from OS type (i.e., "source" for linux, else "binary")
#'
#' @examples
#' \dontrun{
#' radiant.update::radiant.update()
#' }
#'
#' @export
radiant.update <- function(
  lib.loc = .libPaths()[1],
  repos = "https://radiant-rstats.github.io/minicran/",
  dev = FALSE,
  type
) {

  ## cleanup old session files
  unlink("~/radiant.sessions/*.rds", force = TRUE)

  ## https://stackoverflow.com/questions/50422627/different-results-from-deparse-in-r-3-4-4-and-r-3-5
  dctrl <- if (getRversion() > "3.4.4") c("keepNA", "niceNames") else "keepNA"

  ## unload pkgs if needed
  unload_pkgs()

  if (is.null(Sys.getenv("RSTUDIO")) && length(sessionInfo()$otherPkgs) > 0) {
    message("Some packages are already loaded. Please restart R and run radiant.update::radiant.update() again")
  } else {
    if (dev) {
      repos <- c(repos, "https://radiant-rstats.github.io/minicran/dev")
    }
    if (missing(type)) {
      os_type <- Sys.info()["sysname"]
      type <- ifelse(os_type %in% c("Windows", "Darwin"), "binary", "source")
    }
    to_install <- old.packages(
      lib.loc = lib.loc,
      repos = repos,
      type = type
    )[, "Package"]

    pkgs_inst <- installed.packages(lib.loc = lib.loc)[, "Version"]
    pkgs_avail <- available.packages(repos = repos, type = type)[, "Version"]
    to_install <- c(to_install, names(pkgs_avail[!names(pkgs_avail) %in% names(pkgs_inst)]))

    if (length(to_install) > 0) {
      to_install <- paste0("c(", paste0("\"", to_install, "\"", collapse = ", "), ")")
      ## needed in case Rstudio wants to restart because package is loaded
      to_run <- paste0(
        "install.packages(",
        to_install, ", lib = ",
        deparse(lib.loc), ", repos = ",
        deparse(repos, control = dctrl, width.cutoff = 500L), ", type = ",
        deparse(type),
        "); radiant.check()"
      )
      to_run <- try(eval(parse(text = to_run)), silent = TRUE)
    } else {
      message("Nothing to update")
    }
  }
}

## based on https://rtask.thinkr.fr/blog/our-shiny-template-to-design-a-prod-ready-app/?noredirect=en_US
#' Unload packages
#' @export
unload_pkgs <- function() {
  ops <- function() sessionInfo()$otherPkgs
  if (length(ops()) > 0) {
    suppressWarnings(
      sapply(
        paste0("package:", names(ops())),
        detach,
        character.only = TRUE,
        unload = TRUE
      )
    )
  }

  # unload_namespaces <- function(lo) {
  #   ret <- sapply(
  #     setdiff(names(lo), c("radiant.update", "compiler", "tools", "packrat")),
  #     function(x) try(unloadNamespace(x), silent = TRUE)
  #   )
  #   lor <- lo()
  #   if (length(lor) > 4) {
  #     unload_namespaces(lor)
  #   }
  # }
  #
  # lo <- function() sessionInfo()$loadedOnly
  # if (length(lo()) > 4) {
  #   unload_namespaces(lo)
  # }
}

#' Check if the radiant package can be loaded
#' @export
radiant.check <- function() {
  message('\nTesting if Radiant can be loaded ...')
  ret <- try(eval(parse(text = "suppressMessages(requireNamespace('radiant'))")), silent = TRUE)
  if (isTRUE(ret)) {
    message("\nRadiant update was successfull\n")
  } else {
    message("\nRadiant update attempt was unsuccessful. Please restart R(studio) and run the update (radiant.update::radiant.update()) or sync (radiant.update::sync_packages()) command again. If update (sync) is still not successful, please send an email to radiant@rady.ucsd.edu with screen shots of the output shown in R(studio).")
  }
}

#' Sync packages to the version on the miniCRAN repo
#'
#' @param lib.loc Library to install packages in (see .libPaths())
#' @param repos Repo to update from (default is the radiant repo on GitHub)
#' @param dev If TRUE, add the radiant development repo to the repo list
#' @param type Package type ("binary" or "source"). If missing, will try infer from OS type (i.e., "source" for linux, else "binary")
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
  dev = FALSE,
  type
) {

  ## cleanup old session files
  unlink("~/radiant.sessions/*.rds", force = TRUE)

  ## https://stackoverflow.com/questions/50422627/different-results-from-deparse-in-r-3-4-4-and-r-3-5
  dctrl <- if (getRversion() > "3.4.4") c("keepNA", "niceNames") else "keepNA"

  ## unload pkgs if needed
  unload_pkgs()

  if (is.null(Sys.getenv("RSTUDIO")) && length(sessionInfo()$otherPkgs) > 0) {
    message("Some packages are already loaded. Please restart R and run radiant.update::sync_packages() again")
  } else {
    if (dev) {
      repos <- c(repos, "https://radiant-rstats.github.io/minicran/dev")
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
        ", repos = ", deparse(repos, control = dctrl, width.cutoff = 500L),
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
        ", repos = ", deparse(repos, control = dctrl, width.cutoff = 500L),
        ", type = ", deparse(type), ")"
      )
      try(eval(parse(text = to_run)), silent = TRUE)
    }
    radiant.check()
  }
}


