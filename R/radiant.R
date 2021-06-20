#' Update Radiant
#'
#' @description Update radiant and dependencies to the latest (supported) versions
#'
#' @param lib.loc Library to install packages in (see .libPaths())
#' @param repos Repo to update from (default is the radiant repo on GitHub)
#' @param type Package type ("binary" or "source"). If missing, will try infer from OS type (i.e., "source" for linux, else "binary")
#' @param options Install options as a string to pass to install.packages (e.g., '--no-lock')
#'
#'
#' @examples
#' \dontrun{
#' radiant.update::radiant.update()
#' }
#'
#' @export
radiant.update <- function(
  lib.loc = .libPaths()[1],
  repos = "",
  type,
  options = ""
) {

  if (repos == "") {
    os <- Sys.info()["sysname"]
    if (os == "Linux") {
      options(HTTPUserAgent = sprintf("R/%s R (%s)", getRversion(), paste(getRversion(), R.version$platform, R.version$arch, R.version$os)))
      # repos <- c(RSM = "https://rsm-compute-01.ucsd.edu:4242/rsm-msba/__linux__/focal/latest", RSPM = "https://packagemanager.rstudio.com/all/__linux__/focal/latest", CRAN = "https://cloud.r-project.org")
      repos <- c(RSM = "https://rsm-compute-01.ucsd.edu:4242/rsm-msba/__linux__/focal/latest")
    } else {
      # repos <- c(RSM = "https://radiant-rstats.github.io/minicran/", CRAN = "https://cloud.r-project.org")
      repos <- c(RSM = "https://radiant-rstats.github.io/minicran/")
    }
  }

  ## remove old sessions directory
  unlink("~/radiant.sessions", recursive = TRUE, force = TRUE)
  ## cleanup old session files
  unlink("~/.radiant.sessions/*.state.rda", force = TRUE)

  ## https://stackoverflow.com/questions/50422627/different-results-from-deparse-in-r-3-4-4-and-r-3-5
  dctrl <- if (getRversion() > "3.4.4") c("keepNA", "niceNames") else "keepNA"

  ## unload pkgs if needed
  unload_pkgs()

  if (is.null(Sys.getenv("RSTUDIO")) && length(sessionInfo()$otherPkgs) > 0) {
    message("Some packages are already loaded. Please restart R and run radiant.update::radiant.update() again")
  } else {
    if (missing(type)) {
      os_type <- Sys.info()["sysname"]
      type <- ifelse(os_type %in% c("Windows", "Darwin"), "binary", "source")
    }
    to_install <- old.packages(
      lib.loc = lib.loc,
      repos = repos,
      type = type
    )[, "Package"]

    pkgs_avail <- available.packages(repos = repos, type = type)[, "Version"]
    if (length(pkgs_avail) == 0) {
      ## may occur when a new version of R comes out
      repos <- "https://cloud.r-project.org/"
      to_install <- unique(c(to_install, "radiant"))
    } else {
      pkgs_inst <- installed.packages(lib.loc = lib.loc)[, "Version"]
      to_install <- c(to_install, names(pkgs_avail[!names(pkgs_avail) %in% names(pkgs_inst)]))
    }

    if (length(to_install) > 0) {
      to_install <- paste0("c(", paste0("\"", to_install, "\"", collapse = ", "), ")")
      ## needed in case Rstudio wants to restart because package is loaded
      to_run <- paste0(
        "install.packages(", to_install,
        ", lib = ", deparse(lib.loc),
        ", repos = ", deparse(repos, control = dctrl, width.cutoff = 500L),
        ", type = ", deparse(type),
        ", INSTALL_opts =", deparse(options),
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
}

#' Check if the radiant package can be loaded
#' @export
radiant.check <- function(type = "update") {
  message('\nTesting if Radiant can be loaded ...')
  ret <- try(eval(parse(text = "suppressMessages(requireNamespace('radiant'))")), silent = TRUE)
  if (isTRUE(ret)) {
    message(paste0("\nRadiant ", type, " was successful\n"))
  } else {
    message(paste0("\nRadiant ", type, " attempt was unsuccessful. Please restart R(studio) and run the update (radiant.update::radiant.update()) or sync (radiant.update::sync_packages()) command again. If update (sync) is still not successful, please send an email to radiant@rady.ucsd.edu with screen shots of the output shown in R(studio)."))
  }
}

#' Sync packages to the version on the miniCRAN repo
#'
#' @param lib.loc Library to install packages in (see .libPaths())
#' @param repos Repo to update from (default is the radiant repo on GitHub)
#' @param type Package type ("binary" or "source"). If missing, will try infer from OS type (i.e., "source" for linux, else "binary")
#' @param options Install options as a string to pass to install.packages (e.g., '--no-lock')
#'
#' @examples
#' \dontrun{
#' radiant.update::sync_packages()
#' }
#'
#' @export
sync_packages <- function(
  lib.loc = .libPaths()[1],
  repos = "",
  type,
  options = ""
) {

  if (repos == "") {
    os <- Sys.info()["sysname"]
    if (os == "Linux") {
      options(HTTPUserAgent = sprintf("R/%s R (%s)", getRversion(), paste(getRversion(), R.version$platform, R.version$arch, R.version$os)))
      # repos <- c(RSM = "https://rsm-compute-01.ucsd.edu:4242/rsm-msba/__linux__/focal/latest", RSPM = "https://packagemanager.rstudio.com/all/__linux__/focal/latest", CRAN = "https://cloud.r-project.org")
      repos <- c(RSM = "https://rsm-compute-01.ucsd.edu:4242/rsm-msba/__linux__/focal/latest")
    } else {
      # repos <- c(RSM = "https://radiant-rstats.github.io/minicran/", CRAN = "https://cloud.r-project.org")
      repos <- c(RSM = "https://radiant-rstats.github.io/minicran/")
    }
  }

  ## remove old sessions directory
  unlink("~/radiant.sessions", recursive = TRUE, force = TRUE)
  ## cleanup old session files
  unlink("~/.radiant.sessions/*.state.rda", force = TRUE)

  ## https://stackoverflow.com/questions/50422627/different-results-from-deparse-in-r-3-4-4-and-r-3-5
  dctrl <- if (getRversion() > "3.4.4") c("keepNA", "niceNames") else "keepNA"

  ## unload pkgs if needed
  unload_pkgs()

  if (is.null(Sys.getenv("RSTUDIO")) && length(sessionInfo()$otherPkgs) > 0) {
    message("Some packages are already loaded. Please restart R and run radiant.update::sync_packages() again")
  } else {
    if (missing(type)) {
      os_type <- Sys.info()["sysname"]
      type <- ifelse(os_type %in% c("Windows", "Darwin"), "binary", "source")
    }

    pkgs_avail <- available.packages(repos = repos, type = type)[, "Version"]
    if (length(pkgs_avail) == 0) {
      ## may occur when a new version of R comes out
      repos <- "https://cloud.r-project.org/"
      to_install <- "radiant"
    } else {
      pkgs_inst <- installed.packages(lib.loc = lib.loc)[, "Version"]
      to_install <- names(pkgs_avail[!names(pkgs_avail) %in% names(pkgs_inst)])
    }

    if (length(to_install) > 0) {
      ## needed in case Rstudio wants to restart because package is loaded
      to_install <- paste0("c(", paste0("\"", to_install, "\"", collapse = ", "), ")")
      to_run <- paste0(
        "install.packages(", to_install,
        ", lib = ", deparse(lib.loc),
        ", repos = ", deparse(repos, control = dctrl, width.cutoff = 500L),
        ", type = ", deparse(type),
        ", INSTALL_opts =", deparse(options),
        "\")"
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
    radiant.check("sync")
  }
}


#' Check which packages are installed in a user's local packages directory
#'
#' @export
user_packages <- function() {
  local <- Sys.getenv("R_LIBS_USER")
  ipn <- c()
  if (dir.exists(local)) {
    ip <- installed.packages()
    ipl <- ip[,"LibPath"] == normalizePath(local)
    ipn <- names(ip[,"Package"])[ipl]
    if (length(ipn) == 0) {
      cat("No user-level local R-packages installed")
    } else {
      cat("The following packages are installed locally at the user level\n")
      ipv <- ip[,"Version"][ipl]
      cat(paste0(ipn, "_", ipv, collapse = "\n"))
    }
  } else {
    cat("No user-level local R-packages installed")
  }
  invisible(list(packages = ipn, local = local))
}

#' Remove packages that are installed in a user's local packages directory
#'
#' @export
remove_user_packages <- function() {
  up <- user_packages()
  if (length(up$packages) == 0) {
    cat("\nNo user-level local R-packages to remove")
  } else {
    cat("\nUser-level local R-packages were removed")
    remove.packages(up$packages, up$local)
  }
}
