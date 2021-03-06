% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/radiant.R
\name{sync_packages}
\alias{sync_packages}
\title{Sync packages to the version on the miniCRAN repo}
\usage{
sync_packages(lib.loc = .libPaths()[1], repos = "", type, options = "")
}
\arguments{
\item{lib.loc}{Library to install packages in (see .libPaths())}

\item{repos}{Repo to update from (default is the radiant repo on GitHub)}

\item{type}{Package type ("binary" or "source"). If missing, will try infer from OS type (i.e., "source" for linux, else "binary")}

\item{options}{Install options as a string to pass to install.packages (e.g., '--no-lock')}
}
\description{
Sync packages to the version on the miniCRAN repo
}
\examples{
\dontrun{
radiant.update::sync_packages()
}

}
