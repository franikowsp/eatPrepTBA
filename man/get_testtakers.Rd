% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_testtakers.R
\name{get_testtakers}
\alias{get_testtakers}
\alias{get_testtakers,WorkspaceTestcenter-method}
\title{Get a testtakers from IQB Testcenter}
\usage{
get_testtakers(workspace, files = NULL)

\S4method{get_testtakers}{WorkspaceTestcenter}(workspace, files = NULL)
}
\arguments{
\item{workspace}{\linkS4class{WorkspaceTestcenter}. Workspace information necessary to retrieve unit information and resources from the API.}

\item{files}{Character (optional). Names of the testtakers files to be retrieved. Defaults to \code{NULL} which returns all testtakers defined on the workspace.}
}
\value{
A tibble.
}
\description{
This function returns all testtakers with specified files.
}
\section{Functions}{
\itemize{
\item \code{get_testtakers(WorkspaceTestcenter)}: Get testtakers in a Testcenter workspace

}}
\keyword{internal}
