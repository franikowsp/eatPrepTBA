% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/0AllClass.R
\docType{class}
\name{Workspace-class}
\alias{Workspace-class}
\title{Workspace access}
\description{
A class containing the token to communicate with the Testcenter API. Objects of this class will be created by the function \code{\link[=access_workspace]{access_workspace()}} after entering a valid \linkS4class{Login} object.
}
\section{Slots}{

\describe{
\item{\code{ws_id}}{ID of the workspace. The workspace ID can also be found in the workspace URL.}

\item{\code{ws_label}}{Label of the workspace.}
}}

