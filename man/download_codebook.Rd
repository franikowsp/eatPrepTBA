% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_codebook.R
\name{download_codebook}
\alias{download_codebook}
\alias{download_codebook,WorkspaceStudio-method}
\title{Download codebook}
\usage{
download_codebook(
  workspace,
  path,
  unit_keys = NULL,
  format = "docx",
  missingsProfile = NULL,
  general_instructions = TRUE,
  only_manual = TRUE,
  closed = TRUE,
  derived = TRUE,
  show_score = FALSE,
  code_label_to_upper = TRUE
)

\S4method{download_codebook}{WorkspaceStudio}(
  workspace,
  path,
  unit_keys = NULL,
  format = "docx",
  missingsProfile = NULL,
  general_instructions = TRUE,
  only_manual = TRUE,
  closed = TRUE,
  derived = TRUE,
  show_score = FALSE,
  code_label_to_upper = TRUE
)
}
\arguments{
\item{workspace}{\linkS4class{WorkspaceStudio}. Workspace information necessary to download codebook via the API.}

\item{path}{Character. Path for the codebook file to be downloaded.}

\item{unit_keys}{Character. Keys (short names) of the units in the workspace the codebook should be retrieved from. If set to \code{NULL} (default), the codebook will be generated for the all units.}

\item{format}{Character. Either \code{"docx"} (default) or \code{json}.}

\item{missingsProfile}{Missings profile. (Currently with no effect.)}

\item{general_instructions}{Logical. Should the general coding instructions be printed? Defaults to \code{TRUE}.}

\item{only_manual}{Logical. Should only items with manual coding be printed? Defaults to \code{TRUE}.}

\item{closed}{Logical. Should items that could be automatically coded be printed? Defaults to \code{TRUE}.}

\item{derived}{Logical. Should the derived variables be printed? Defaults to \code{TRUE}.}

\item{show_score}{Logical. Should the score be printed? Defaults to \code{FALSE}.}

\item{code_label_to_upper}{Logical. Should the code labels be printed in capital letters? Defaults to \code{TRUE}.}
}
\description{
This function downloads codebooks from the IQB Studio.
}
\section{Functions}{
\itemize{
\item \code{download_codebook(WorkspaceStudio)}: Upload a file in a defined workspace

}}
