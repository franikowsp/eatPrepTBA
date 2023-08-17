#' Login credentials
#'
#' @slot domain Character. Domain of the IQB Testcenter instance.
#' @slot token Character. Access token to communicate with the IQB Testcenter API (will be handled internally).
#' @slot workspace Named character vector. Returns the labels and ids of the workspaces the user has access to.
#'
#' @description
#' A class containing the token to communicate with the Testcenter API. Objects of this class will be created by the function [createLogin()].
#'
#' @export
#'
#' @name Login-class
#' @rdname Login-class
setClass("Login",
         slots = c(
           domain = "character",
           token = "character",
           workspace = "character"
         ))

#' Workspace access
#'
#' @slot login [Login-class] created with [createLogin()].
#' @slot label Label of the workspace.
#' @slot id ID of the workspace. See URL of the workspace.
#'
#' @description
#' A class containing the token to communicate with the Testcenter API. Objects of this class will be created by the function [accessWorkspace()] after entering a valid [Login-class] object.
#'
#' @export
#'
#' @name Workspace-class
#' @rdname Workspace-class
setClass("Workspace",
         contains = "Login",
         slots = c(
           login = "Login",
           label = "character",
           id = "numeric"
         ))
