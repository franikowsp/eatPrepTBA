#' Login credentials
#'
#' @slot domain Character. Domain of the IQB instance.
#' @slot token Character. Access token to communicate with the API (will be handled internally).
#'
#' @description
#' A class containing the token to communicate with the API of the IQB Studio Lite or the IQB Testcenter. Objects of this class will be created by the function [createStudioLogin()] or [createTestcenterLogin()].
#'
#' @export
#'
#' @name Login-class
#' @rdname Login-class
setClass("Login",
         slots = c(
           domain = "character",
           token = "character"
         ))

#' @inherit Login slot
#' @slot workspace Named character vector. Returns the labels and ids of the workspaces the user has access to.

setClass("LoginTestcenter",
         contains = "Login",
         slots = c(
           workspace = "character"
         ))

#' @inherit Login slot
#' @slot workspace Named list. Returns a list of the labels and ids of the workspaces the user has access to (nested within workspace groups).

setClass("LoginStudio",
         contains = "Login",
         slots = c(
           workspace = "character",
           workspace_groups = "list",
           version = "character"
           # TODO: Must become a list!
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

setClass("WorkspaceStudio",
         contains = "LoginStudio",
         slots = c(
           login = "LoginStudio",
           label = "character",
           id = "numeric",
           group_label = "character",
           group_id = "numeric"
         ))

setClass("WorkspaceTestcenter",
         contains = "LoginTestcenter",
         slots = c(
           login = "LoginTestcenter",
           label = "character",
           id = "numeric"
         ))
