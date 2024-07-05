#' Login credentials
#'
#' @slot base_url Character. Base URL of the instance.
#' @slot token Character. Access token to communicate with the API (will be handled internally).
#' @slot base_call Function. Base [httr2] request (will be handled internally).
#' @slot ws_list Named list. Returns a list of the labels and ids of the workspaces the user has access to.
#'
#' @description
#' A class containing the token to communicate with the API of either the IQB Studio Lite or the IQB Testcenter. Objects of this class will be created by the function [createStudioLogin()] or [createTestcenterLogin()].
#'
#' @export
#'
#' @name Login-class
#' @rdname Login-class
setClass("Login",
         slots = c(
           base_url = "character",
           auth_token = "character",
           base_req = "function",
           ws_list = "list"
         ))

#' @inherit Login slot
#' @slot wsg_list Named list. Returns a list of the workspace groups the user has access to.
setClass("LoginStudio",
         contains = "Login",
         slots = c(
           wsg_list = "list",
           version = "character"
         ))

#' @inherit Login slot
setClass("LoginTestcenter",
         contains = "Login")

#' Workspace access
#'
#' @inherit Login slot
#' @slot ws_id ID of the workspace. See URL to identify the workspace id.
#' @slot ws_label Label of the workspace.
#'
#' @description
#' A class containing the token to communicate with the Testcenter API. Objects of this class will be created by the function [accessWorkspace()] after entering a valid [Login-class] object.
#'
#' @export
#'
#' @name Workspace-class
#' @rdname Workspace-class
setClass("Workspace",
         slots = c(
           ws_id = "numeric",
           ws_label = "character"
         ))

#' @inherit Workspace slot
#' @inherit LoginStudio slot
#' @slot wsg_id ID of the workspace group the current workspace belongs to. See URL to identify the workspace group id.
#' @slot wsg_label Label of the workspace group the current workspace belongs to.
setClass("WorkspaceStudio",
         contains = c("Workspace", "LoginStudio"),
         slots = c(
           wsg_id = "numeric",
           wsg_label = "character"
         ))

#' @inherit Workspace slot
#' @inherit LoginTestcenter slot
setClass("WorkspaceTestcenter",
         contains = c("Workspace", "LoginTestcenter"))
