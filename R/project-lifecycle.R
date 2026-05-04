# Project lifecycle: public seam exports.
#
# This file is the home for `loadProject()` and (in later chapters)
# `saveProject()`, `initProject()`, etc. Currently it only contains
# `loadProject()` — the first user-callable entry point of the
# JSON-first workflow.

#' Load a project from a JSON configuration file.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Loads a v2.0 [Project] from a `Project.json` file. Pass the
#' returned [Project] to [runScenarios()] to drive the JSON-first
#' workflow.
#'
#' @param path Path to the `Project.json` file. Defaults to
#'   `Project.json` in the working directory.
#'
#' @returns An object of class [Project].
#'
#' @export
#'
#' @examples
#' \dontrun{
#' project <- loadProject("Project.json")
#' results <- runScenarios(project)
#' }
loadProject <- function(path = "Project.json") {
  .loadProjectJson(path)
}
