#' @title WarningManager
#' @docType class
#' @description An object that stores warnings with scenario names, warning codes, and messages.
#'
#' @format NULL
#' @export
WarningManager <- R6::R6Class(
  "WarningManager",

  public = list(
    warnings = list(),

    #' @description Initialize the WarningManager
    initialize = function() {
      self$warnings <- list()
    },

    #' @description Add a warning to the manager.
    #' @param scenario_name Name of the scenario where the warning originated.
    #' @param code Warning code.
    #' @param message Warning message.
    add_warning = function(scenario_name, code, message) {
      if (is.null(self$warnings[[scenario_name]])) {
        self$warnings[[scenario_name]] <- list()
      }
      self$warnings[[scenario_name]][[code]] <- message
    },

    #' @description Remove a warning from the manager.
    #' @param scenario_name Name of the scenario from which the warning should be removed.
    #' @param code Warning code to be removed.
    remove_warning = function(scenario_name, code) {
      if (!is.null(self$warnings[[scenario_name]]) && !is.null(self$warnings[[scenario_name]][[code]])) {
        self$warnings[[scenario_name]][[code]] <- NULL
        # Remove scenario entry if no warnings left
        if (length(self$warnings[[scenario_name]]) == 0) {
          self$warnings[[scenario_name]] <- NULL
        }
      }
    },

    #' @description Retrieve all warnings.
    #' @return A list of warnings.
    get_warnings = function() {
      return(self$warnings)
    }
  ),
  private = list(
    .warningManager = NULL  # Field for WarningManager
  )

)

