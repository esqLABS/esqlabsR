#!/usr/bin/env Rscript

# Script to update Remotes field in DESCRIPTION based on package version status
# - For development versions (with 4th component like x.y.z.9000): remove @*release tags
# - For release versions (3 components like x.y.z): add @*release tags

library(desc)

# Function to determine if current version is a development version
is_dev_version <- function(version_string) {
  version_parts <- strsplit(version_string, "\\.")[[1]]
  # Development versions have 4 components and the 4th component >= 9000
  if (length(version_parts) == 4) {
    fourth_component <- as.numeric(version_parts[4])
    return(fourth_component >= 9000)
  }
  return(FALSE)
}

# Function to update remotes based on version status
update_remotes <- function() {
  desc <- description$new()
  
  # Get current version
  current_version <- desc$get("Version")
  cat("Current package version:", current_version, "\n")
  
  is_dev <- is_dev_version(current_version)
  cat("Is development version:", is_dev, "\n")
  
  # Get current remotes
  remotes <- desc$get("Remotes")
  
  if (is.na(remotes) || remotes == "") {
    cat("No Remotes field found in DESCRIPTION\n")
    return(invisible())
  }
  
  # Parse remotes list
  remotes_list <- trimws(strsplit(remotes, ",\\s*")[[1]])
  cat("Current remotes:\n")
  cat(paste("  -", remotes_list), sep = "\n")
  
  # Update remotes based on version status
  updated_remotes <- sapply(remotes_list, function(r) {
    if (is_dev) {
      # For dev versions: remove @*release tags
      if (grepl("@\\*release$", r)) {
        updated <- sub("@\\*release$", "", r)
        cat("  Removing @*release from:", r, "->", updated, "\n")
        return(updated)
      }
    } else {
      # For release versions: add @*release tags if not present
      if (!grepl("@\\*release$", r)) {
        updated <- paste0(r, "@*release")
        cat("  Adding @*release to:", r, "->", updated, "\n")
        return(updated)
      }
    }
    return(r)
  }, USE.NAMES = FALSE)
  
  # Check if any changes were made
  if (identical(remotes_list, updated_remotes)) {
    cat("No changes needed for Remotes field\n")
    return(invisible())
  }
  
  # Update DESCRIPTION file
  new_remotes <- paste(updated_remotes, collapse = ",\n    ")
  desc$set("Remotes", new_remotes)
  desc$write()
  
  cat("Updated Remotes field in DESCRIPTION\n")
  cat("New remotes:\n")
  cat(paste("  -", updated_remotes), sep = "\n")
}

# Main execution
tryCatch({
  update_remotes()
}, error = function(e) {
  cat("Error updating remotes:", conditionMessage(e), "\n")
  quit(status = 1)
})
