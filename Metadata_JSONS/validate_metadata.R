
#!/usr/bin/env Rscript

# =============================================================================
# validate_metadata.R
#
# Validates disaster metadata JSON files and generates a README.md
# summarizing disasters by development status.
#
# Usage:
#   Rscript validate_metadata.R metadata.json
#
#   Rscript validate_metadata.R metadata/*.json
#
# Required packages:
#   jsonlite
#   cli
# =============================================================================


# =============================================================================
# Packages
# =============================================================================

suppressPackageStartupMessages({
  library(jsonlite)
  library(cli)
})


# =============================================================================
# Configuration
# =============================================================================

ALLOWED_DISASTER_TYPES <- c(
  "wildfire",
  "storm",
  "earthquake"
)

ALLOWED_DATA_TYPES <- c(
  "Population",
  "Movement",
  "Business Activity",
  "Network Coverage"
)

REQUIRED_GENERAL_INFO <- c(
  "disaster_name",
  "country",
  "disaster_start_date",
  "data_start_date",
  "data_end_date",
  "disaster_type",
  "available_data",
  "report_exists",
  "interactive_version_exists",
  "report_complete",
  "interactive_version_complete"
)


# =============================================================================
# Validation state
# =============================================================================

validation_errors <- character()
validation_warnings <- character()


add_error <- function(message) {
  
  validation_errors <<- c(
    validation_errors,
    message
  )
  
  cli_alert_danger(message)
}


add_warning <- function(message) {
  
  validation_warnings <<- c(
    validation_warnings,
    message
  )
  
  cli_alert_warning(message)
}


# =============================================================================
# General validation helpers
# =============================================================================

is_nonempty_string <- function(x) {
  
  is.character(x) &&
    length(x) == 1 &&
    !is.na(x) &&
    nzchar(trimws(x))
}


is_valid_date <- function(x) {
  
  if (!is_nonempty_string(x)) {
    return(FALSE)
  }
  
  parsed <- as.Date(
    x,
    format = "%Y-%m-%d"
  )
  
  !is.na(parsed) &&
    format(parsed, "%Y-%m-%d") == x
}


# =============================================================================
# Required fields
# =============================================================================

check_required_fields <- function(metadata) {
  
  if (!"general_info" %in% names(metadata)) {
    
    add_error(
      "Missing required top-level field: general_info"
    )
    
    return(FALSE)
  }
  
  missing_fields <- setdiff(
    REQUIRED_GENERAL_INFO,
    names(metadata$general_info)
  )
  
  if (length(missing_fields) > 0) {
    
    for (field in missing_fields) {
      
      add_error(
        sprintf(
          "Missing required field: general_info.%s",
          field
        )
      )
    }
    
    return(FALSE)
  }
  
  TRUE
}


# =============================================================================
# String fields
# =============================================================================

check_string_field <- function(info, field) {
  
  value <- info[[field]]
  
  if (!is_nonempty_string(value)) {
    
    add_error(
      sprintf(
        "general_info.%s must be a non-empty string.",
        field
      )
    )
    
    return(FALSE)
  }
  
  TRUE
}


# =============================================================================
# Date validation
# =============================================================================

check_date_fields <- function(info) {
  
  date_fields <- c(
    "disaster_start_date",
    "data_start_date",
    "data_end_date"
  )
  
  valid_dates <- logical(
    length(date_fields)
  )
  
  names(valid_dates) <- date_fields
  
  
  for (field in date_fields) {
    
    value <- info[[field]]
    
    valid_dates[field] <- is_valid_date(value)
    
    if (!valid_dates[field]) {
      
      add_error(
        sprintf(
          paste0(
            "general_info.%s must be a valid date in ",
            "YYYY-MM-DD format; got '%s'."
          ),
          field,
          as.character(value)
        )
      )
    }
  }
  
  
  # Do not compare dates if one or more are invalid.
  if (!all(valid_dates)) {
    return(FALSE)
  }
  
  
  disaster_start <- as.Date(
    info$disaster_start_date
  )
  
  data_start <- as.Date(
    info$data_start_date
  )
  
  data_end <- as.Date(
    info$data_end_date
  )
  
  
  if (data_start < disaster_start) {
    
    add_error(
      paste0(
        "data_start_date cannot be earlier than ",
        "disaster_start_date."
      )
    )
  }
  
  
  if (data_end < data_start) {
    
    add_error(
      paste0(
        "data_end_date cannot be earlier than ",
        "data_start_date."
      )
    )
  }
  
  
  TRUE
}


# =============================================================================
# Disaster type validation
# =============================================================================

check_disaster_type <- function(info) {
  
  value <- info$disaster_type
  
  
  if (!is_nonempty_string(value)) {
    
    add_error(
      "general_info.disaster_type must be a non-empty string."
    )
    
    return(FALSE)
  }
  
  
  if (!value %in% ALLOWED_DISASTER_TYPES) {
    
    add_error(
      sprintf(
        paste0(
          "Invalid disaster_type '%s'. ",
          "Allowed values are: %s."
        ),
        value,
        paste(
          ALLOWED_DISASTER_TYPES,
          collapse = ", "
        )
      )
    )
    
    return(FALSE)
  }
  
  
  TRUE
}


# =============================================================================
# Available data validation
# =============================================================================

check_available_data <- function(info) {
  
  value <- info$available_data
  
  
  if (!is.character(value)) {
    
    add_error(
      paste0(
        "general_info.available_data must be ",
        "an array of strings."
      )
    )
    
    return(FALSE)
  }
  
  
  if (length(value) == 0) {
    
    add_warning(
      "general_info.available_data is empty."
    )
    
    return(TRUE)
  }
  
  
  # ---------------------------------------------------------------------------
  # Invalid values
  # ---------------------------------------------------------------------------
  
  invalid_values <- setdiff(
    value,
    ALLOWED_DATA_TYPES
  )
  
  if (length(invalid_values) > 0) {
    
    add_error(
      sprintf(
        paste0(
          "Invalid available_data value(s): %s. ",
          "Allowed values are: %s."
        ),
        paste(
          invalid_values,
          collapse = ", "
        ),
        paste(
          ALLOWED_DATA_TYPES,
          collapse = ", "
        )
      )
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Duplicate values
  # ---------------------------------------------------------------------------
  
  duplicate_values <- unique(
    value[duplicated(value)]
  )
  
  if (length(duplicate_values) > 0) {
    
    add_warning(
      sprintf(
        "Duplicate available_data value(s): %s.",
        paste(
          duplicate_values,
          collapse = ", "
        )
      )
    )
  }
  
  
  TRUE
}


# =============================================================================
# Boolean validation
# =============================================================================

check_boolean_fields <- function(info) {
  
  boolean_fields <- c(
    "report_exists",
    "interactive_version_exists",
    "report_complete",
    "interactive_version_complete"
  )
  
  
  for (field in boolean_fields) {
    
    value <- info[[field]]
    
    
    if (
      !is.logical(value) ||
      length(value) != 1 ||
      is.na(value)
    ) {
      
      add_error(
        sprintf(
          "general_info.%s must be a single boolean (true/false).",
          field
        )
      )
    }
  }
  
  
  TRUE
}


# =============================================================================
# Existence / completion consistency
# =============================================================================

check_development_consistency <- function(info) {
  
  
  # ---------------------------------------------------------------------------
  # Report
  # ---------------------------------------------------------------------------
  
  if (
    isTRUE(info$report_complete) &&
    !isTRUE(info$report_exists)
  ) {
    
    add_error(
      paste0(
        "report_complete cannot be true when ",
        "report_exists is false."
      )
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Interactive version
  # ---------------------------------------------------------------------------
  
  if (
    isTRUE(info$interactive_version_complete) &&
    !isTRUE(info$interactive_version_exists)
  ) {
    
    add_error(
      paste0(
        "interactive_version_complete cannot be true when ",
        "interactive_version_exists is false."
      )
    )
  }
  
  
  TRUE
}


# =============================================================================
# Development level
# =============================================================================
#
# Report status:
#   - Report complete
#   - Report in development
#
# Interactive status:
#   - Interactive version complete
#   - Interactive version in development
#
# Completion is determined by the *_complete fields.
# The *_exists fields are separately validated for consistency.
# =============================================================================

get_development_level <- function(info) {
  
  report_status <- dplyr::case_when(
    isTRUE(info$report_complete) ~ "Complete",
    isTRUE(info$report_exists) ~ "In development",
    .default = "Not available"
  )
  
  
  interactive_status <- dplyr::case_when(
    isTRUE(info$interactive_version_complete) ~ "Complete",
    isTRUE(info$interactive_version_exists) ~ "In development",
    .default = "Not available"
  )
  
  list(
    report_status = report_status,
    interactive_status = interactive_status
  )
}


# =============================================================================
# Validate one metadata file
# =============================================================================

validate_metadata <- function(file) {
  
  cli_h2(
    sprintf(
      "Validating %s",
      file
    )
  )
  
  
  # ---------------------------------------------------------------------------
  # Check file exists
  # ---------------------------------------------------------------------------
  
  if (!file.exists(file)) {
    
    add_error(
      sprintf(
        "Metadata file does not exist: %s",
        file
      )
    )
    
    return(FALSE)
  }
  
  
  # ---------------------------------------------------------------------------
  # Parse JSON
  # ---------------------------------------------------------------------------
  
  metadata <- tryCatch(
    
    fromJSON(
      file,
      simplifyVector = TRUE
    ),
    
    error = function(e) {
      
      add_error(
        sprintf(
          "Invalid JSON in '%s': %s",
          file,
          e$message
        )
      )
      
      NULL
    }
  )
  
  
  if (is.null(metadata)) {
    return(FALSE)
  }
  
  
  # ---------------------------------------------------------------------------
  # Check required fields
  # ---------------------------------------------------------------------------
  
  if (!check_required_fields(metadata)) {
    return(FALSE)
  }
  
  
  info <- metadata$general_info
  
  
  # ---------------------------------------------------------------------------
  # Validate fields
  # ---------------------------------------------------------------------------
  
  check_string_field(
    info,
    "disaster_name"
  )
  
  check_string_field(
    info,
    "country"
  )
  
  check_date_fields(
    info
  )
  
  check_disaster_type(
    info
  )
  
  check_available_data(
    info
  )
  
  check_boolean_fields(
    info
  )
  
  check_development_consistency(
    info
  )
  
  
  # ---------------------------------------------------------------------------
  # Development status
  # ---------------------------------------------------------------------------
  
  development <- get_development_level(
    info
  )
  
  
  cli_alert_info(
    "Report status: {.strong {development$report_status}}"
  )
  
  cli_alert_info(
    "Interactive status: {.strong {development$interactive_status}}"
  )
  
  
  TRUE
}


# =============================================================================
# Generate README
# =============================================================================

generate_readme <- function(
    metadata_files,
    output_file = "README.md"
) {
  
  disasters <- list()
  
  # ---------------------------------------------------------------------------
  # Read metadata
  # ---------------------------------------------------------------------------
  
  for (file in metadata_files) {
    
    metadata <- tryCatch(
      
      fromJSON(
        file,
        simplifyVector = TRUE
      ),
      
      error = function(e) {
        
        add_error(
          sprintf(
            "Could not parse '%s': %s",
            file,
            e$message
          )
        )
        
        NULL
      }
    )
    
    
    if (
      is.null(metadata) ||
      is.null(metadata$general_info)
    ) {
      next
    }
    
    
    info <- metadata$general_info
    
    
    # -------------------------------------------------------------------------
    # Available data
    # -------------------------------------------------------------------------
    
    data_display <- if (
      length(info$available_data) > 0
    ) {
      
      paste(
        info$available_data,
        collapse = ", "
      )
      
    } else {
      
      "None"
    }
    
    
    # -------------------------------------------------------------------------
    # Add disaster
    # -------------------------------------------------------------------------
    
    development <- get_development_level(
      info
    )
    
    disasters[[length(disasters) + 1]] <- list(
      
      name = info$disaster_name,
      
      country = info$country,
      
      type = info$disaster_type,
      
      disaster_start_date = info$disaster_start_date,
      
      data_start_date = info$data_start_date,
      
      data_end_date = info$data_end_date,
      
      available_data = data_display,
      
      report_status = development$report_status,
      
      interactive_status = development$interactive_status
    )
  }
  
  
  # ===========================================================================
  # README header
  # ===========================================================================
  
  lines <- c(
    
    "# Disaster Metadata",
    
    "",
    
    "Summary of all disasters and their available data and ",
    "development status.",
    
    ""
  )
  
  
  # ===========================================================================
  # No disasters
  # ===========================================================================
  
  if (length(disasters) == 0) {
    
    lines <- c(
      lines,
      "No disaster metadata files were found.",
      ""
    )
    
  } else {
    
    # =======================================================================
    # Table header
    # =======================================================================
    
    # ===========================================================================
    # Table
    # ===========================================================================
    
    lines <- c(
      lines,
      "| Disaster | Country | Type | Disaster start | Data start | Data end | Available data | Report Status | Interactive Status |",
      "|---|---|---|---|---|---|---|---|---|"
    )
    
    for (disaster in disasters) {
      
      lines <- c(
        lines,
        sprintf(
          "| %s | %s | %s | %s | %s | %s | %s | %s | %s |",
          disaster$name,
          disaster$country,
          disaster$type,
          disaster$disaster_start_date,
          disaster$data_start_date,
          disaster$data_end_date,
          disaster$available_data,
          disaster$report_status,
          disaster$interactive_status
        )
      )
    }
    
    lines <- c(
      lines,
      ""
    )
  }
  
  
  # ===========================================================================
  # Footer
  # ===========================================================================
  
  lines <- c(
    
    lines,
    
    "---",
    
    "",
    
    sprintf(
      "_Generated automatically on %s._",
      format(
        Sys.Date(),
        "%Y-%m-%d"
      )
    ),
    
    ""
  )
  
  
  # ===========================================================================
  # Write README
  # ===========================================================================
  
  writeLines(
    lines,
    output_file
  )
  
  
  cli_alert_success(
    "README written to {.file %s}",
    output_file
  )
}



# =============================================================================
# Main
# =============================================================================


# -----------------------------------------------------------------------------
# Check arguments
# -----------------------------------------------------------------------------




metadata_files <- list.files(pattern = "\\.JSON$")
metadata_files <- metadata_files[metadata_files != "template.JSON"]

if (length(metadata_files) == 0) {
  
  cli_abort(
    paste0(
      "No metadata files supplied.\n",
      "Usage: Rscript validate_metadata.R metadata.json"
    )
  )
}

# =============================================================================
# Run validation
# =============================================================================

cli_h1(
  "Disaster Metadata Validation"
)


for (file in metadata_files) {
  
  validate_metadata(
    file
  )
}


# =============================================================================
# Generate README
# =============================================================================

generate_readme(
  metadata_files
)


# =============================================================================
# Summary
# =============================================================================

cli_h1(
  "Validation Summary"
)


if (length(validation_errors) == 0) {
  
  cli_alert_success(
    "Validation passed: no errors found."
  )
  
} else {
  
  cli_alert_danger(
    "Validation failed: {length(validation_errors)} error(s) found."
  )
}


if (length(validation_warnings) > 0) {
  
  cli_alert_warning(
    "{length(validation_warnings)} warning(s) found."
  )
  
} else {
  
  cli_alert_success(
    "No warnings."
  )
}


# -----------------------------------------------------------------------------
# Final status
# -----------------------------------------------------------------------------

if (length(validation_errors) == 0) {
  
  cli_alert_success(
    "Metadata validation complete."
  )
  
  quit(
    status = 0
  )
  
} else {
  
  cli_alert_danger(
    "Metadata validation failed."
  )
  
}
