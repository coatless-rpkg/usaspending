#' Parameter Validation Functions
#'
#' @name param_validation
#' @keywords internal
NULL

#' Check if a string is a valid toptier code
#' 
#' @param x String to test
#' @return TRUE if valid toptier code
#' @export
is_toptier_code <- function(x) {
  is.character(x) && 
    nchar(x) %in% 3:4 && 
    grepl("^[0-9]+$", x)
}


#' Check if a value is a valid fiscal year
#' 
#' @param x Value to test
#' @return TRUE if valid fiscal year
#' @export
is_fiscal_year <- function(x) {
  is.numeric(x) && 
    length(x) == 1 && 
    x >= 2000 && 
    x <= as.numeric(format(Sys.Date(), "%Y")) + 1
}

#' Validate an optional fiscal year
#' 
#' @param x Value to test
#' @return TRUE if NULL or valid fiscal year
#' @export
is_fiscal_year_optional <- function(x) {
  is.null(x) || is_fiscal_year(x)
}

#' Check if a string is a valid agency type
#' 
#' @param x String to test
#' @return TRUE if valid agency type
#' @export
is_agency_type <- function(x) {
  x %in% c("awarding", "funding")
}

#' Check if value has valid award type codes
#'
#' @param x Value to check
#' @return Logical indicating if x contains valid award type codes
#' @keywords internal
has_valid_award_codes <- function(x) {
  if (is.null(x)) return(TRUE)  # Award codes can be NULL
  if (!is.character(x)) return(FALSE)
  
  valid_codes <- c(
    "02", "03", "04", "05", "06", "07", "08", "09", "10", "11",
    "A", "B", "C", "D", 
    "IDV_A", "IDV_B", "IDV_B_A", "IDV_B_B", "IDV_B_C", "IDV_C", "IDV_D", "IDV_E"
  )
  
  all(x %in% valid_codes)
}

#' Check if value is a valid sort direction
#'
#' @param x Value to check
#' @return Logical indicating if x is a valid sort direction
#' @keywords internal
is_sort_direction <- function(x) {
  x %in% c("asc", "desc")
}

#' Check if value is a valid page number
#'
#' @param x Value to check
#' @return Logical indicating if x is a valid page number
#' @keywords internal
is_page_number <- function(x) {
  is.numeric(x) && length(x) == 1 && x > 0 && x == floor(x)
}

#' Check if value is a valid limit
#'
#' @param x Value to check
#' @return Logical indicating if x is a valid limit
#' @keywords internal
is_page_limit <- function(x) {
  is.numeric(x) && length(x) == 1 && x > 0 && x <= 100 && x == floor(x)
}

# Check functions that throw errors

#' Check toptier code validity
#'
#' @param x Value to check
#' @return Invisibly returns TRUE if valid, errors if not
#' @keywords internal
check_toptier_code <- function(x) {
  if (!is_toptier_code(x)) {
    cli::cli_abort(c(
      "Invalid toptier code",
      "x" = "Must be 3-4 digits",
      "i" = "Got: {x}"
    ))
  }
  invisible(TRUE)
}

#' Check fiscal year validity
#'
#' @param x Value to check
#' @return Invisibly returns TRUE if valid, errors if not
#' @keywords internal
check_fiscal_year <- function(x) {
  if (!is_fiscal_year(x)) {
    cli::cli_abort(c(
      "Invalid fiscal year",
      "x" = "Must be numeric year >= 2017",
      "i" = "Got: {x}"
    ))
  }
  invisible(TRUE)
}


#' Validate an optional fiscal year
#' 
#' @param x Value to validate
#' @return Invisibly returns TRUE if valid, errors if not
#' @export
check_fiscal_year_optional <- function(x) {
  if (!is_fiscal_year_optional(x)) {
    cli::cli_abort(c(
      "Invalid fiscal year",
      "x" = "Must be NULL or numeric year between 2000 and current year + 1",
      "i" = "Got: {x}"
      )
    )
  }
  invisible(TRUE)
}

#' Check agency type validity
#'
#' @param x Value to check
#' @return Invisibly returns TRUE if valid, errors if not
#' @keywords internal
check_agency_type <- function(x) {
  if (!is_agency_type(x)) {
    cli::cli_abort(c(
      "Invalid agency type",
      "x" = "Must be 'awarding' or 'funding'",
      "i" = "Got: {x}"
    ))
  }
  invisible(TRUE)
}

#' Check award codes validity
#'
#' @param x Value to check
#' @return Invisibly returns TRUE if valid, errors if not
#' @keywords internal
check_award_codes <- function(x) {
  if (!has_valid_award_codes(x)) {
    valid_codes <- c(
      "02", "03", "04", "05", "06", "07", "08", "09", "10", "11",
      "A", "B", "C", "D", 
      "IDV_A", "IDV_B", "IDV_B_A", "IDV_B_B", "IDV_B_C", "IDV_C", "IDV_D", "IDV_E"
    )
    cli::cli_abort(c(
      "Invalid award type codes",
      "x" = "Got invalid codes: {setdiff(x, valid_codes)}",
      "i" = "Valid codes are: {toString(valid_codes)}"
    ))
  }
  invisible(TRUE)
}

#' Check sort direction validity
#'
#' @param x Value to check
#' @return Invisibly returns TRUE if valid, errors if not
#' @keywords internal
check_sort_direction <- function(x) {
  if (!is_sort_direction(x)) {
    cli::cli_abort(c(
      "Invalid sort direction",
      "x" = "Must be 'asc' or 'desc'",
      "i" = "Got: {x}"
    ))
  }
  invisible(TRUE)
}

#' Check page number validity
#'
#' @param x Value to check
#' @return Invisibly returns TRUE if valid, errors if not
#' @keywords internal
check_page_number <- function(x) {
  if (!is_page_number(x)) {
    cli::cli_abort(c(
      "Invalid page number",
      "x" = "Must be numeric >= 1",
      "i" = "Got: {x}"
    ))
  }
  invisible(TRUE)
}

#' Check limit validity
#'
#' @param x Value to check
#' @return Invisibly returns TRUE if valid, errors if not
#' @keywords internal
check_page_limit <- function(x) {
  if (!is_page_limit(x)) {
    cli::cli_abort(c(
      "Invalid limit",
      "x" = "Must be numeric between 1 and 100",
      "i" = "Got: {x}"
    ))
  }
  invisible(TRUE)
}


#' Check if a string is a valid endpoint
#' 
#' @param x String to test
#' @return TRUE if valid endpoint
#' @keywords internal
is_endpoint <- function(x) {
  is.character(x) && length(x) == 1 && nzchar(x)
}

#' Check endpoint validity
#' 
#' @param x Value to check
#' @return Invisibly returns TRUE if valid, errors if not
#' @keywords internal
check_endpoint <- function(x) {
  if (!is_endpoint(x)) {
    cli::cli_abort(c(
      "Invalid endpoint",
      "x" = "Must be a single non-empty character string",
      "i" = "Got: {x}"
    ))
  }
  invisible(TRUE)
}

#' Check if a string is a valid API URL
#' 
#' @param x String to test
#' @return TRUE if valid API URL
#' @keywords internal
is_api_url <- function(x) {
  is.character(x) && length(x) == 1 && grepl("^https?://", x)
}

#' Check API URL validity
#' 
#' @param x Value to check
#' @return Invisibly returns TRUE if valid, errors if not
#' @keywords internal
check_api_url <- function(x) {
  if (!is_api_url(x)) {
    cli::cli_abort(c(
      "Invalid API URL",
      "x" = "Must be a single character string starting with http:// or https://",
      "i" = "Got: {x}"
    ))
  }
  invisible(TRUE)
}

#' Check if a string is a valid API version
#' 
#' @param x String to test
#' @return TRUE if valid API version
#' @keywords internal
is_api_version <- function(x) {
  is.character(x) && length(x) == 1 && grepl("^v\\d+$", x)
}

#' Check API version validity
#' 
#' @param x Value to check
#' @return Invisibly returns TRUE if valid, errors if not
#' @keywords internal
check_api_version <- function(x) {
  if (!is_api_version(x)) {
    cli::cli_abort(c(
      "Invalid API version",
      "x" = "Must be a single character string starting with 'v' followed by numbers",
      "i" = "Got: {x}"
    ))
  }
  invisible(TRUE)
}

#' Check if value is a valid page_all parameter
#'
#' @param x Value to check
#' @return TRUE if valid page_all parameter
#' @keywords internal
is_page_all <- function(x) {
  is.logical(x) && length(x) == 1 && !is.na(x)
}

#' Check page_all parameter validity
#'
#' @param x Value to check
#' @return Invisibly returns TRUE if valid, errors if not
#' @keywords internal
check_page_all <- function(x) {
  if (!is_page_all(x)) {
    cli::cli_abort(c(
      "Invalid page_all parameter",
      "x" = "Must be a single logical value",
      "i" = "Got: {x}"
    ))
  }
  invisible(TRUE)
}

#' Check if response has valid content type
#'
#' @param x Response to check
#' @return TRUE if response has valid content type
#' @keywords internal
has_valid_content_type <- function(x) {
  grepl("application/json", httr2::resp_content_type(x))
}

#' Check response content type validity
#'
#' @param x Response to check
#' @return Invisibly returns TRUE if valid, errors if not
#' @keywords internal
check_response_type <- function(x) {
  if (!has_valid_content_type(x)) {
    cli::cli_abort(c(
      "API request failed",
      "x" = "Unexpected response type",
      "i" = "Got: {httr2::resp_content_type(x)}",
      "i" = "Expected: application/json"
    ))
  }
  invisible(TRUE)
}

#' Check if value is a valid page range
#'
#' @param x Value to check
#' @param total_pages Maximum number of pages available
#' @return Logical indicating if x is a valid page range
#' @keywords internal
is_page_range <- function(x, total_pages) {
  is.null(x) || # NULL is valid
    (is.numeric(x) && 
       length(x) == 2 && 
       !any(is.na(x)) &&
       all(x > 0) && 
       all(x <= total_pages) &&
       x[1] <= x[2])
}

#' Check page range validity 
#'
#' @param x Value to check
#' @param total_pages Maximum number of pages available
#' @return Invisibly returns TRUE if valid, errors if not
#' @keywords internal
check_page_range <- function(x, total_pages) {
  if (!is_page_range(x, total_pages)) {
    cli::cli_abort(c(
      "Invalid page range",
      "x" = "Must be NULL or numeric vector of length 2 with valid page numbers",
      "i" = "Valid range is 1 to {total_pages}",
      "i" = "First page must not exceed second page",
      "i" = "Got: {toString(x)}"
    ))
  }
  invisible(TRUE)
}

#' Check API response for errors and provide detailed diagnostics
#' 
#' @param resp An httr2 response object
#' @param endpoint The API endpoint that was called
#' @param method The HTTP method that was used
#' @return The original response if no errors, otherwise aborts with detailed error
#' @keywords internal
check_api_response <- function(resp, endpoint, method) {
  if (httr2::resp_is_error(resp)) {
    # Try to extract error details from response
    error_body <- tryCatch({
      httr2::resp_body_json(resp)
    }, error = function(e) NULL)
    
    # Build informative error message
    error_msg <- c(
      "API request failed",
      "x" = "HTTP {httr2::resp_status(resp)}: {httr2::resp_status_desc(resp)}"
    )
    
    # Add additional error details if available
    if (!is.null(error_body) && !is.null(error_body$detail)) {
      error_msg <- c(error_msg, "i" = "Error detail: {error_body$detail}")
    } else if (!is.null(error_body) && !is.null(error_body$message)) {
      error_msg <- c(error_msg, "i" = "Error message: {error_body$message}")
    }
    
    # Add information about the endpoint and parameters
    error_msg <- c(error_msg, 
                   "i" = "Endpoint: {endpoint}",
                   "i" = "HTTP method: {method}"
    )
    
    # Add the raw response for debugging
    raw_response <- tryCatch({
      httr2::resp_body_string(resp)
    }, error = function(e) NULL)
    
    if (!is.null(raw_response) && nchar(raw_response) > 0) {
      if (nchar(raw_response) > 500) {
        raw_response <- paste0(substr(raw_response, 1, 500), "...")
      }
      error_msg <- c(error_msg, "i" = "Raw response: {raw_response}")
    }
    
    cli::cli_abort(error_msg)
  }
  
  return(resp)
}