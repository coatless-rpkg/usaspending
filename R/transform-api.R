#' Transform an API Response into a Tibble
#'
#' Converts a USAspending API response into a tibble format, handling both paginated 
#' and non-paginated responses. The function preserves pagination metadata and messages
#' as attributes in the resulting tibble.
#'
#' @param x An API response object containing either a 'results' field for paginated 
#'   responses or direct data fields for non-paginated responses
#' @param flatten Logical. If TRUE, attempts to flatten nested data structures within 
#'   the response into separate columns. For complex nested structures, some elements 
#'   may remain as nested tibbles if they cannot be flattened without data loss.
#' @return A tibble containing:
#'   * Data from the API response organized in columns
#'   * Pagination metadata as an attribute (if present in response)
#'   * API messages as an attribute (if present in response)
#' 
#' @note The function automatically handles different response structures:
#'   * Paginated responses with a 'results' field
#'   * Non-paginated responses with direct data fields
#'   * Responses with nested data structures (when flatten = TRUE)
#' @importFrom tibble tibble as_tibble
#' @keywords internal
transform_response <- function(x, flatten = FALSE) {
  # Extract pagination metadata if present
  pagination <- if (!is.null(x$page_metadata)) {
    x$page_metadata
  } else {
    NULL
  }
  
  # Create main data frame based on structure
  if (!is.null(x$results)) {
    # Handle paginated results or results array
    if (is.list(x$results) && length(x$results) == 1 && 
        (is.list(x$results[[1]]) && is.null(names(x$results[[1]])))) {
      # Handle the case where results is a list containing a single unnamed list (combined pages)
      df <- transform_list_to_tibble(x$results[[1]])
    } else {
      # Regular results array
      df <- transform_list_to_tibble(x$results)
    }
  } else {
    # Handle non-paginated response by excluding metadata fields
    metadata_fields <- c("page_metadata", "messages", "totals", "total")
    data_fields <- setdiff(names(x), metadata_fields)
    
    if (length(data_fields) > 0) {
      # Extract the data fields
      data <- x[data_fields]
      df <- transform_list_to_tibble(data)
    } else {
      df <- tibble::tibble()
    }
  }
  
  # Handle nested structures if requested
  if (flatten && ncol(df) > 0) {
    df <- flatten_tibble(df)
  }
  
  # Add pagination metadata as attributes
  if (!is.null(pagination)) {
    attr(df, "pagination") <- pagination
  }
  
  # Add messages as attribute if present
  if (!is.null(x$messages)) {
    attr(df, "messages") <- x$messages
  }
  
  # Preserve totals if present
  if (!is.null(x$totals)) {
    attr(df, "totals") <- x$totals
  } else if (!is.null(x$total)) {
    attr(df, "total") <- x$total
  }
  
  # Preserve category/spending_level if present (for spending endpoints)
  if (!is.null(x$category)) {
    attr(df, "category") <- x$category
  }
  
  if (!is.null(x$spending_level)) {
    attr(df, "spending_level") <- x$spending_level
  }
  
  # Preserve scope/geo_layer if present (for geography endpoints)
  if (!is.null(x$scope)) {
    attr(df, "scope") <- x$scope
  }
  
  if (!is.null(x$geo_layer)) {
    attr(df, "geo_layer") <- x$geo_layer
  }
  
  # Preserve end_date for spending explorer responses
  if (!is.null(x$end_date)) {
    attr(df, "end_date") <- x$end_date
  }
  
  df
}


#' Transform a List into a Tibble
#'
#' Recursively converts a list or nested list structure into a tibble format, 
#' handling various list structures and edge cases.
#'
#' @param x A list to transform, which can be:
#'   * A data frame (returned as tibble)
#'   * An empty list (returns empty tibble)
#'   * A named list of atomic values (becomes one row)
#'   * A list of lists (each sublist becomes a row)
#'   * A named list with nested structures
#'
#' @return A tibble where:
#'   * Each top-level list element typically becomes a row
#'   * Nested lists are either flattened into columns or preserved as list-columns
#'   * NULL values are converted to NA
#'   * Column names are preserved from list names when available
#'
#' @note This function handles complex nested structures by:
#'   * Converting consistent nested lists to tibbles
#'   * Preserving nested data frames as list-columns
#'   * Creating default names (v1, v2, etc.) for unnamed lists
#' @keywords internal
transform_list_to_tibble <- function(x) {
  # Handle different input types
  if (is.data.frame(x)) {
    return(tibble::as_tibble(x))
  }
  
  if (!is.list(x) || length(x) == 0) {
    return(tibble::tibble())
  }
  
  # Determine if x is a single row (named list with atomic values)
  if (is.list(x) && !is.null(names(x)) && 
      !any(vapply(x, is.list, logical(1))) && 
      !any(vapply(x, is.data.frame, logical(1)))) {
    # Convert NULL values to NA
    x[vapply(x, is.null, logical(1))] <- NA
    return(tibble::tibble(!!!x))
  }
  
  # Handle list of lists (each element becomes a row)
  if (is.null(names(x))) {
    # Elements are lists - each becomes a row
    if (all(vapply(x, is.list, logical(1)))) {
      if (all(vapply(x, function(l) !is.null(names(l)), logical(1)))) {
        # Convert list of lists to data frame rows
        rows_list <- lapply(x, function(.x) {
          result <- as.list(.x)
          result[vapply(result, is.null, logical(1))] <- NA
          
          # Process nested structures
          for (nm in names(result)) {
            if (is.list(result[[nm]]) && !is.data.frame(result[[nm]])) {
              # Convert deeply nested lists to tibbles
              if (length(result[[nm]]) > 0 && 
                  all(vapply(result[[nm]], is.list, logical(1)))) {
                result[[nm]] <- list(transform_list_to_tibble(result[[nm]]))
              }
            }
          }
          
          tibble::as_tibble(result)
        })
        
        # Combine all rows
        if (length(rows_list) > 0) {
          # Use bind_rows to correctly handle different column sets
          df <- dplyr::bind_rows(rows_list)
        } else {
          df <- tibble::tibble()
        }
        
        return(df)
      } else {
        # Unnamed lists, try to convert each to a tibble
        rows_list <- lapply(x, transform_list_to_tibble)
        if (length(rows_list) > 0) {
          df <- dplyr::bind_rows(rows_list)
          return(df)
        } else {
          return(tibble::tibble())
        }
      }
    } else {
      # Create default column names for unnamed atomic vector
      names(x) <- paste0("v", seq_along(x))
      return(tibble::tibble(!!!x))
    }
  } else {
    # Named list - treat as a single row unless elements are complex
    x[vapply(x, is.null, logical(1))] <- NA
    
    # Check if all elements are atomic (non-list)
    if (all(vapply(x, function(el) !is.list(el) && !is.data.frame(el), logical(1)))) {
      return(tibble::tibble(!!!x))
    } else {
      # Handle list columns
      for (nm in names(x)) {
        if (is.list(x[[nm]]) && !is.data.frame(x[[nm]])) {
          # Convert nested list to tibble if it's a consistent list of lists
          if (length(x[[nm]]) > 0 && 
              all(vapply(x[[nm]], is.list, logical(1)))) {
            x[[nm]] <- list(transform_list_to_tibble(x[[nm]]))
          }
        }
      }
      
      return(tibble::as_tibble(x))
    }
  }
}

#' Flatten a Nested Tibble Structure
#'
#' Attempts to flatten complex nested structures within a tibble into a simpler format
#' while preserving data relationships. This function handles various types of nested
#' data intelligently, either by flattening where possible or preserving structure
#' where flattening would cause data loss.
#'
#' @param df A tibble containing potentially nested structures such as:
#'   * Lists of consistent data (converted to new tibble columns)
#'   * Simple lists of scalars (unlisted into atomic vectors)
#'   * Complex nested structures (flattened with column name prefixes)
#'   * Nested data frames (preserved as list-columns)
#'
#' @return A tibble where:
#'   * Nested structures are flattened where possible
#'   * Column names for flattened data use parent_child format
#'   * Complex structures that cannot be safely flattened remain as list-columns
#'   * Original column order is preserved for non-nested columns
#'
#' @note The function employs different strategies based on the nested data:
#'   * Consistent nested lists are converted to tibbles
#'   * Simple scalar lists are unlisted to vectors
#'   * Complex structures are flattened with prefixed column names
#'   * Data frames are preserved as list-columns to maintain their structure
#' @keywords internal
flatten_tibble <- function(df) {
  if (ncol(df) == 0) return(df)
  
  # Process each column
  flat_cols <- list()
  
  for (col in names(df)) {
    val <- df[[col]]
    
    if (is.list(val) && !is.data.frame(val)) {
      # Check for list column types
      if (all(vapply(val, length, integer(1)) == 1) && 
          all(vapply(val, function(x) !is.list(x) && !is.data.frame(x), logical(1)))) {
        # Simple list of scalars - unlist to vector
        flat_cols[[col]] <- unlist(val)
      } else if (all(vapply(val, is.list, logical(1))) && 
                 length(unique(vapply(lapply(val, names), paste, collapse = ",", FUN.VALUE = ""))) == 1) {
        # List of consistent structure that can be made into a tibble
        nested_tibble <- transform_list_to_tibble(val)
        if (ncol(nested_tibble) > 0) {
          flat_cols[[col]] <- list(nested_tibble)
        } else {
          flat_cols[[col]] <- val  # Keep original if transformation failed
        }
      } else {
        # Try to unnest structured data
        unnested <- tryCatch({
          # Check if values can be combined into a data frame
          row_data <- lapply(val, function(x) {
            if (is.list(x) && !is.data.frame(x)) {
              # Convert NULL values to NA
              x[vapply(x, is.null, logical(1))] <- NA
              return(as.list(x))
            } else {
              return(list(value = x))
            }
          })
          
          # Only proceed if we have consistent structures
          if (length(row_data) > 0) {
            col_names <- unique(unlist(lapply(row_data, names)))
            if (length(col_names) > 0) {
              # Create a tibble with prefixed column names
              nested_df <- dplyr::bind_rows(row_data)
              if (ncol(nested_df) > 0) {
                names(nested_df) <- paste(col, names(nested_df), sep = "_")
                return(nested_df)
              }
            }
          }
          NULL
        }, error = function(e) NULL)
        
        if (!is.null(unnested) && ncol(unnested) > 0) {
          # Add the unnested columns to flat_cols
          flat_cols <- c(flat_cols, as.list(unnested))
        } else {
          # Keep original if unnesting failed
          flat_cols[[col]] <- val
        }
      }
    } else if (is.data.frame(val)) {
      # Keep nested data frames as they are
      flat_cols[[col]] <- val
    } else {
      # Regular column - keep as is
      flat_cols[[col]] <- val
    }
  }
  
  # Create the flattened tibble
  result <- tibble::as_tibble(flat_cols)
  
  # Preserve attributes from original tibble
  for (attr_name in names(attributes(df))) {
    if (!attr_name %in% c("names", "row.names", "class")) {
      attr(result, attr_name) <- attr(df, attr_name)
    }
  }
  
  return(result)
}

#' Print Method for USAspending API Response Tibbles
#'
#' Provides a structured, informative display of USAspending API response data,
#' including metadata, pagination information, and a preview of the actual data.
#' This method enhances the default tibble printing with API-specific context.
#'
#' @param x A transformed API response of class 'usasp_tibble'
#' @param ... Additional arguments passed to the default tibble print method
#'
#' @details The output is organized into sections:
#'   1. API endpoint identification
#'   2. Pagination information (if present)
#'   3. API messages (if any)
#'   4. Data summary (dimensions and column names)
#'   5. Data preview
#'
#' @note The function uses cli package for formatted console output
#'   and falls back to the standard tibble printing method for
#'   the actual data preview
#' @export
print.usasp_tibble <- function(x, ...) {
  # Get endpoint info
  endpoint <- attr(x, "endpoint")
  if (!is.null(endpoint)) {
    cli::cli_h1("USAspending API Response: {endpoint}")
  }
  
  # Print pagination info if present
  pagination <- attr(x, "pagination")
  if (!is.null(pagination)) {
    cli::cli_h2("Pagination")
    
    # Determine if all pages were retrieved
    all_retrieved <- !pagination$hasNext && nrow(x) >= pagination$total
    
    # Create pagination details list
    pagination_details <- c(
      "Current Page" = pagination$page,
      "Total Results" = pagination$total,
      "Results per Page" = pagination$limit,
      "Retrieved" = if(all_retrieved) 
        "All pages" 
      else 
        sprintf("Page %d of %d", 
                pagination$page, 
                ceiling(pagination$total/pagination$limit))
    )
    
    cli::cli_dl(pagination_details)
    
    # Add info alert for partial retrieval
    if (!all_retrieved) {
      cli::cli_alert_info(c(
        "Showing partial results. ",
        "Use '.page_all = TRUE' to retrieve all {pagination$total} results."
      ))
    }
  }
  
  # Print totals info if present (for disaster endpoints)
  totals <- attr(x, "totals")
  if (!is.null(totals)) {
    cli::cli_h2("Totals")
    # Format totals as a definition list
    totals_list <- as.list(totals)
    cli::cli_dl(totals_list)
  }
  
  # Print spending explorer total if present
  total <- attr(x, "total")
  if (!is.null(total)) {
    cli::cli_h2("Total")
    cli::cli_text("Total Amount: {format(total, big.mark = ',', scientific = FALSE)}")
  }
  
  # Print category info for spending endpoints
  if (!is.null(attr(x, "category"))) {
    cli::cli_h2("Category Information")
    cli::cli_text("Category: {attr(x, 'category')}")
    if (!is.null(attr(x, "spending_level"))) {
      cli::cli_text("Spending Level: {attr(x, 'spending_level')}")
    }
  }
  
  # Print geography info for geographic endpoints
  if (!is.null(attr(x, "scope")) || !is.null(attr(x, "geo_layer"))) {
    cli::cli_h2("Geographic Information")
    if (!is.null(attr(x, "scope"))) {
      cli::cli_text("Scope: {attr(x, 'scope')}")
    }
    if (!is.null(attr(x, "geo_layer"))) {
      cli::cli_text("Geographic Layer: {attr(x, 'geo_layer')}")
    }
  }
  
  # Print end_date for spending explorer responses
  if (!is.null(attr(x, "end_date"))) {
    cli::cli_h2("Response Date")
    cli::cli_text("As of: {attr(x, 'end_date')}")
  }
  
  # Print messages if present
  messages <- attr(x, "messages")
  if (!is.null(messages) && length(messages) > 0) {
    cli::cli_h2("Messages")
    cli::cli_bullets(messages)
  }
  
  # Print data summary
  cli::cli_h2("Data Summary")
  if (ncol(x) == 0) {
    cli::cli_alert_warning("No data columns found in response")
  } else {
    cli::cli_text("{nrow(x)} rows x {ncol(x)} columns")
    
    # Print column names in groups for better readability
    col_groups <- split(names(x), ceiling(seq_along(names(x))/5))
    for (group in col_groups) {
      cli::cli_text(paste(group, collapse = ", "))
    }
  }
  
  # Print actual data
  cli::cli_h2("Data Preview")
  NextMethod()
}

#' Convert USAspending API Response to Tibble
#'
#' Transforms a raw USAspending API response into a tibble format with additional
#' metadata preserved as attributes. This method handles the conversion of API-specific
#' data structures while maintaining important context from the original response.
#'
#' @param x An API response object of class 'usasp_response'
#' @param ... Additional arguments not used
#' @param flatten Logical. If TRUE, attempts to flatten nested data structures
#'   into separate columns where possible. Complex nested structures may remain
#'   as list-columns if they cannot be flattened without data loss.
#'
#' @return A tibble of class 'usasp_tibble' containing:
#'   * Transformed API response data
#'   * Original endpoint information as an attribute
#'   * Pagination metadata if present
#'   * API messages if present
#'
#' @note This function serves as the primary method for converting raw API
#'   responses into a tabular format suitable for data analysis while
#'   preserving important metadata
#' @export
as_tibble.usasp_raw_response <- function(x, ..., flatten = FALSE) {
  # Get additional parameters
  params <- list(...)
  if (!is.null(params$flatten)) {
    flatten <- params$flatten
  }
  
  # Transform response
  df <- transform_response(x, flatten = flatten)
  
  # Add endpoint info and class
  attr(df, "endpoint") <- class(x)[1]
  class(df) <- c("usasp_tibble", class(df))
  
  # Add original endpoint path
  if (!is.null(attr(x, "endpoint"))) {
    attr(df, "endpoint_path") <- attr(x, "endpoint")
  }
  
  df
}

#' Combine Multiple USAspending API Response Tibbles
#'
#' Combines multiple tibbles from the same endpoint type into a single tibble,
#' preserving metadata and handling different column structures.
#'
#' @param ... Multiple tibbles of class 'usasp_tibble' to combine
#'
#' @return A combined tibble of class 'usasp_tibble' containing:
#'   * All rows from input tibbles
#'   * Columns from all tibbles (filled with NA where needed)
#'   * Preserved metadata from the first tibble
#'
#' @note This function is useful for combining responses from multiple API calls
#'   to the same endpoint with different parameters or from pagination
#' @export
combine_usasp_tibbles <- function(...) {
  tibbles <- list(...)
  
  # Validate inputs
  if (length(tibbles) == 0) {
    return(tibble::tibble())
  }
  
  # Check if all inputs are usasp_tibbles
  is_usasp_tibble <- vapply(tibbles, function(x) inherits(x, "usasp_tibble"), logical(1))
  if (!all(is_usasp_tibble)) {
    cli::cli_abort("All inputs must be usasp_tibble objects")
  }
  
  # Get attributes from first tibble to preserve
  first_tibble <- tibbles[[1]]
  attrs_to_preserve <- attributes(first_tibble)
  
  # Remove attributes that will be recalculated
  attrs_to_preserve$names <- NULL
  attrs_to_preserve$row.names <- NULL
  attrs_to_preserve$class <- NULL
  
  # Combine tibbles using bind_rows
  combined <- dplyr::bind_rows(tibbles)
  
  # Restore preserved attributes
  for (attr_name in names(attrs_to_preserve)) {
    attr(combined, attr_name) <- attrs_to_preserve[[attr_name]]
  }
  
  # Update class
  class(combined) <- c("usasp_tibble", class(combined)[-1])
  
  # Update pagination info if present
  if (!is.null(attr(combined, "pagination"))) {
    pagination <- attr(combined, "pagination")
    pagination$page <- 1
    pagination$hasNext <- FALSE
    pagination$hasPrevious <- FALSE
    pagination[["next"]] <- NULL
    pagination$previous <- NULL
    pagination$total <- nrow(combined)
    attr(combined, "pagination") <- pagination
  }
  
  # Update totals if present by summing numeric values or using max for counts
  if (!is.null(attr(combined, "totals"))) {
    totals <- attr(combined, "totals")
    # Get all totals attributes
    all_totals <- lapply(tibbles, function(x) attr(x, "totals"))
    all_totals <- all_totals[!vapply(all_totals, is.null, logical(1))]
    
    if (length(all_totals) > 0) {
      # Combine totals - sum numeric values, take max of counts
      combined_totals <- totals
      for (field in names(totals)) {
        if (is.numeric(totals[[field]])) {
          # For award_count, take the max value as we're aggregating same data
          if (field == "award_count") {
            combined_totals[[field]] <- max(vapply(all_totals, function(t) t[[field]], numeric(1)))
          } else {
            # For monetary values, sum them
            combined_totals[[field]] <- sum(vapply(all_totals, function(t) t[[field]], numeric(1)))
          }
        }
      }
      attr(combined, "totals") <- combined_totals
    }
  }
  
  # Combine messages if present
  all_messages <- unique(unlist(lapply(tibbles, function(x) attr(x, "messages"))))
  if (length(all_messages) > 0) {
    attr(combined, "messages") <- all_messages
  }
  
  combined
}

#' Extract Nested Data from USAspending API Tibble
#'
#' Extracts and transforms nested data structures from list columns in
#' USAspending API response tibbles.
#'
#' @param x A tibble with list columns to extract
#' @param col_name Name of the list column to extract
#' @param flatten Logical. If TRUE, attempts to flatten the extracted tibble
#'
#' @return A tibble containing the extracted and transformed nested data
#'
#' @note This function is particularly useful for extracting nested data
#'   structures like 'children', 'results', or other list-columns that
#'   contain complex data
#' @export
extract_nested_data <- function(x, col_name, flatten = FALSE) {
  if (!col_name %in% names(x)) {
    cli::cli_abort(c(
      "Column not found",
      "x" = "Column '{col_name}' not found in the tibble"
    ))
  }
  
  if (!is.list(x[[col_name]]) || is.null(x[[col_name]])) {
    cli::cli_abort(c(
      "Invalid column type",
      "x" = "Column '{col_name}' is not a list column or is NULL"
    ))
  }
  
  # Extract nested data
  nested_data <- x[[col_name]]
  
  # Convert to tibble
  if (all(vapply(nested_data, is.data.frame, logical(1)))) {
    # List of data frames - bind them
    result <- dplyr::bind_rows(nested_data)
  } else {
    # List of other structures - transform to tibble
    result <- transform_list_to_tibble(nested_data)
  }
  
  # Flatten if requested
  if (flatten && ncol(result) > 0) {
    result <- flatten_tibble(result)
  }
  
  result
}

#' Get all pages of a USAspending API endpoint
#'
#' Fetches all pages of results from a paginated API endpoint by making
#' multiple requests and combining the results.
#'
#' @param initial_response The first response object from the API call
#' @param request_fn A function that takes page number as argument and returns API response
#' @param max_pages Maximum number of pages to retrieve (optional)
#'
#' @return A combined tibble with all pages of results
#'
#' @export
get_all_pages <- function(initial_response, request_fn, max_pages = NULL) {
  # Convert initial response to tibble
  result_tibble <- as_tibble(initial_response)
  
  # Check if pagination exists
  pagination <- attr(result_tibble, "pagination")
  if (is.null(pagination) || !pagination$hasNext) {
    return(result_tibble)
  }
  
  # Calculate total pages
  total_pages <- ceiling(pagination$total / pagination$limit)
  if (!is.null(max_pages) && max_pages < total_pages) {
    total_pages <- max_pages
  }
  
  # Initialize list to store all tibbles
  all_tibbles <- list(result_tibble)
  
  # Fetch remaining pages
  for (page in 2:total_pages) {
    cli::cli_alert_info("Fetching page {page} of {total_pages}")
    
    response <- request_fn(page)
    page_tibble <- as_tibble(response)
    
    all_tibbles <- c(all_tibbles, list(page_tibble))
    
    # Check if we've reached the last page
    page_pagination <- attr(page_tibble, "pagination")
    if (is.null(page_pagination) || !page_pagination$hasNext) {
      break
    }
  }
  
  # Combine all tibbles
  do.call(combine_usasp_tibbles, all_tibbles)
}


#' Convert API Response to Raw Format
#'
#' Returns the API response in its raw format without conversion to tibble,
#' while still adding endpoint information as an attribute.
#'
#' @param x An API response object
#' @param ... Additional arguments (not used)
#'
#' @return The original API response with added endpoint attribute
#'
#' @export
as_usasp_response <- function(x, ...) {
  # Add endpoint info as attribute if available
  if (inherits(x, "usasp_raw_response")) {
    endpoint <- class(x)[1]
    attr(x, "endpoint") <- endpoint
  }
  
  # Return marked up response
  class(x) <- c("usasp_response", class(x))
  return(x)
}

#' Print Method for Marked Up USAspending API Responses
#'
#' Provides a structured display of API response data
#'
#' @param x A raw API response of class `'usasp_response'`
#' @param ... Additional arguments passed to print
#'
#' @export
print.usasp_response <- function(x, ...) {
  # Print endpoint if available
  endpoint <- attr(x, "endpoint")
  if (!is.null(endpoint)) {
    cli::cli_h1("USAspending API Response: {endpoint}")
  } else {
    cli::cli_h1("USAspending API Response")
  }
  
  # Print message about raw format
  cli::cli_alert_info("Displaying Marked Up API response. Use as_tibble() to convert to tibble format.")
  
  # Print structure overview
  cli::cli_h2("Response Structure")
  
  # Print main components
  components <- names(x)
  if (length(components) > 0) {
    cli::cli_text("Components: {paste(components, collapse = ', ')}")
    
    # Check for pagination
    if ("page_metadata" %in% components) {
      cli::cli_h3("Pagination")
      print(x$page_metadata)
    }
    
    # Check for results
    if ("results" %in% components) {
      cli::cli_h3("Results")
      if (length(x$results) > 0) {
        cli::cli_text("Contains {length(x$results)} result(s)")
        
        # Display sample of first result
        if (is.list(x$results[[1]])) {
          cli::cli_text("{.strong First Result Structure}")
          first_item_keys <- names(x$results[[1]])
          if (length(first_item_keys) > 0) {
            cli::cli_text("Fields: {paste(first_item_keys, collapse = ', ')}")
          }
        }
      }
    }
    
    # Check for totals
    if ("totals" %in% components) {
      cli::cli_h3("Totals")
      print(x$totals)
    } else if ("total" %in% components) {
      cli::cli_h3("Total")
      cli::cli_text("Total: {format(x$total, big.mark = ',', scientific = FALSE)}")
    }
    
    # Check for messages
    if ("messages" %in% components && length(x$messages) > 0) {
      cli::cli_h3("Messages")
      cli::cli_bullets(x$messages)
    }
  }
  
  # Print class information
  cli::cli_h3("Object Classes")
  cli::cli_text("Class: {paste(class(x), collapse = ', ')}")
  
  invisible(x)
}