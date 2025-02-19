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
  
  # Create main data frame
  if (!is.null(x$results)) {
    # Handle paginated results
    df <- transform_list_to_tibble(x$results)
  } else {
    # Handle non-paginated response by excluding metadata fields
    metadata_fields <- c("page_metadata", "messages")
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
  if (is.data.frame(x)) {
    return(tibble::as_tibble(x))
  }
  
  if (!is.list(x) || length(x) == 0) {
    return(tibble::tibble())
  }
  
  # If x is a single list that should be one row
  if (is.list(x) && !is.null(names(x)) && 
      !any(vapply(x, is.list, logical(1))) && 
      !any(vapply(x, is.data.frame, logical(1)))) {
    return(tibble::tibble(!!!x))
  }
  
  if (is.null(names(x))) {
    # If elements are named lists, combine them into rows
    if (all(vapply(x, is.list, logical(1))) && 
        all(vapply(x, function(l) !is.null(names(l)), logical(1)))) {
      # Convert list of lists to data frame rows
      rows_list <- lapply(x, function(.x) {
        result <- as.list(.x)
        result[vapply(result, is.null, logical(1))] <- NA
        # Handle nested lists within each element
        for (nm in names(result)) {
          if (is.list(result[[nm]]) && !is.data.frame(result[[nm]])) {
            if (all(vapply(result[[nm]], is.list, logical(1)))) {
              # Convert nested list to tibble
              result[[nm]] <- list(transform_list_to_tibble(result[[nm]]))
            }
          }
        }
        tibble::as_tibble(result)
      })
      
      # Combine all rows using do.call and rbind
      if (length(rows_list) > 0) {
        df <- do.call(rbind, c(rows_list, list(stringsAsFactors = FALSE)))
        df <- tibble::as_tibble(df)
      } else {
        df <- tibble::tibble()
      }
    } else {
      # Handle case where all elements are named lists with same structure
      if (length(x) > 0 && 
          all(vapply(x[[1]], function(el) is.list(el) || is.atomic(el), logical(1)))) {
        rows_list <- lapply(x[[1]], function(.x) tibble::tibble(!!!.x))
        if (length(rows_list) > 0) {
          df <- do.call(rbind, c(rows_list, list(stringsAsFactors = FALSE)))
          df <- tibble::as_tibble(df)
        } else {
          df <- tibble::tibble()
        }
      } else {
        # Create default names v1, v2, etc.
        names(x) <- paste0("v", seq_along(x))
        df <- tibble::as_tibble(x)
      }
    }
  } else {
    # Named list
    x[vapply(x, is.null, logical(1))] <- NA
    
    # Check if all elements are atomic (non-list)
    if (all(vapply(x, function(el) !is.list(el) && !is.data.frame(el), logical(1)))) {
      df <- tibble::tibble(!!!x)
    } else {
      # Handle nested lists by transforming them to tibbles
      for (nm in names(x)) {
        if (is.list(x[[nm]]) && !is.data.frame(x[[nm]])) {
          if (all(vapply(x[[nm]], is.list, logical(1)))) {
            x[[nm]] <- list(transform_list_to_tibble(x[[nm]]))
          }
        }
      }
      df <- tibble::as_tibble(x)
    }
  }
  
  df
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
  # Process each column
  flat_cols <- list()
  
  for (col in names(df)) {
    val <- df[[col]]
    
    if (is.list(val) && !is.data.frame(val)) {
      # Check if it's a list of consistent data that can be converted to a tibble
      if (all(vapply(val, is.list, logical(1))) && 
          length(unique(vapply(lapply(val, names), paste, collapse = ",", FUN.VALUE = ""))) == 1) {
        # Convert to a nested tibble instead of flattening
        tibble_list <- lapply(val, function(.x) {
          result <- as.list(.x)
          result[vapply(result, is.null, logical(1))] <- NA
          tibble::as_tibble(result)
        })
        
        if (length(tibble_list) > 0) {
          combined_tibble <- do.call(rbind, c(tibble_list, list(stringsAsFactors = FALSE)))
          flat_cols[[col]] <- list(tibble::as_tibble(combined_tibble))
        } else {
          flat_cols[[col]] <- list(tibble::tibble())
        }
      } else if (all(vapply(val, length, integer(1)) == 1)) {
        # Simple list of scalars
        flat_cols[[col]] <- unlist(val)
      } else {
        # Complex nested structure - create separate columns if possible
        nested_df <- tryCatch({
          # Use lapply and do.call to avoid using purrr
          list_data <- lapply(val, as.list)
          if (length(list_data) > 0) {
            combined_data <- do.call(rbind, c(list_data, list(stringsAsFactors = FALSE)))
            tibble::as_tibble(combined_data)
          } else {
            NULL
          }
        }, error = function(e) NULL)
        
        if (!is.null(nested_df) && ncol(nested_df) > 0) {
          new_names <- paste(col, names(nested_df), sep = "_")
          names(nested_df) <- new_names
          flat_cols <- c(flat_cols, as.list(nested_df))
        } else {
          # Keep as nested tibble if can't flatten
          flat_cols[[col]] <- val
        }
      }
    } else if (is.data.frame(val)) {
      # Keep nested data frames as is
      flat_cols[[col]] <- val
    } else {
      # Regular column
      flat_cols[[col]] <- val
    }
  }
  
  tibble::as_tibble(flat_cols)
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
    
    # Print column names
    cli::cli_text("Columns: {toString(names(x))}")
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
#' @param flatten Logical. If TRUE, attempts to flatten nested data structures
#'   into separate columns where possible. Complex nested structures may remain
#'   as list-columns if they cannot be flattened without data loss.
#' @param ... Additional arguments passed to tibble conversion methods
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
as_tibble.usasp_response <- function(x, flatten, ...) {
  
  df <- transform_response(x, flatten = flatten)
  
  # Add endpoint info and class
  attr(df, "endpoint") <- class(x)[1]
  class(df) <- c("usasp_tibble", class(df))
  
  df
}