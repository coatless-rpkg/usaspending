#' Make a call to the USASpending API
#' 
#' @param endpoint API endpoint to call
#' @param ... Name-value pairs giving API parameters
#' @param .method HTTP method to use ("GET" or "POST")
#' @param .api_url The base URL for the API
#' @param .api_version The API version to use
#' @param .page_all Logical, whether to automatically fetch all pages
#' @param .limit Number of results per page (max 100)
#' @param .as_tibble Logical, whether to return response as a tibble
#' @param .flatten Logical, whether to flatten nested structures in tibble output
#' @return Response from the API, either as a tibble or raw response
#' @export
#' @examplesIf interactive()
#' # Get single page of results as a tibble
#' usasp("/agency/awards/count", fiscal_year = 2023)
#' 
#' # Get all pages of flattened results
#' usasp("/agency/awards/count", fiscal_year = 2023, 
#'       .page_all = TRUE, .flatten = TRUE)
#'       
#' # Send POST request with request body
#' usasp("/search/spending_by_award",
#'       filters = list(
#'         award_type = c("contracts"),
#'         time_period = list(list(
#'           start_date = "2023-01-01",
#'           end_date = "2023-12-31"
#'         ))
#'       ),
#'       fields = c("recipient_name", "total_obligation"),
#'       .method = "POST")
#'       
#' # Get raw JSON response
#' usasp("/agency/awards/count", fiscal_year = 2023, .as_tibble = FALSE)
usasp <- function(endpoint, ..., 
                  .method = "GET",
                  .api_url = "https://api.usaspending.gov",
                  .api_version = "v2",
                  .page_all = FALSE,
                  .limit = 10,
                  .as_tibble = TRUE,
                  .flatten = FALSE) {
  
  # Validate inputs
  check_endpoint(endpoint)
  check_api_url(.api_url)
  check_api_version(.api_version)
  check_page_all(.page_all)
  check_page_limit(.limit)
  
  # Process dots for both path and query parameters
  dots <- list(...)
  
  # Handle path parameters (those in {brackets})
  path_params <- regmatches(endpoint, gregexpr("\\{[^}]+\\}", endpoint))[[1]]
  if (length(path_params) > 0) {
    path_params <- gsub("[\\{\\}]", "", path_params)
    missing_params <- setdiff(path_params, names(dots))
    if (length(missing_params) > 0) {
      cli::cli_abort(c(
        "Missing required path parameters",
        "x" = "Required parameters: {paste(missing_params, collapse = ', ')}"
      ))
    }
    
    for (param in path_params) {
      endpoint <- gsub(paste0("\\{", param, "\\}"), dots[[param]], endpoint)
      dots[[param]] <- NULL
    }
  }
  
  # Remove leading/trailing slashes from endpoint
  endpoint <- gsub("^/+|/+$", "", endpoint)
  
  # Build initial request
  req <- httr2::request(.api_url) |>
    httr2::req_url_path_append(paste0("api/", .api_version)) |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_headers(Accept = "application/json") |>
    httr2::req_user_agent("usaspending-r-api-client") |>
    httr2::req_error(is_error = function(resp) FALSE)  # Don't treat HTTP errors as R errors
  
  # Validate HTTP method
  .method <- toupper(.method)
  if (!.method %in% c("GET", "POST")) {
    cli::cli_abort("HTTP method must be either 'GET' or 'POST'")
  }
  
  # Handle parameters based on method
  if (.method == "GET") {
    if (length(dots) > 0 || .limit != 10) {
      query_params <- dots
      query_params$limit <- .limit
      req <- httr2::req_url_query(req, !!!query_params)
    }
  } else {
    # For POST, add parameters as JSON body
    body_params <- dots
    if (.limit != 10) {
      body_params$limit <- .limit
    }
    req <- req |>
      httr2::req_method("POST") |>
      httr2::req_headers("Content-Type" = "application/json") |>
      httr2::req_body_json(body_params)
  }
  
  # Make initial request
  first_resp <- httr2::req_perform(req)
  
  # Check for HTTP errors with detailed diagnostics
  check_api_response(first_resp, endpoint, .method)
  
  # Check response type
  check_response_type(first_resp)
  data <- httr2::resp_body_json(first_resp)
  
  # Handle pagination if needed
  if (.page_all && has_pagination(data) && data$page_metadata$hasNext) {
    total_pages <- ceiling(data$page_metadata$total / .limit)
    
    if (total_pages > 1) {
      cli::cli_alert_info("Fetching {total_pages} pages...")
      
      # Setup pagination request
      paginated_req <- httr2::req_url_query(req, page = 2) # Start from page 2
      
      # Store all raw responses
      all_responses <- list(data)
      
      responses <- httr2::req_perform_iterative(
        paginated_req,
        next_req = function(resp, req) {
          resp_data <- httr2::resp_body_json(resp)
          page <- resp_data$page_metadata$page
          
          if (page >= total_pages) {
            return(NULL)
          }
          
          httr2::req_url_query(req, page = page + 1)
        },
        max_reqs = total_pages - 1
      )
      
      # Combine all results
      for (resp in responses) {
        resp_data <- httr2::resp_body_json(resp)
        all_responses <- c(all_responses, list(resp_data))
      }
      
      # Combine results maintaining structure
      combined_results <- list()
      for (resp in all_responses) {
        if (!is.null(resp$results)) {
          if (is.list(resp$results) && length(resp$results) == 1 && 
              is.list(resp$results[[1]])) {
            # Handle nested list case
            combined_results <- c(combined_results, resp$results[[1]])
          } else {
            combined_results <- c(combined_results, resp$results)
          }
        }
      }
      
      # Update the data structure
      data$results <- list(combined_results)
      
      # Update metadata
      data$page_metadata$page <- 1
      data$page_metadata$hasNext <- FALSE
      data$page_metadata[['next']] <- NULL
    }
  }
  
  # Clean endpoint by removing leading/trailing slashes
  clean_endpoint <- gsub("^/+|/+$", "", endpoint)
  
  # Determine response class
  if (grepl("\\{.*\\}", endpoint)) {
    # For parameterized endpoints (e.g., "/agency/{toptier_code}")
    # Get the base path before any parameters
    parts <- strsplit(clean_endpoint, "/")[[1]]
    base_parts <- parts[!grepl("\\{.*\\}", parts)]
    class_name <- paste0("usasp_", paste(base_parts, collapse = "_"))
  } else {
    # For regular endpoints, get the path parts before any numeric segments
    parts <- strsplit(clean_endpoint, "/")[[1]]
    # Keep parts until we hit a numeric segment
    path_parts <- parts[!grepl("^\\d+$", parts)]
    class_name <- paste0("usasp_", paste(path_parts, collapse = "_"))
  }
  
  # Create response object
  resp <- structure(data, 
                    class = c(class_name, "usasp_response"),
                    endpoint = clean_endpoint)
  
  # Transform to tibble if requested
  if (.as_tibble) {
    return(as_tibble(resp, flatten = .flatten))
  }
  
  resp
}


#' Check if a response has pagination
#' 
#' @param x Response to check
#' @return TRUE if response has pagination metadata
#' @keywords internal
has_pagination <- function(x) {
  !is.null(x$page_metadata) &&
    all(c("page", "total", "limit", "hasNext") %in% names(x$page_metadata))
}

#' Print method for usasp_response objects
#' 
#' @param x A usasp_response object
#' @param ... Additional arguments passed to print
#' @export
print.usasp_response <- function(x, ...) {
  # Get endpoint type from class and format for display
  endpoint_type <- sub("usasp_", "", class(x)[1])
  title <- tools::toTitleCase(gsub("_", " ", endpoint_type))
  
  cat("Agency Overview\n\n")
  
  NextMethod()
}
