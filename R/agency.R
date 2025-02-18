#' Access USAspending API Agency-Related Endpoints
#'
#' Functions to retrieve agency-related data from the USAspending API. These functions
#' provide access to agency overview information, budget information, award data, and award counts.
#'
#' @name agency
#' @param toptier_code A character string representing the unique identifier for a 
#'        top-tier federal agency (e.g., "086" for Department of Housing and Urban Development).
#' @param fiscal_year Optional numeric value specifying the fiscal year for the query.
#'        If NULL (default), returns data for all available fiscal years.
#' @param .page_all Logical indicating whether to retrieve all pages of results (TRUE)
#'        or just the first page (FALSE, default). Use TRUE for complete datasets,
#'        but be aware this may take longer for large result sets.
#' @param ... Additional parameters passed to the API endpoint (e.g., page, limit,
#'        sort, order). See USAspending API documentation for details.
#'
#' @section API Documentation:
#' For detailed information about the API endpoints and parameters, visit:
#' \url{https://api.usaspending.gov/docs/}
#'
#' @return A tibble containing the requested agency data. The structure varies by endpoint:
#' \itemize{
#'   \item agency_overview(): Basic agency information including name, code, and current data
#'   \item agency_awards(): Detailed award information including amounts and recipients
#'   \item agency_awards_count(): Count of awards by category and fiscal year
#'   \item agency_awards_count_all(): Government-wide award counts
#'   \item agency_federal_accounts(): Federal accounts associated with the agency
#'   \item agency_object_classes(): Object class spending for the agency
#'   \item agency_program_activities(): Program activities for the agency
#'   \item agency_sub_agencies(): Sub-agencies within the main agency
#'   \item agency_budget_function_count(): Count of budget functions
#'   \item agency_budget_functions(): Detailed budget function data
#'   \item agency_budget_resources(): Overview of agency budgetary resources
#' }
#'
#' @rdname agency
#' @examplesIf interactive()
#' # Get overview of Department of Housing and Urban Development
#' agency_overview("086")
#'
#' # Get 2023 fiscal year awards with complete pagination
#' awards <- agency_awards("086", 
#'                        fiscal_year = 2023, 
#'                        .page_all = TRUE)
#'
#' # Get award counts for specific fiscal year
#' award_counts <- agency_awards_count("086", fiscal_year = 2023)
#'
#' # Get all agency award counts with custom limit
#' all_counts <- agency_awards_count_all(fiscal_year = 2023, 
#'                                      limit = 100)
#'
#' # Get federal accounts for an agency
#' fed_accounts <- agency_federal_accounts("086", fiscal_year = 2023)
#'
#' # Get object classes with pagination
#' obj_classes <- agency_object_classes("086",
#'                                     fiscal_year = 2023,
#'                                     .page_all = TRUE)
#'
#' # Get program activities
#' programs <- agency_program_activities("086", fiscal_year = 2023)
#'
#' # Get sub-agencies
#' sub_agencies <- agency_sub_agencies("086")
NULL

#' @rdname agency
#' @export
agency_overview <- function(toptier_code, ...) {
  check_toptier_code(toptier_code)
  usasp("/agency/{toptier_code}", toptier_code = toptier_code, ...)
}

#' @rdname agency
#' @export
agency_awards <- function(toptier_code, fiscal_year = NULL, .page_all = FALSE, ...) {
  check_toptier_code(toptier_code)
  check_fiscal_year_optional(fiscal_year)
  usasp("/agency/{toptier_code}/awards", 
        toptier_code = toptier_code, 
        fiscal_year = fiscal_year,
        .page_all = .page_all,
        ...)
}

#' @rdname agency
#' @export
agency_awards_count <- function(toptier_code, fiscal_year = NULL, ...) {
  check_toptier_code(toptier_code)
  check_fiscal_year_optional(fiscal_year)
  usasp("/agency/{toptier_code}/awards/new/count",
        toptier_code = toptier_code,
        fiscal_year = fiscal_year,
        ...)
}

#' @rdname agency
#' @export
agency_awards_count_all <- function(fiscal_year = NULL, .page_all = FALSE, ...) {
  check_fiscal_year_optional(fiscal_year)
  usasp("/agency/awards/count",
        fiscal_year = fiscal_year,
        .page_all = .page_all,
        ...)
}


#' @rdname agency
#' @export
agency_budget_function_count <- function(toptier_code, fiscal_year = NULL, ...) {
  check_toptier_code(toptier_code)
  check_fiscal_year_optional(fiscal_year)
  usasp("/agency/{toptier_code}/budget_function/count",
        toptier_code = toptier_code,
        fiscal_year = fiscal_year,
        ...)
}

#' @rdname agency
#' @export
agency_budget_functions <- function(toptier_code, fiscal_year = NULL, .page_all = FALSE, ...) {
  check_toptier_code(toptier_code)
  check_fiscal_year_optional(fiscal_year)
  usasp("/agency/{toptier_code}/budget_function",
        toptier_code = toptier_code,
        fiscal_year = fiscal_year,
        .page_all = .page_all,
        ...)
}

#' @rdname agency
#' @export
agency_budget_resources <- function(toptier_code, ...) {
  check_toptier_code(toptier_code)
  usasp("/agency/{toptier_code}/budgetary_resources",
        toptier_code = toptier_code,
        ...)
}

#' @rdname agency
#' @export
agency_federal_accounts <- function(toptier_code, fiscal_year = NULL, .page_all = FALSE, ...) {
  check_toptier_code(toptier_code)
  check_fiscal_year_optional(fiscal_year)
  usasp("/agency/{toptier_code}/federal_account",
        toptier_code = toptier_code,
        fiscal_year = fiscal_year,
        .page_all = .page_all,
        ...)
}

#' @rdname agency
#' @export
agency_object_classes <- function(toptier_code, fiscal_year = NULL, .page_all = FALSE, ...) {
  check_toptier_code(toptier_code)
  check_fiscal_year_optional(fiscal_year)
  usasp("/agency/{toptier_code}/object_class",
        toptier_code = toptier_code,
        fiscal_year = fiscal_year,
        .page_all = .page_all,
        ...)
}

#' @rdname agency
#' @export
agency_program_activities <- function(toptier_code, fiscal_year = NULL, .page_all = FALSE, ...) {
  check_toptier_code(toptier_code)
  check_fiscal_year_optional(fiscal_year)
  usasp("/agency/{toptier_code}/program_activity",
        toptier_code = toptier_code,
        fiscal_year = fiscal_year,
        .page_all = .page_all,
        ...)
}

#' @rdname agency
#' @export
agency_sub_agencies <- function(toptier_code, fiscal_year = NULL, .page_all = FALSE, ...) {
  check_toptier_code(toptier_code)
  check_fiscal_year_optional(fiscal_year)
  usasp("/agency/{toptier_code}/sub_agency",
        toptier_code = toptier_code,
        fiscal_year = fiscal_year,
        .page_all = .page_all,
        ...)
}
