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
#'   \item agency_federal_account(): Federal accounts associated with the agency
#'   \item agency_federal_account_count(): Count of federal accounts and treasury accounts
#'   \item agency_object_classes(): Object class spending for the agency
#'   \item agency_program_activities(): Program activities for the agency
#'   \item agency_sub_agencies(): Sub-agencies within the main agency
#'   \item agency_budget_function_count(): Count of budget functions
#'   \item agency_budget_functions(): Detailed budget function data
#'   \item agency_budget_resources(): Overview of agency budgetary resources
#'   \item agency_obligations_by_award_category(): Obligation amounts by award category
#'   \item agency_object_class_count(): Count of object classes
#'   \item agency_program_activity_count(): Count of program activity categories
#'   \item agency_sub_agency_count(): Count of sub-agencies and offices
#'   \item agency_sub_components(): List of bureaus for the agency
#'   \item agency_sub_components_accounts(): Federal accounts by bureau
#'   \item treasury_account_object_class(): Object classes for a treasury account
#'   \item treasury_account_program_activity(): Program activities for a treasury account
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
agency_federal_account <- function(toptier_code, fiscal_year = NULL, .page_all = FALSE, ...) {
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
agency_federal_account_count <- function(toptier_code, fiscal_year = NULL, ...) {
  check_toptier_code(toptier_code)
  check_fiscal_year_optional(fiscal_year)
  usasp("/agency/{toptier_code}/federal_account/count",
        toptier_code = toptier_code,
        fiscal_year = fiscal_year,
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
agency_object_class_count <- function(toptier_code, fiscal_year = NULL, ...) {
  check_toptier_code(toptier_code)
  check_fiscal_year_optional(fiscal_year)
  usasp("/agency/{toptier_code}/object_class/count",
        toptier_code = toptier_code,
        fiscal_year = fiscal_year,
        ...)
}

#' @rdname agency
#' @export
agency_obligations_by_award_category <- function(toptier_code, fiscal_year = NULL, ...) {
  check_toptier_code(toptier_code)
  check_fiscal_year_optional(fiscal_year)
  usasp("/agency/{toptier_code}/obligations_by_award_category",
        toptier_code = toptier_code,
        fiscal_year = fiscal_year,
        ...)
}

#' @rdname agency
#' @export
agency_program_activity <- function(toptier_code, fiscal_year = NULL, .page_all = FALSE, ...) {
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
agency_program_activity_count <- function(toptier_code, fiscal_year = NULL, .page_all = FALSE, ...) {
  check_toptier_code(toptier_code)
  check_fiscal_year_optional(fiscal_year)
  usasp("/agency/{toptier_code}/program_activity/count",
        toptier_code = toptier_code,
        fiscal_year = fiscal_year,
        .page_all = .page_all,
        ...)
}

#' @rdname agency
#' @export
agency_sub_agency <- function(toptier_code, fiscal_year = NULL, .page_all = FALSE, ...) {
  check_toptier_code(toptier_code)
  check_fiscal_year_optional(fiscal_year)
  usasp("/agency/{toptier_code}/sub_agency",
        toptier_code = toptier_code,
        fiscal_year = fiscal_year,
        .page_all = .page_all,
        ...)
}

#' @rdname agency
#' @export
agency_sub_agency_count <- function(toptier_code, fiscal_year = NULL, .page_all = FALSE, ...) {
  check_toptier_code(toptier_code)
  check_fiscal_year_optional(fiscal_year)
  usasp("/agency/{toptier_code}/sub_agency/count",
        toptier_code = toptier_code,
        fiscal_year = fiscal_year,
        .page_all = .page_all,
        ...)
}

#' @rdname agency
#' @export
agency_sub_components <- function(toptier_code, fiscal_year = NULL, ...) {
  check_toptier_code(toptier_code)
  check_fiscal_year_optional(fiscal_year)
  usasp("/agency/{toptier_code}/sub_components",
        toptier_code = toptier_code,
        fiscal_year = fiscal_year,
        ...)
}

#' @rdname agency
#' @param bureau_slug A character string slug identifier for the bureau
#' @export
agency_sub_components_accounts <- function(toptier_code, bureau_slug, fiscal_year = NULL, ...) {
  check_toptier_code(toptier_code)
  check_fiscal_year_optional(fiscal_year)
  usasp("/agency/{toptier_code}/sub_components/{bureau_slug}",
        toptier_code = toptier_code,
        bureau_slug = bureau_slug,
        fiscal_year = fiscal_year,
        ...)
}

#' @rdname agency
#' @param treasury_account_symbol A character string representing the Treasury Account Symbol (TAS)
#' @export
agency_treasury_account_object_class <- function(treasury_account_symbol, ...) {
  usasp("/agency/treasury_account/{treasury_account_symbol}/object_class",
        treasury_account_symbol = treasury_account_symbol,
        ...)
}

#' @rdname agency
#' @export
agency_treasury_account_program_activity <- function(treasury_account_symbol, ...) {
  usasp("/agency/treasury_account/{treasury_account_symbol}/program_activity",
        treasury_account_symbol = treasury_account_symbol,
        ...)
}
