#' Common Parameters for USA Spending API Agency Endpoints
#'
#' Documentation for common parameters used across USA Spending API agency endpoints
#'
#' @param toptier_code A character string containing a 3-4 digit numeric code that uniquely
#'        identifies a top-tier federal agency (e.g., "086" for Department of Housing
#'        and Urban Development). Must be a valid CGAC (Common Government-wide Accounting
#'        Classification) or FREC (Federal Record Classification) code.
#'
#' @param fiscal_year Optional numeric value specifying the fiscal year for the query.
#'        If NULL (default), the API will use the current fiscal year. Must be 2017 or later.
#'
#' @param .page_all Logical indicating whether to retrieve all pages of results (TRUE)
#'        or just the first page (FALSE, default). When TRUE, the function will automatically
#'        handle pagination to retrieve the complete dataset.
#'
#' @param ... Additional parameters passed to specific API endpoints. Common options include:
#'   * `page`: Integer specifying the page number to retrieve (default: 1)
#'   * `limit`: Integer specifying number of records per page (default: 10)
#'   * `sort`: Field to sort results by (varies by endpoint)
#'   * `order`: Sort direction: "asc" or "desc" (default varies by endpoint)
#'
#' @name usaspending-common-params
NULL

#' Agency Overview Information
#'
#' Retrieves basic information about a federal agency including name,
#' abbreviation, mission statement, website, and other metadata.
#'
#' @inheritParams usaspending-common-params
#'
#' @return
#' A tibble containing agency overview information:
#' * `fiscal_year`: Current fiscal year
#' * `toptier_code`: Agency identifier code
#' * `name`: Full agency name
#' * `abbreviation`: Agency abbreviation
#' * `agency_id`: Numeric agency identifier
#' * `icon_filename`: Agency icon file name
#' * `mission`: Agency mission statement
#' * `website`: Agency website URL
#' * `congressional_justification_url`: URL to budget justification
#' * `about_agency_data`: Additional agency information
#' * `subtier_agency_count`: Number of sub-agencies
#'
#' @seealso
#' [agency_sub_components()], [agency_budget_resources()]
#'
#' @examplesIf interactive()
#' # Get overview for Department of Housing and Urban Development
#' hud_overview <- agency_overview("086")
#'
#' @rdname agency_overview
#' @export
agency_overview <- function(toptier_code, ...) {
  check_toptier_code(toptier_code)
  usasp("/agency/{toptier_code}", toptier_code = toptier_code, ...)
}

#' Agency Awards Information
#'
#' Functions to retrieve award information for federal agencies, including counts,
#' transaction data, and new award metrics.
#'
#' @inheritParams usaspending-common-params
#'
#' @return
#' Each function returns a tibble with different structures:
#'
#' * `agency_awards()` returns:
#'   * `toptier_code`: Agency identifier code
#'   * `fiscal_year`: Fiscal year of the awards
#'   * `latest_action_date`: Date of most recent award action
#'   * `transaction_count`: Number of award transactions
#'   * `obligations`: Total award obligations amount
#'
#' * `agency_awards_count()` returns:
#'   * `toptier_code`: Agency identifier code
#'   * `fiscal_year`: Fiscal year requested
#'   * `new_award_count`: Count of new awards for the period
#'
#' * `agency_awards_count_all()` returns:
#'   * `page_metadata`: Pagination information
#'   * `results`: List of agencies with award counts by type
#'
#' @seealso
#' [agency_obligations_by_award_category()], [agency_overview()]
#'
#' @examplesIf interactive()
#' # Get 2023 awards for HUD with all pages
#' hud_awards <- agency_awards("086", 
#'                            fiscal_year = 2023,
#'                            .page_all = TRUE)
#'                            
#' # Get award counts for HUD in fiscal year 2023
#' award_counts <- agency_awards_count("086", fiscal_year = 2023)
#'
#' # Get award counts across all agencies
#' all_counts <- agency_awards_count_all(fiscal_year = 2023)
#'
#' @rdname agency_awards
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

#' @rdname agency_awards
#' @export
agency_awards_count <- function(toptier_code, fiscal_year = NULL, ...) {
  check_toptier_code(toptier_code)
  check_fiscal_year_optional(fiscal_year)
  usasp("/agency/{toptier_code}/awards/new/count",
        toptier_code = toptier_code,
        fiscal_year = fiscal_year,
        ...)
}

#' @rdname agency_awards
#' @export
agency_awards_count_all <- function(fiscal_year = NULL, .page_all = FALSE, ...) {
  check_fiscal_year_optional(fiscal_year)
  usasp("/agency/awards/count",
        fiscal_year = fiscal_year,
        .page_all = .page_all,
        ...)
}


#' Agency Budget Functions and Resources
#'
#' Functions to retrieve budget-related information for federal agencies, including
#' budget functions, budgetary resources, and obligations by award category.
#'
#' @inheritParams usaspending-common-params
#'
#' @return
#' Each function returns a tibble with different structures:
#'
#' * `agency_budget_functions()` returns:
#'   * `name`: Budget function name
#'   * `obligated_amount`: Amount obligated
#'   * `gross_outlay_amount`: Gross outlay amount
#'   * `children`: Nested data frame of subfunction details
#'
#' * `agency_budget_function_count()` returns:
#'   * `toptier_code`: Agency identifier code
#'   * `fiscal_year`: Fiscal year
#'   * `budget_function_count`: Number of budget functions
#'   * `budget_sub_function_count`: Number of budget subfunctions
#'
#' * `agency_budget_resources()` returns:
#'   * `fiscal_year`: Fiscal year
#'   * `agency_budgetary_resources`: Total agency budget
#'   * `agency_total_obligated`: Total obligations
#'   * `total_budgetary_resources`: Government-wide total budget
#'   * `agency_obligation_by_period`: Nested data frame of periodic obligations
#'
#' * `agency_obligations_by_award_category()` returns:
#'   * `total_aggregated_amount`: Total award obligations
#'   * `results`: List of obligation amounts by award category
#'
#' @seealso
#' [agency_federal_account()], [agency_object_class()]
#'
#' @examplesIf interactive()
#' # Get budget functions for HUD
#' budget_funcs <- agency_budget_functions("086", fiscal_year = 2023)
#'
#' # Get budget function counts
#' func_counts <- agency_budget_function_count("086", fiscal_year = 2023)
#'
#' # Get budgetary resources for HUD
#' budget_resources <- agency_budget_resources("086")
#'
#' # Get obligations by award category
#' award_obligations <- agency_obligations_by_award_category("086", fiscal_year = 2023)
#'
#' @rdname agency_budget
#' @export
agency_budget_function_count <- function(toptier_code, fiscal_year = NULL, ...) {
  check_toptier_code(toptier_code)
  check_fiscal_year_optional(fiscal_year)
  usasp("/agency/{toptier_code}/budget_function/count",
        toptier_code = toptier_code,
        fiscal_year = fiscal_year,
        ...)
}

#' @rdname agency_budget
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

#' @rdname agency_budget
#' @export
agency_budget_resources <- function(toptier_code, ...) {
  check_toptier_code(toptier_code)
  usasp("/agency/{toptier_code}/budgetary_resources",
        toptier_code = toptier_code,
        ...)
}


#' @rdname agency_budget
#' @export
agency_obligations_by_award_category <- function(toptier_code, fiscal_year = NULL, ...) {
  check_toptier_code(toptier_code)
  check_fiscal_year_optional(fiscal_year)
  usasp("/agency/{toptier_code}/obligations_by_award_category",
        toptier_code = toptier_code,
        fiscal_year = fiscal_year,
        ...)
}

#' Agency Federal Accounts
#'
#' Functions to retrieve federal account information for agencies, including
#' account details, counts, and associated treasury accounts.
#'
#' @inheritParams usaspending-common-params
#'
#' @return
#' Each function returns a tibble with different structures:
#'
#' * `agency_federal_account()` returns:
#'   * `code`: Federal account code
#'   * `name`: Account name
#'   * `total_budgetary_resources`: Total resources
#'   * `obligated_amount`: Amount obligated
#'   * `gross_outlay_amount`: Gross outlay amount
#'   * `children`: Nested data frame of treasury account details
#'
#' * `agency_federal_account_count()` returns:
#'   * `toptier_code`: Agency identifier code
#'   * `fiscal_year`: Fiscal year
#'   * `federal_account_count`: Number of federal accounts
#'   * `treasury_account_count`: Number of treasury accounts
#'
#' @seealso
#' [agency_treasury_account_object_class()], [agency_treasury_account_program_activity()], 
#' [agency_budget_resources()]
#'
#' @examplesIf interactive()
#' # Get federal accounts for HUD
#' fed_accounts <- agency_federal_account("086", 
#'                                       fiscal_year = 2023,
#'                                       .page_all = TRUE)
#'
#' # Get account counts
#' account_counts <- agency_federal_account_count("086", fiscal_year = 2023)
#'
#' @rdname agency_accounts
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


#' @rdname agency_accounts
#' @export
agency_federal_account_count <- function(toptier_code, fiscal_year = NULL, ...) {
  check_toptier_code(toptier_code)
  check_fiscal_year_optional(fiscal_year)
  usasp("/agency/{toptier_code}/federal_account/count",
        toptier_code = toptier_code,
        fiscal_year = fiscal_year,
        ...)
}


#' Agency Object Class Information
#'
#' Functions to retrieve object class data for federal agencies, including
#' counts and spending details.
#'
#' @inheritParams usaspending-common-params
#'
#' @return
#' Each function returns a tibble with different structures:
#'
#' * `agency_object_class()` returns:
#'   * `name`: Object class name
#'   * `obligated_amount`: Amount obligated
#'   * `gross_outlay_amount`: Gross outlay amount
#'   * `children`: Nested data frame of detailed object classes
#'
#' * `agency_object_class_count()` returns:
#'   * `toptier_code`: Agency identifier code
#'   * `fiscal_year`: Fiscal year
#'   * `object_class_count`: Number of object classes
#'
#' @seealso
#' [agency_program_activity()], [agency_treasury_account_object_class()]
#'
#' @examplesIf interactive()
#' # Get object classes for HUD
#' obj_classes <- agency_object_class("086",
#'                                   fiscal_year = 2023,
#'                                   .page_all = TRUE)
#'
#' # Get object class counts
#' class_counts <- agency_object_class_count("086", fiscal_year = 2023)
#'
#' @rdname agency_object_class
#' @export
agency_object_class <- function(toptier_code, fiscal_year = NULL, .page_all = FALSE, ...) {
  check_toptier_code(toptier_code)
  check_fiscal_year_optional(fiscal_year)
  usasp("/agency/{toptier_code}/object_class",
        toptier_code = toptier_code,
        fiscal_year = fiscal_year,
        .page_all = .page_all,
        ...)
}

#' @rdname agency_object_class
#' @export
agency_object_class_count <- function(toptier_code, fiscal_year = NULL, ...) {
  check_toptier_code(toptier_code)
  check_fiscal_year_optional(fiscal_year)
  usasp("/agency/{toptier_code}/object_class/count",
        toptier_code = toptier_code,
        fiscal_year = fiscal_year,
        ...)
}

#' Agency Program Activities
#'
#' Functions to retrieve program activity information for federal agencies, including
#' details and counts.
#'
#' @inheritParams usaspending-common-params
#'
#' @return
#' Each function returns a tibble with different structures:
#'
#' * `agency_program_activity()` returns:
#'   * `name`: Program activity name
#'   * `obligated_amount`: Amount obligated
#'   * `gross_outlay_amount`: Gross outlay amount
#'
#' * `agency_program_activity_count()` returns:
#'   * `toptier_code`: Agency identifier code
#'   * `fiscal_year`: Fiscal year
#'   * `program_activity_count`: Number of program activities
#'
#' @seealso
#' [agency_object_class()], [agency_treasury_account_program_activity()]
#'
#' @examplesIf interactive()
#' # Get program activities for HUD
#' programs <- agency_program_activity("086", 
#'                                    fiscal_year = 2023,
#'                                    .page_all = TRUE)
#'
#' # Get program activity counts
#' program_counts <- agency_program_activity_count("086", fiscal_year = 2023)
#'
#' @rdname agency_program_activity
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

#' @rdname agency_program_activity
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


#' Agency Sub-Components and Sub-Agencies
#'
#' Functions to retrieve information about agency sub-components, sub-agencies,
#' and organizational structure.
#'
#' @inheritParams usaspending-common-params
#' @param bureau_slug A character string representing the URL-friendly identifier
#'        for a specific bureau (e.g., "bureau_of_the_census")
#'
#' @return
#' Each function returns a tibble with different structures:
#'
#' * `agency_sub_components()` returns:
#'   * `name`: Sub-component name
#'   * `id`: Sub-component identifier (bureau_slug)
#'   * `total_budgetary_resources`: Total resources
#'   * `total_obligations`: Total obligations
#'   * `total_outlays`: Total outlays
#'
#' * `agency_sub_components_accounts()` returns:
#'   * `name`: Federal account name
#'   * `id`: Federal account number
#'   * `total_budgetary_resources`: Total resources
#'   * `total_obligations`: Total obligations
#'   * `total_outlays`: Total outlays
#'
#' * `agency_sub_agency()` returns:
#'   * `name`: Sub-agency name
#'   * `abbreviation`: Sub-agency abbreviation
#'   * `total_obligations`: Total obligations
#'   * `transaction_count`: Number of transactions
#'   * `new_award_count`: Number of new awards
#'   * `children`: Nested data frame of offices
#'
#' * `agency_sub_agency_count()` returns:
#'   * `toptier_code`: Agency identifier code
#'   * `fiscal_year`: Fiscal year
#'   * `sub_agency_count`: Number of sub-agencies
#'   * `office_count`: Number of offices
#'
#' @seealso
#' [agency_overview()], [agency_federal_account()]
#'
#' @examplesIf interactive()
#' # Get sub-components for HUD
#' sub_comps <- agency_sub_components("086", fiscal_year = 2023)
#'
#' # Get federal accounts for a specific bureau
#' bureau_accounts <- agency_sub_components_accounts(
#'   "086",
#'   bureau_slug = "federal_housing_administration",
#'   fiscal_year = 2023
#' )
#'
#' # Get sub-agencies
#' sub_agencies <- agency_sub_agency("086", fiscal_year = 2023)
#'
#' # Get sub-agency counts
#' agency_counts <- agency_sub_agency_count("086", fiscal_year = 2023)
#'
#' @rdname agency_sub_components
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

#' @rdname agency_sub_components
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

#' @rdname agency_sub_components
#' @export
agency_sub_components <- function(toptier_code, fiscal_year = NULL, ...) {
  check_toptier_code(toptier_code)
  check_fiscal_year_optional(fiscal_year)
  usasp("/agency/{toptier_code}/sub_components",
        toptier_code = toptier_code,
        fiscal_year = fiscal_year,
        ...)
}

#' @rdname agency_sub_components
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

#' Treasury Account Operations
#'
#' Functions to retrieve detailed information about specific treasury accounts
#' including object classes and program activities.
#'
#' @param treasury_account_symbol A character string representing the Treasury 
#'        Account Symbol (TAS) in the format "XXX-X-XXXX-XXX"
#' @inheritParams usaspending-common-params
#'
#' @return
#' Each function returns a tibble with different structures:
#'
#' * `agency_treasury_account_object_class()` returns:
#'   * `treasury_account_symbol`: TAS code
#'   * `fiscal_year`: Fiscal year
#'   * `results`: List of object classes with financial data
#'
#' * `agency_treasury_account_program_activity()` returns:
#'   * `treasury_account_symbol`: TAS code
#'   * `fiscal_year`: Fiscal year
#'   * `results`: List of program activities with financial data
#'
#' @seealso
#' [agency_federal_account()], [agency_object_class()], [agency_program_activity()]
#'
#' @examplesIf interactive()
#' # Get object classes for a treasury account
#' tas_objects <- agency_treasury_account_object_class("086-X-0302-000")
#'
#' # Get program activities for a treasury account
#' tas_programs <- agency_treasury_account_program_activity("086-X-0302-000")
#'
#' @rdname agency_treasury_account
#' @export
agency_treasury_account_object_class <- function(treasury_account_symbol, ...) {
  usasp("/agency/treasury_account/{treasury_account_symbol}/object_class",
        treasury_account_symbol = treasury_account_symbol,
        ...)
}

#' @rdname agency_treasury_account
#' @export
agency_treasury_account_program_activity <- function(treasury_account_symbol, ...) {
  usasp("/agency/treasury_account/{treasury_account_symbol}/program_activity",
        treasury_account_symbol = treasury_account_symbol,
        ...)
}
