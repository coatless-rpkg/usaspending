% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/agency.R
\name{agency_federal_account}
\alias{agency_federal_account}
\alias{agency_federal_account_count}
\title{Agency Federal Accounts}
\usage{
agency_federal_account(
  toptier_code,
  fiscal_year = NULL,
  .page_all = FALSE,
  ...
)

agency_federal_account_count(toptier_code, fiscal_year = NULL, ...)
}
\arguments{
\item{toptier_code}{A character string containing a 3-4 digit numeric code that uniquely
identifies a top-tier federal agency (e.g., "086" for Department of Housing
and Urban Development). Must be a valid CGAC (Common Government-wide Accounting
Classification) or FREC (Federal Record Classification) code.}

\item{fiscal_year}{Optional numeric value specifying the fiscal year for the query.
If NULL (default), the API will use the current fiscal year. Must be 2017 or later.}

\item{.page_all}{Logical indicating whether to retrieve all pages of results (TRUE)
or just the first page (FALSE, default). When TRUE, the function will automatically
handle pagination to retrieve the complete dataset.}

\item{...}{Additional parameters passed to specific API endpoints. Common options include:
\itemize{
\item \code{page}: Integer specifying the page number to retrieve (default: 1)
\item \code{limit}: Integer specifying number of records per page (default: 10)
\item \code{sort}: Field to sort results by (varies by endpoint)
\item \code{order}: Sort direction: "asc" or "desc" (default varies by endpoint)
}}
}
\value{
Each function returns a tibble with different structures:
\itemize{
\item \code{agency_federal_account()} returns:
\itemize{
\item \code{code}: Federal account code
\item \code{name}: Account name
\item \code{total_budgetary_resources}: Total resources
\item \code{obligated_amount}: Amount obligated
\item \code{gross_outlay_amount}: Gross outlay amount
\item \code{children}: Nested data frame of treasury account details
}
\item \code{agency_federal_account_count()} returns:
\itemize{
\item \code{toptier_code}: Agency identifier code
\item \code{fiscal_year}: Fiscal year
\item \code{federal_account_count}: Number of federal accounts
\item \code{treasury_account_count}: Number of treasury accounts
}
}
}
\description{
Functions to retrieve federal account information for agencies, including
account details, counts, and associated treasury accounts.
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# Get federal accounts for HUD
fed_accounts <- agency_federal_account("086", 
                                      fiscal_year = 2023,
                                      .page_all = TRUE)

# Get account counts
account_counts <- agency_federal_account_count("086", fiscal_year = 2023)
\dontshow{\}) # examplesIf}
}
\seealso{
\code{\link[=agency_treasury_account_object_class]{agency_treasury_account_object_class()}}, \code{\link[=agency_treasury_account_program_activity]{agency_treasury_account_program_activity()}},
\code{\link[=agency_budget_resources]{agency_budget_resources()}}
}
