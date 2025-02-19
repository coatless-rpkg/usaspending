httptest2::with_mock_dir("mocks/agency/agency_overview", {
  testthat::test_that("agency_overview() makes correct API call", {
    response <- agency_overview("086")
    
    testthat::expect_s3_class(response, "usasp_tibble")
    testthat::expect_true(all(c("toptier_code", "name", "abbreviation", "mission") %in% names(response)))
    
  })
})

httptest2::with_mock_dir("mocks/agency/agency_awards", {
  testthat::test_that("agency_awards() makes correct API call", {
    response <- agency_awards("086", fiscal_year = 2023)
    
    testthat::expect_s3_class(response, "usasp_tibble")
    testthat::expect_true(all(c("toptier_code", "fiscal_year", "transaction_count", "obligations") %in% names(response)))
    
  })
})

httptest2::with_mock_dir("mocks/agency/agency_budget", {
  testthat::test_that("agency_budget_resources() makes correct API call", {
    response <- agency_budget_resources("086")
    
    testthat::expect_s3_class(response, "usasp_tibble")
    testthat::expect_true(all(c("toptier_code", "agency_data_by_year") %in% names(response)))

  })
})

httptest2::with_mock_dir("mocks/agency/agency_object_class", {
  testthat::test_that("agency_object_class() makes correct API call", {
    response <- agency_object_class("086", fiscal_year = 2023)
    
    testthat::expect_s3_class(response, "usasp_tibble")
    testthat::expect_true(all(c("name", "obligated_amount", "gross_outlay_amount") %in% names(response)))

  })
})

httptest2::with_mock_dir("mocks/agency/agency_program_activity", {
  testthat::test_that("agency_program_activity() makes correct API call", {
    response <- agency_program_activity("086", fiscal_year = 2023)
    
    testthat::expect_s3_class(response, "usasp_tibble")
    testthat::expect_true(all(c("name", "obligated_amount", "gross_outlay_amount") %in% names(response)))
    
  })
})

httptest2::with_mock_dir("mocks/agency/agency_awards_count", {
  testthat::test_that("agency_awards_count() makes correct API call", {
    response <- agency_awards_count("086", fiscal_year = 2023)
    
    testthat::expect_s3_class(response, "usasp_tibble")
    testthat::expect_true(all(c("toptier_code", "fiscal_year", "new_award_count") %in% names(response)))
    
  })
})

httptest2::with_mock_dir("mocks/agency/agency_awards_count_all", {
  testthat::test_that("agency_awards_count_all() makes correct API call", {
    response <- agency_awards_count_all(fiscal_year = 2023)
    
    testthat::expect_s3_class(response, "usasp_tibble")
    testthat::expect_true(all(c("awarding_toptier_agency_name", "awarding_toptier_agency_code", "contracts",
                            "direct_payments", "grants",
                            "idvs", "loans", "other") %in% names(response)))

  })
})

httptest2::with_mock_dir("mocks/agency/agency_federal_account", {
  testthat::test_that("agency_federal_account() makes correct API call", {
    response <- agency_federal_account("086", fiscal_year = 2023)
    
    testthat::expect_s3_class(response, "usasp_tibble")
    testthat::expect_true(all(c("code", "name", "obligated_amount") %in% names(response)))

  })
})

httptest2::with_mock_dir("mocks/agency/agency_sub_agency", {
  testthat::test_that("agency_sub_agency() makes correct API call", {
    response <- agency_sub_agency("086", fiscal_year = 2023)
    
    testthat::expect_s3_class(response, "usasp_tibble")
    testthat::expect_true(all(c("name", "abbreviation", "total_obligations") %in% names(response)))

  })
})

httptest2::with_mock_dir("mocks/agency/agency_sub_components", {
  testthat::test_that("agency_sub_components() makes correct API call", {
    response <- agency_sub_components("086", fiscal_year = 2023)
    
    testthat::expect_s3_class(response, "usasp_tibble")
    testthat::expect_true(all(c("name", "id", "total_budgetary_resources") %in% names(response)))

  })
})
