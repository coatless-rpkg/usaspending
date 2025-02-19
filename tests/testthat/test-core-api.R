httptest2::with_mock_dir("mocks/core/basic_request", {
  testthat::test_that("usasp handles basic requests correctly", {
    resp <- usasp("/agency/086")
    testthat::expect_s3_class(resp, "usasp_tibble")
  })
})


httptest2::with_mock_dir("mocks/core/path_params", {
  testthat::test_that("usasp handles path parameters correctly", {
      resp <- usasp("/agency/{toptier_code}", toptier_code = "086")
      testthat::expect_s3_class(resp, "usasp_tibble")
      testthat::expect_equal(resp$toptier_code, "086")
  })
})


testthat::test_that("usasp validates path parameters", {
  testthat::expect_error(
    usasp("/agency/{toptier_code}")
  )
})

testthat::test_that("usasp handles query parameters correctly", {
  httptest2::with_mock_dir("mocks/core/query_params", {
    resp <- usasp("/agency/awards/count", 
                  fiscal_year = 2023, 
                  group = "all")
    testthat::expect_s3_class(resp, "usasp_tibble")
  })
})

httptest2::with_mock_dir("mocks/core/pagination", {
  testthat::test_that("usasp handles pagination correctly", {
    # First page
    resp_single <- usasp("/agency/awards/count", 
                         fiscal_year = 2023, 
                         .page_all = FALSE)
    limit_total <- attr(resp_single, "pagination")$limit
    hasNext <- attr(resp_single, "pagination")$hasNext
    testthat::expect_equal(nrow(resp_single), limit_total)
    testthat::expect_true(hasNext)
    
    # All pages
    resp_all <- usasp("/agency/awards/count", 
                      fiscal_year = 2023, 
                      .page_all = TRUE)
    total <- attr(resp_all, "pagination")$total
    hasNext <- attr(resp_all, "pagination")$hasNext
    testthat::expect_equal(nrow(resp_all), total)
    testthat::expect_false(hasNext)
  })
})

httptest2::with_mock_dir("mocks/core/errors", {
  testthat::test_that("usasp handles API errors gracefully", {
    testthat::expect_error(
      usasp("/nonexistent/endpoint"),
      "API request failed"
    )
  })
})

testthat::test_that("usasp validates limit parameter", {
  testthat::expect_error(
    usasp("/agency/awards/count", .limit = 101)
  )
  testthat::expect_error(
    usasp("/agency/awards/count", .limit = 0)
  )
  testthat::expect_error(
    usasp("/agency/awards/count", .limit = -1)
  )
})
