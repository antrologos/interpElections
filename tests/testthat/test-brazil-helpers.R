test_that("br_prepare_population has proper signature", {
  expect_true(is.function(br_prepare_population))
  expect_true("code_muni" %in% names(formals(br_prepare_population)))
  expect_true("year" %in% names(formals(br_prepare_population)))
})

test_that("br_prepare_electoral requires proper packages", {
  expect_true(is.function(br_prepare_electoral))
  expect_true("code_muni_ibge" %in% names(formals(br_prepare_electoral)))
  expect_true("year" %in% names(formals(br_prepare_electoral)))
  expect_true("cargo" %in% names(formals(br_prepare_electoral)))
})

test_that("br_prepare_tracts requires proper packages", {
  expect_true(is.function(br_prepare_tracts))
  expect_true("code_muni" %in% names(formals(br_prepare_tracts)))
  expect_true("pop_data" %in% names(formals(br_prepare_tracts)))
})

# Integration tests (require internet, skip on CRAN)
test_that("br_prepare_population works for a small municipality", {
  skip_on_cran()
  skip_if_not_installed("geobr")
  skip_if_not_installed("censobr")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("stringr")

  # Boa Vista, Roraima (small capital)
  result <- br_prepare_population(1400100)
  expect_true("code_tract" %in% names(result))
  expect_true("code_muni" %in% names(result))
  expect_true("pop_18_20" %in% names(result))
  expect_true("pop_60_69" %in% names(result))
  expect_true(nrow(result) > 0)
})
