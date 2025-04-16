test_that("Defensive; fail fast", {
  expect_error(glm_fixit(y ~ x || w + z, data = research_data, data2 = val_data, proxy_family = gaussian()))
  expect_error(glm_fixit(y ~ x || w + z, data = research_data, data2 = val_data, truth_family = gaussian()))
  expect_error(glm_fixit(y ~ x || w + z, data = research_data, data2 = val_data, truth_family = gaussian(), proxy_family = gaussian()))
})

test_that("Base case IV Gaussian", {
  expect_error(glm_fixit(y ~ x || w + z, data = research_data, data2 = val_data), NA)
})

test_that("Base case IV Binomial", {
  val_data3 <- val_data
  research_data3 <- research_data
  research_data3$y <- research_data$y >= median(research_data$y)
  val_data3$y <- val_data$y >= median(val_data$y)
  expect_error(glm_fixit(y ~ x || w + z, data = research_data3, data2 = val_data3, family = binomial()), NA)
})

test_that(".conv_formula works correctly", {
  f <- .conv_formula(y ~ x || w ~ z || y ~ w)

  expect_type(f, "list")
  expect_true("outcome_formula" %in% names(f))
  expect_true("naive_formula" %in% names(f))
  expect_true("proxy_formula" %in% names(f))

  expect_true(inherits(f$outcome_formula, "formula"))
  expect_true(inherits(f$proxy_formula, "formula"))
  expect_true(inherits(f$naive_formula, "formula"))
})

test_that(".conv_formula works with multiple proxy variables", {
  f <- .conv_formula(y ~ x || w ~ x + a || b ~ x || y ~ 1)
  expect_type(f$proxy_formula, "list")
})
