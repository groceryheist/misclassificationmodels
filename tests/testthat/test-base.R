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
  f <- .conv_formula(y ~ x || w + z)

  expect_type(f, "list")
  expect_true("outcome_formula" %in% names(f))
  expect_true("proxy_formula" %in% names(f))
  expect_true("naive_formula" %in% names(f))
  expect_true("yproxy" %in% names(f))

  expect_s3_class(f$outcome_formula, "formula")
  expect_s3_class(f$proxy_formula, "formula")
  expect_s3_class(f$naive_formula, "formula")
  expect_false(f$yproxy)  # assuming this isn't a DV misclassification case
})

test_that(".measerr_mle_iv runs for 1 proxy variable", {
  # use the same formula structure
  formula <- y ~ x || w + z
  proxy_family <- list(binomial(link = "logit"))

  # get the transformed formula
  proxy_formulas <- conv_formula_updated(formula)

  # just test that it runs
  fit <- .measerr_mle_iv(
    df = dplyr::bind_rows(research_data, val_data),
    outcome_formula = formula(.conv_formula(formula)$outcome_formula),
    outcome_family = gaussian(),
    proxy_families = proxy_family,
    truth_families = list()
  )

  expect_type(fit$value, "double")  # negative log-likelihood value
  expect_s3_class(fit, "list")
  expect_true("par" %in% names(fit))
})
