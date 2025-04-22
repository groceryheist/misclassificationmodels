library(stringr)

test_that(".conv_formulas works", {
  expect_no_error({
    res <- .conv_formulas(list(y ~ x || w + z || v + a, w ~ x + z, v ~ a + z + x, x ~ z))
  })
  stopifnot(res$outcome_formula == as.formula(y ~ x + z + a))
  stopifnot(c(as.formula(w ~ x + z)) %in% res$proxy_formulas)
  stopifnot(c(as.formula(v ~ a + z + x)) %in% res$proxy_formulas)
  stopifnot(c(as.formula(x ~ z)) %in% res$truth_formulas)
  stopifnot(c(as.formula(z ~ 1)) %in% res$truth_formulas)
  stopifnot(as.formula(y ~ w + v + a) == res$naive_formula)
})

test_that("Defensive; fail fast", {
  expect_error(glm_fixit(y ~ x || w + z, w ~ x + z, data = research_data, data2 = val_data, proxy_family = gaussian()))
  expect_error(glm_fixit(y ~ x || w + z, w ~ x + z, data = research_data, data2 = val_data, truth_family = gaussian()))
  expect_error(glm_fixit(y ~ x || w + z, w ~ x + z, data = research_data, data2 = val_data, truth_family = gaussian(), proxy_family = gaussian()))
})


test_that("Base case IV Gaussian", {
  expect_no_error(glm_fixit(y ~ x || w + z, w ~ x + z, data = research_data, data2 = val_data))
})

test_that("log likelihood works correctly", {
  set.seed(1)
  expect_no_error({
    # parse formulas
    f_list <- .conv_formulas(list(y ~ x || w + a || v + z, w ~ x + z, v ~ a + z + x, x ~ z))

    data <- research_data_3
    data2 <- val_data_3
    
    proxy_formulas <- f_list$proxy_formulas
    truth_formulas <- f_list$truth_formulas
    
    # Convert formula early
    outcome_formula <- f_list$outcome_formula
    naive_formula <- f_list$naive_formula

    df <- vctrs::vec_rbind(data, data2)
  
  ## # Choose the appropriate likelihood function based on the dependent variable type.
  mla_function <- if (isFALSE(f_list$yproxy)) measerr_mle_iv else measerr_mle_dv
  
  n_proxy_params <- sum(sapply(proxy_formulas, function(f) {
    ncol(model.matrix(f, df))
  }))
  
  n_truth_params <- sum(sapply(truth_formulas, function(f) {
    ncol(model.matrix(f, df))
  }))

  n_outcome_params <- length(colnames(model.matrix(outcome_formula, df))) + 1

  params <- abs(rnorm(n_outcome_params +
                        n_proxy_params +
                        n_truth_params, sd = 0.01))

  missing_idx <- rep(FALSE, nrow(df))
  for (tf in truth_formulas) {
    f_terms <- terms(tf)
    truth_resp <- attr(f_terms, "variables")[[attr(f_terms, "response") + 1]]
    missing_idx <- is.na(df[[truth_resp]]) | missing_idx
  }

  observed_idx <- missing_idx == FALSE
  df[["observed"]] <- observed_idx
  df[is.na(df$z), ][["z"]] <- rbinom(300, 1,0.5)

  llik <- measerr_mle_iv(params, df,
                         outcome_formula = outcome_formula,
                         proxy_formulas = proxy_formulas,
                         truth_formulas = truth_formulas
                         )
    stopifnot(!is.na(llik))
  })
})

test_that("Base case IV Binomial", {
  val_data3 <- val_data
  research_data3 <- research_data
  research_data3$y <- research_data$y >= median(research_data$y)
  val_data3$y <- val_data$y >= median(val_data$y)
  expect_no_error(glm_fixit(y ~ x || w + z, w ~ x + z, data = research_data3, data2 = val_data3, family = binomial()))
})
