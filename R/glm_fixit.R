# ----------------------------
# Part 1: Data Preparation
# ----------------------------
prepare_data_sets <- function(df, outcome_formula, proxy_formula, truth_formula) {
  # Observed data: full data frame with complete information
  df_obs <- df
  
  # Predicted data: rows with missing ground truth variable
  truth.variable <- all.vars(truth_formula)[1]
  df_pred <- df[is.na(df[[truth.variable]]), ]
  
  # Create two copies of the predicted data:
  # One assuming the ground truth equals 0, and one assuming it equals 1.
  df_pred_x0 <- df_pred
  df_pred_x1 <- df_pred
  df_pred_x0[[truth.variable]] <- 0
  df_pred_x1[[truth.variable]] <- 1
  
  return(list(
    observed = df_obs,
    predicted_x0 = df_pred_x0,
    predicted_x1 = df_pred_x1
  ))
}

# ----------------------------
# Part 2: Log Likelihood Calculation
# ----------------------------
compute_observed_ll <- function(params, df, outcome_formula, outcome_family,
                                proxy_formula, proxy_family,
                                truth_formula, truth_family) {
  # Outcome component:
  df.outcome <- model.frame(outcome_formula, df)
  outcome.X <- model.matrix(outcome_formula, df)
  response.var <- all.vars(outcome_formula)[1]
  y.obs <- with(df.outcome, eval(parse(text = response.var)))
  
  if (outcome_family$family == "gaussian") {
    index_shift <- 1
    outcome.llfun <- ll.gaussian  # user-defined likelihood function for gaussian
  } else if ((outcome_family$family == "binomial") && (outcome_family$link == "logit")) {
    index_shift <- 0
    outcome.llfun <- ll.logistic  # user-defined likelihood function for logistic regression
  }
  n.out <- ncol(outcome.X)
  outcome.params <- params[1:(n.out + index_shift)]
  
  # Proxy component:
  proxy.X <- model.matrix(proxy_formula, df)
  proxy.variable <- all.vars(proxy_formula)[1]
  df.proxy <- model.frame(proxy_formula, df)
  proxy.obs <- with(df.proxy, eval(parse(text = proxy.variable)))
  n.prox <- ncol(proxy.X)
  proxy.params <- params[(n.out + index_shift + 1):(n.out + index_shift + n.prox)]
  proxy.llfun <- ll.logistic  # supports only binomial(logit)
  
  # Truth component:
  truth.X <- model.matrix(truth_formula, df)
  truth.variable <- all.vars(truth_formula)[1]
  df.truth <- model.frame(truth_formula, df)
  truth.obs <- with(df.truth, eval(parse(text = truth.variable)))
  n.tru <- ncol(truth.X)
  truth.params <- params[(n.out + index_shift + n.prox + 1):(n.out + index_shift + n.prox + n.tru)]
  truth.llfun <- ll.logistic  # supports only binomial(logit)
  
  # Compute log likelihoods for observed data:
  ll.outcome <- outcome.llfun(y.obs, outcome.params, outcome.X)
  ll.proxy   <- proxy.llfun(proxy.obs, proxy.params, proxy.X)
  ll.truth   <- truth.llfun(truth.obs, truth.params, truth.X)
  
  ll_obs_total <- ll.outcome + ll.proxy + ll.truth
  return(ll_obs_total)
}

compute_predicted_ll <- function(params, df_pred_x0, df_pred_x1,
                                 outcome_formula, outcome_family,
                                 proxy_formula, truth_formula) {
  # Outcome component for predicted data:
  outcome.X.x0 <- model.matrix(outcome_formula, df_pred_x0)
  outcome.X.x1 <- model.matrix(outcome_formula, df_pred_x1)
  response.var <- all.vars(outcome_formula)[1]
  y.pred <- with(df_pred_x0, eval(parse(text = response.var)))  # same outcome values used
  
  if (outcome_family$family == "gaussian") {
    index_shift <- 1
    outcome.llfun <- ll.gaussian
  } else if ((outcome_family$family == "binomial") && (outcome_family$link == "logit")) {
    index_shift <- 0
    outcome.llfun <- ll.logistic
  }
  n.out <- ncol(model.matrix(outcome_formula, df_pred_x0))
  outcome.params <- params[1:(n.out + index_shift)]
  
  ll.y.x0 <- outcome.llfun(y.pred, outcome.params, outcome.X.x0)
  ll.y.x1 <- outcome.llfun(y.pred, outcome.params, outcome.X.x1)
  
  # Proxy component for predicted data:
  proxy.X.x0 <- model.matrix(proxy_formula, df_pred_x0)
  proxy.X.x1 <- model.matrix(proxy_formula, df_pred_x1)
  proxy.variable <- all.vars(proxy_formula)[1]
  proxy.pred <- df_pred_x0[[proxy.variable]]
  n.prox <- ncol(model.matrix(proxy_formula, df_pred_x0))
  proxy.params <- params[(n.out + index_shift + 1):(n.out + index_shift + n.prox)]
  proxy.llfun <- ll.logistic
  ll.w.x0 <- proxy.llfun(proxy.pred, proxy.params, proxy.X.x0)
  ll.w.x1 <- proxy.llfun(proxy.pred, proxy.params, proxy.X.x1)
  
  # Truth component for predicted data:
  truth.X <- model.matrix(truth_formula, df_pred_x0)  # same structure for both copies
  truth.variable <- all.vars(truth_formula)[1]
  n.tru <- ncol(truth.X)
  truth.params <- params[(n.out + index_shift + n.prox + 1):(n.out + index_shift + n.prox + n.tru)]
  truth.llfun <- ll.logistic
  ll.x.x0 <- truth.llfun(df_pred_x0[[truth.variable]], truth.params, truth.X)
  ll.x.x1 <- truth.llfun(df_pred_x1[[truth.variable]], truth.params, truth.X)
  
  # Combine likelihoods for truth = 0 and truth = 1 using the log-sum-exp trick:
  ll_x0 <- ll.y.x0 + ll.w.x0 + ll.x.x0
  ll_x1 <- ll.y.x1 + ll.w.x1 + ll.x.x1
  
  ll_pred_total <- sum(matrixStats::colLogSumExps(rbind(ll_x0, ll_x1)))
  return(ll_pred_total)
}

# ----------------------------
# Part 3: Aggregation into the Main Likelihood Function and Updated glm_fixit
# ----------------------------
.measrr_mle_nll <- function(params, df, outcome_formula, outcome_family = gaussian(),
                             proxy_formula, proxy_family = binomial(link = 'logit'),
                             truth_formula, truth_family = binomial(link = 'logit')) {
  
  # Part 1: Data Preparation
  data_sets <- prepare_data_sets(df, outcome_formula, proxy_formula, truth_formula)
  
  # Part 2: Log Likelihood Calculation
  ll_obs <- compute_observed_ll(params, data_sets$observed, outcome_formula, outcome_family,
                                proxy_formula, proxy_family, truth_formula, truth_family)
  ll_pred <- compute_predicted_ll(params, data_sets$predicted_x0, data_sets$predicted_x1,
                                  outcome_formula, outcome_family, proxy_formula, truth_formula)
  
  # Part 3: Aggregation
  total_ll <- ll_obs + ll_pred
  
  # Return negative log likelihood (for minimization via optim())
  return(-total_ll)
}

glm_fixit <- function(formula, family = gaussian(), data, data2,
                      proxy_formula = NULL, proxy_family = binomial(link = 'logit'),
                      truth_formula = NULL, truth_family = binomial(link = 'logit'),
                      maxit = 1e6, method = 'L-BFGS-B') {
  
  if ((proxy_family$family != "binomial") || (proxy_family$link != 'logit')) {
    stop("Unsupported `proxy_family`. The proxy family should be binomial(link='logit').", call. = FALSE)
  }
  if ((truth_family$family != "binomial") || (truth_family$link != 'logit')) {
    stop("Unsupported `truth_family`. The truth family should be binomial(link='logit').", call. = FALSE)
  }
  
  parsed_formula <- .conv_formula(formula)
  if (isTRUE(parsed_formula$yproxy) && (family$family != "binomial" || family$link != "logit")) {
    stop("Only logistic regression is supported for dependent variable with misclassification.", call. = FALSE)
  }
  
  # Combine primary and validation data
  df <- vctrs::vec_rbind(data, data2)
  
  if (is.null(proxy_formula)) {
    proxy_formula <- formula(paste0(parsed_formula$proxy, "~."))
  }
  if (is.null(truth_formula)) {
    truth_formula <- formula(paste0(parsed_formula$truth, "~ 1"))
  }
  
  # Choose the appropriate likelihood function based on the dependent variable type.
  mla_function <- if (isFALSE(parsed_formula$yproxy)) .measrr_mle_iv else .measrr_mle_dv        
  
  # Call our refactored likelihood function.
  res <- mla_function(df, outcome_formula = formula(parsed_formula$outcome_formula),
                      outcome_family = family, proxy_formula = proxy_formula,
                      truth_formula = truth_formula, truth_family = truth_family,
                      maxit = maxit, method = method)
  
  # Fit naive and feasible models for comparison
  naive <- glm(formula = formula(parsed_formula$naive_formula), family = family, data = data)
  feasible <- glm(formula = formula(parsed_formula$outcome_formula), family = family, data = data2)
  
  res$naive <- naive
  res$feasible <- feasible
  res$formula <- formula
  res$family <- family
  res$proxy_formula <- proxy_formula
  res$proxy_family <- proxy_family
  res$truth_formula <- truth_formula
  res$truth_family <- truth_family
  class(res) <- c("glm_fixit", class(res))
  
  return(res)
}
