# Assumes ll.gaussian, ll.logistic, and library(matrixStats) are available

# ----------------------------
# Part 1: Data Preparation
# ----------------------------
prepare_data_sets <- function(df, outcome_formula, proxy_formulas, truth_formula) {
  # Observed data: full data frame
  df_obs <- df
  
  # Predicted data: rows where the truth variable is missing (NA)
  truth.var <- all.vars(truth_formula)[1]
  df_pred  <- df[is.na(df[[truth.var]]), ]
  
  # Create two copies: truth = 0 and truth = 1
  df_x0 <- df_pred
  df_x1 <- df_pred
  df_x0[[truth.var]] <- 0
  df_x1[[truth.var]] <- 1
  
  list(
    observed     = df_obs,
    predicted_x0 = df_x0,
    predicted_x1 = df_x1
  )
}

# ----------------------------
# Part 2a: Log Likelihood for Observed Data
# ----------------------------
compute_observed_ll <- function(params, df,
                                outcome_formula, outcome_family,
                                proxy_formulas, truth_formula, truth_family) {
  # Outcome component
  df_outcome    <- model.frame(outcome_formula, df)
  X_outcome     <- model.matrix(outcome_formula, df)
  response.var  <- all.vars(outcome_formula)[1]
  y_obs         <- df_outcome[[response.var]]
  
  if (outcome_family$family == "gaussian") {
    shift         <- 1
    outcome_llfun <- ll.gaussian
  } else if (outcome_family$family == "binomial" && outcome_family$link == "logit") {
    shift         <- 0
    outcome_llfun <- ll.logistic
  } else {
    stop("Unsupported outcome family", call. = FALSE)
  }
  
  idx    <- 1
  n_out  <- ncol(X_outcome)
  thetao <- params[idx:(idx + n_out + shift - 1)]
  idx    <- idx + n_out + shift
  ll_out <- outcome_llfun(y_obs, thetao, X_outcome)
  
  # Proxy components (multiple)
  ll_proxy_total <- 0
  proxy_llfun    <- ll.logistic  # assume binomial(logit)
  for (pf in proxy_formulas) {
    Xp     <- model.matrix(pf, df)
    var_p  <- all.vars(pf)[1]
    w_obs  <- model.frame(pf, df)[[var_p]]
    n_p    <- ncol(Xp)
    thetap <- params[idx:(idx + n_p - 1)]
    ll_proxy_total <- ll_proxy_total + proxy_llfun(w_obs, thetap, Xp)
    idx <- idx + n_p
  }
  
  # Truth component
  df_truth    <- model.frame(truth_formula, df)
  X_truth     <- model.matrix(truth_formula, df)
  var_t       <- all.vars(truth_formula)[1]
  x_obs       <- df_truth[[var_t]]
  n_t         <- ncol(X_truth)
  thetat      <- params[idx:(idx + n_t - 1)]
  truth_llfun <- ll.logistic  # assume binomial(logit)
  ll_truth    <- truth_llfun(x_obs, thetat, X_truth)
  
  ll_out + ll_proxy_total + ll_truth
}

# ----------------------------
# Part 2b: Log Likelihood for Predicted Data
# ----------------------------
compute_predicted_ll <- function(params, df_x0, df_x1,
                                 outcome_formula, outcome_family,
                                 proxy_formulas, truth_formula) {
  # Outcome for predicted data
  response.var   <- all.vars(outcome_formula)[1]
  y_pred         <- model.frame(outcome_formula, df_x0)[[response.var]]
  X0_outcome     <- model.matrix(outcome_formula, df_x0)
  X1_outcome     <- model.matrix(outcome_formula, df_x1)
  
  if (outcome_family$family == "gaussian") {
    shift         <- 1
    outcome_llfun <- ll.gaussian
  } else if (outcome_family$family == "binomial" && outcome_family$link == "logit") {
    shift         <- 0
    outcome_llfun <- ll.logistic
  } else {
    stop("Unsupported outcome family", call. = FALSE)
  }
  
  n_out  <- ncol(X0_outcome)
  thetao <- params[1:(n_out + shift)]
  ll_y0  <- outcome_llfun(y_pred, thetao, X0_outcome)
  ll_y1  <- outcome_llfun(y_pred, thetao, X1_outcome)
  
  # Proxy for predicted data
  ll_w_total <- 0
  idx         <- n_out + shift + 1
  proxy_llfun <- ll.logistic
  for (pf in proxy_formulas) {
    X0p    <- model.matrix(pf, df_x0)
    X1p    <- model.matrix(pf, df_x1)
    var_p  <- all.vars(pf)[1]
    w_obs  <- df_x0[[var_p]]
    n_p    <- ncol(X0p)
    thetap <- params[idx:(idx + n_p - 1)]
    ll_w_total <- ll_w_total +
      proxy_llfun(w_obs, thetap, X0p) +
      proxy_llfun(w_obs, thetap, X1p)
    idx <- idx + n_p
  }
  
  # Truth for predicted data
  X_truth     <- model.matrix(truth_formula, df_x0)  # same dims
  var_t       <- all.vars(truth_formula)[1]
  n_t         <- ncol(X_truth)
  thetat      <- params[idx:(idx + n_t - 1)]
  truth_llfun <- ll.logistic
  ll_x0       <- truth_llfun(df_x0[[var_t]], thetat, X_truth)
  ll_x1       <- truth_llfun(df_x1[[var_t]], thetat, X_truth)
  
  # integrate out truth via log-sum-exp
  ll0 <- ll_y0 + ll_w_total / nrow(df_x0) + ll_x0
  ll1 <- ll_y1 + ll_w_total / nrow(df_x0) + ll_x1
  sum(matrixStats::colLogSumExps(rbind(ll0, ll1)))
}

# ----------------------------
# Part 3: Aggregation into .measrr_mle_nll
# ----------------------------
.measrr_mle_nll <- function(params, df,
                             outcome_formula, outcome_family = gaussian(),
                             proxy_formulas, truth_formula, truth_family = binomial(link='logit')) {
  sets   <- prepare_data_sets(df, outcome_formula, proxy_formulas, truth_formula)
  ll_obs <- compute_observed_ll(params, sets$observed,
                                outcome_formula, outcome_family,
                                proxy_formulas, truth_formula, truth_family)
  ll_prd <- compute_predicted_ll(params, sets$predicted_x0, sets$predicted_x1,
                                 outcome_formula, outcome_family,
                                 proxy_formulas, truth_formula)
  -(ll_obs + ll_prd)
}

# ----------------------------
# Top-level glm_fixit (wrapping the new likelihood)
# ----------------------------
glm_fixit <- function(formula, family = gaussian(), data, data2,
                      proxy_formula = NULL, proxy_family = binomial(link='logit'),
                      truth_formula = NULL, truth_family = binomial(link='logit'),
                      maxit = 1e6, method = 'L-BFGS-B') {
  # parse formulas
  parsed <- .conv_formula(formula)
  
  # wrap single proxy_formula into list
  if (is.null(proxy_formula)) {
    proxy_formula <- formula(paste0(parsed$proxy, "~ ."))
  }
  proxy_formulas <- list(proxy_formula)
  
  # default truth_formula
  if (is.null(truth_formula)) {
    truth_formula <- formula(paste0(parsed$truth, "~ 1"))
  }
  
  df <- vctrs::vec_rbind(data, data2)
  
  # run optimization
  fit <- optim(
    par    = rnorm(length(colnames(model.matrix(parsed$outcome_formula, df))) +
                    sum(sapply(proxy_formulas, function(f) ncol(model.matrix(f, df)))) +
                    length(colnames(model.matrix(truth_formula, df)))),
    fn     = .measrr_mle_nll,
    df     = df,
    outcome_formula = formula(parsed$outcome_formula),
    outcome_family  = family,
    proxy_formulas  = proxy_formulas,
    truth_formula   = truth_formula,
    truth_family    = truth_family,
    method = method,
    control = list(maxit = maxit),
    hessian = TRUE
  )
  
  # fit naive & feasible
  naive    <- glm(formula = formula(parsed$naive_formula), family = family, data = data)
  feasible <- glm(formula = formula(parsed$outcome_formula), family = family, data = data2)
  
  fit$naive <- naive
  fit$feasible <- feasible
  class(fit) <- c("glm_fixit", class(fit))
  fit
}
