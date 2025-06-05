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
                                proxy_formulas,
                                truth_formulas,
                                truth_family = binomial(link='logit')) {
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

  ll_truth_total <- 0
  for (tf in truth_formulas) {
    # Truth component
    X_truth <- model.matrix(tf, df)
    var_t <- all.vars(tf)[1]
    x_obs <- df[[var_t]]
    n_t <- ncol(X_truth)
    thetat <- params[idx:(idx + n_t - 1)]
    truth_llfun <- ll.logistic # assume binomial(logit)
    ll_truth <- truth_llfun(x_obs, thetat, X_truth)
    ll_truth_total <- ll_truth_total + ll_truth
    idx <- idx + n_t
  }  
  sum(ll_out + ll_proxy_total + ll_truth_total)
}

# ----------------------------
# Part 2b: Log Likelihood for Predicted Data
# ----------------------------
compute_predicted_ll <- function(params, df,
                                 outcome_formula, outcome_family,
                                 proxy_formulas, truth_formulas) {
  # Outcome for predicted data

  # Make a grid of combinations of proxy variables
  
  response.var <- all.vars(outcome_formula)[1]
  
  y <- df[[response.var]]
    truth_vars <- sapply(truth_formulas, function(truth_formula) {
    f_terms <- terms(truth_formula)
    attr(f_terms, "variables")[[attr(f_terms, "response") + 1]]
  })

  grid.values <- list()
  for (tvar in truth_vars)
    grid.values[[tvar]] <- c(0, 1)

  integration_grid <- expand.grid(grid.values)
  
  if (outcome_family$family == "gaussian") {
    shift         <- 1
    outcome_llfun <- ll.gaussian
  } else if (outcome_family$family == "binomial" && outcome_family$link == "logit") {
    shift         <- 0
    outcome_llfun <- ll.logistic
  } else {
    stop("Unsupported outcome family", call. = FALSE)
  }

  # now for each possible combination of latent variables, we compute the likelihood
  ll_grid <- NULL
  for (grid_idx in 1:dim(integration_grid)[[1]]) {
    # create the data frame for this integration part

    Xval <- unlist(integration_grid[grid_idx, , drop = FALSE])
    for (i in 1:length(Xval)) {
      df[[names(Xval)[[i]]]] <- Xval[[i]]
    }

    X <- model.matrix(outcome_formula, df, na.action = na.pass)
    # compute the ll of the outcome
    n_out <- ncol(X)
    thetao <- params[1:(n_out + shift)]
    ll_y  <- outcome_llfun(y, thetao, X)
    # Proxy for predicted data
    idx         <- n_out + shift + 1
    proxy_llfun <- ll.logistic
    for (pf in proxy_formulas) {
      Xp <- model.matrix(pf, df, na.action=na.pass)
      var_p <- all.vars(pf)[1]
      w_obs <- df[[var_p]]
      n_p <- ncol(Xp)
      thetap <- params[idx:(idx + n_p - 1)]
      ll_w <- proxy_llfun(w_obs, thetap, Xp)
      idx <- idx + n_p
    }
    
    for (tf in truth_formulas) {
      # Truth for predicted data
      X_truth <- model.matrix(tf, df, na.action=na.pass) # same dims
      var_t <- all.vars(tf)[1]
      n_t <- ncol(X_truth)
      thetat <- params[idx:(idx + n_t - 1)]
      truth_llfun <- ll.logistic
      ll_x <- truth_llfun(df[[var_t]], thetat, X_truth)
      idx <- idx + n_t
    }
    if (is.null(ll_grid)) {
      ll_grid <- ll_y + ll_w + ll_x
    } else {
      ll_grid <- rbind(ll_grid, ll_y + ll_w + ll_x)
    }
  }

  # integrate out truth via log-sum-exp
  
  res = sum(matrixStats::colLogSumExps(ll_grid))
}

.conv_formulas <- function(formulas) {

  outcome_formula <- formulas[[1]]

  formula_string <- paste(deparse(outcome_formula), collapse = " ")
  formula_string <- gsub("\\s+", " ", formula_string)
  
  ## find the proxy variables:
  proxies <- stringr::str_match_all(formula_string, "([^~+]+?\\|\\|[^~+]+)")[[1]][, 2]
  proxies <- trimws(proxies)


  ## create the final outcome_formula by stripping off all the proxies

  outcome_formula <- gsub("\\|\\|[^~+]+", "", formula_string)
  naive_formula <- outcome_formula
  outcome_formula <- as.formula(gsub("\\s+", " ", outcome_formula))
  f_terms <- terms(outcome_formula)
  outcome_resp <- attr(f_terms, "variables")[[attr(f_terms, "response") + 1]]


  truth_vars <- c()
  proxy_vars <- c()

  for (proxy in proxies) {
    splt <- strsplit(proxy, "\\|\\|")[[1]]
    truth_var <- trimws(splt[[1]])
    proxy_var <- trimws(splt[[2]])
    truth_vars <- c(truth_vars, truth_var)
    proxy_vars <- c(proxy_vars, proxy_var)
    naive_formula <- gsub(truth_var, proxy_var, naive_formula)
  }

  truth_formulas <- list()
  proxy_formulas <- list()
  found_proxy_resps <- c()
  found_truth_resps <- c()
  yproxy = FALSE

  ## for each remaining formula in formulas, check if it is a proxy or truth formula.
  for (i in 2:length(formulas)) {
    f_form <- formulas[[i]]
    f_terms <- terms(f_form)
    depvar <- c(attr(f_terms, "variables")[[attr(f_terms, "response") + 1]])

    if (depvar == outcome_resp) {
      yproxy <- TRUE
    }

    if (depvar %in% truth_vars) {
      truth_formulas <- append(truth_formulas, f_form)
      found_truth_resps <- c(found_truth_resps, depvar)
    } else if (depvar %in% proxy_vars) {
      proxy_formulas <- append(proxy_formulas, f_form)
      found_proxy_resps <- c(found_proxy_resps, depvar)
    } else {
      stop(paste("Formula response", depvar, "does not match a proxy or truth variable"))
    }
  }

  ## assert that each proxy has a proxy formula
  stopifnot(length(proxy_formulas) == length(proxy_vars))
  for (proxy_var in proxy_vars) {
    stopifnot(proxy_var %in% found_proxy_resps)
  }
  for (truth_var in truth_vars) {
    if (!truth_var %in% found_truth_resps) {
      truth_formulas <- append(truth_formulas, as.formula(paste0(truth_var, " ~ 1")))
    }
  }
  res <- list(
    outcome_formula = outcome_formula,
    naive_formula = formula(naive_formula),
    proxy_formulas = proxy_formulas,
    truth_formulas = truth_formulas,
    yproxy = yproxy
  )
  res
}

# ----------------------------
# Part 3: Aggregation into .measrr_mle_nll
# ----------------------------a
measerr_mle_iv <- function(params, df,
                           outcome_formula, outcome_family = gaussian(),
                           proxy_formulas, truth_formulas) {
  
  ll_obs <- compute_observed_ll(params, df[df$observed==TRUE, ],
                                outcome_formula, outcome_family,
                                proxy_formulas, truth_formulas)

  ll_prd <- compute_predicted_ll(params, df[df$observed==FALSE, ],
                                 outcome_formula, outcome_family,
                                 proxy_formulas, truth_formulas)
  
  -(ll_obs + ll_prd)
}

# ----------------------------
# Top-level glm_fixit (wrapping the new likelihood)
# ----------------------------
# first argument is a list of strings of extended formula syntax.
# The first of which is the "scientific" model to be estimated.
# In this first formula, each variable that has a "proxy" should come paired with the proxy.
# For example, y ~ x || w means that we'll fit a model y ~ x, but use w as a proxy for x.
# For each proxy variable, a subsequent formula needs to have the proxy as an outcome.
# For each independent variables with proxies, we also need a formula for the true value.
# Such "truth" formulas can be omitted; a default of x ~ 1 will be used.
glm_fixit <- function(..., family = gaussian(), data, data2,
                      proxy_family = binomial(link='logit'),
                      truth_family = binomial(link='logit'),
                      maxit = 1e6, method = 'L-BFGS-B') {

  formulas <- list(...)

  f_list <- .conv_formulas(formulas)
  # Basic input validation
  stopifnot(inherits(f_list$outcome_formula, "formula"))
  stopifnot(inherits(family, "family"))
  stopifnot(is.data.frame(data), is.data.frame(data2))


  # Family/link compatibility checks
  if ((proxy_family$family != "binomial") || (proxy_family$link != "logit")) {
    stop("Unsupported `proxy_family`. The proxy family should be binomial(link='logit').", call. = FALSE)
  }
  if ((truth_family$family != "binomial") || (truth_family$link != "logit")) {
    stop("Unsupported `truth_family`. The truth family should be binomial(link='logit').", call. = FALSE)
  }
  # parse formulas
  # Convert formula early

  outcome_formula <- f_list$outcome_formula
  naive_formula <- f_list$naive_formula
  proxy_formulas <- f_list$proxy_formula
  truth_formulas <- f_list$truth_formula
  
  # wrap single proxy_formula into list
  if (! is.list(proxy_formulas))
    proxy_formulas <- list(proxy_formula)
  
  # Combine primary and validation data
  df <- vctrs::vec_rbind(data, data2)
  
  ## # Choose the appropriate likelihood function based on the dependent variable type.
  mla_function <- if (isFALSE(f_list$yproxy)) measerr_mle_iv else measerr_mle_dv

  n_proxy_params <- 0
  proxy_param_names <- c()
  for(f in proxy_formulas){
    f_terms <- terms(f)
    proxy_resp <- attr(f_terms, "variables")[[attr(f_terms, "response") + 1]]
    proxy_model_matrix <- model.matrix(f, df)
    new_proxy_param_names <- paste0(colnames(proxy_model_matrix), '_', proxy_resp, "_proxy")
    proxy_param_names <- c(proxy_param_names, new_proxy_param_names)
    n_proxy_params <- n_proxy_params + length(new_proxy_param_names)
  }

  n_truth_params <- 0
  truth_param_names <- c()
  for(f in truth_formulas){
    f_terms <- terms(f)
    truth_resp <- attr(f_terms, "variables")[[attr(f_terms, "response") + 1]]
    truth_model_matrix <- model.matrix(f, df)
    new_truth_param_names <- paste0(colnames(truth_model_matrix), '_', truth_resp, "_truth")
    truth_param_names <- c(truth_param_names, new_truth_param_names)
    n_truth_params <- n_truth_params + length(new_truth_param_names)
  }

  outcome_model_mat <- model.matrix(outcome_formula, df)
  outcome_param_names <- colnames(outcome_model_mat)
  if (family$family == "gaussian") {
    outcome_param_names <- c(outcome_param_names, "sigma")
    n_outcome_params <- length(outcome_param_names) 
    lower <- c(rep(-Inf, n_outcome_params - 1), 0.00000001, rep(-Inf, n_proxy_params + n_truth_params))
  } else {
    n_outcome_params <- length(outcome_param_names)
    lower <- rep(-Inf, n_outcome_params + n_proxy_params + n_truth_params)
  }

  params <- abs(rnorm(n_outcome_params +
                        n_proxy_params +
                        n_truth_params, sd = 0.01))
  names(params) <- c(outcome_param_names, proxy_param_names, truth_param_names)

  missing_idx <- rep(FALSE, nrow(df))
  for (tf in truth_formulas) {
    f_terms <- terms(tf)
    truth_resp <- attr(f_terms, "variables")[[attr(f_terms, "response") + 1]]
    missing_idx <- is.na(df[[truth_resp]]) | missing_idx
  }

  observed_idx <- missing_idx == FALSE
  df[["observed"]] <- observed_idx
  
  # run optimization
  res <- optim(
    par    = params,
    fn     = mla_function,
    df     = df,
    outcome_formula = outcome_formula,
    outcome_family  = family,
    proxy_formulas  = proxy_formulas,
    truth_formula   = truth_formulas,
    lower = lower,
    method = method,
    control = list(maxit = maxit),
    hessian = TRUE
  )

  ## # Call our refactored likelihood function.
  ## res <- mla_function(df, outcome_formula = formula(f_list$outcome_formula),
  ##                     outcome_family = family, proxy_formula = proxy_formula,
  ##                     truth_formula = truth_formula, truth_family = truth_family,
  ##                     maxit = maxit, method = method)
  
  # Fit naive and feasible models for comparison
  stopifnot(inherits(f_list$naive_formula, "formula"))
  message("Naive formula: ", deparse(f_list$naive_formula))
  message("Outcome formula: ", deparse(f_list$outcome_formula))
  naive <- glm(formula = naive_formula, family = family, data = data)
  feasible <- glm(formula = outcome_formula, family = family, data = data2)

  res$naive <- naive
  res$feasible <- feasible
  res$formula <- formula
  res$family <- family
  res$outcome_formula <- outcome_formula
  res$proxy_formulas <- proxy_formulas
  res$truth_formulas <- truth_formulas
  res$proxy_family <- proxy_family
  res$truth_family <- truth_family
  class(res) <- c("glm_fixit", class(res))

  res
}

#' @method print glm_fixit
#' @export
print.glm_fixit <- function(x, ...) {
  all_vars <- names(coef(x$feasible))
  cat("Corrected Estimator:\n")
  print(x$par[all_vars])
  cat("Feasible Estimator:\n")
  print(coef(x$feasible))
  cat("Naive Estimator:\n")
  print(coef(x$naive))
}

#' @method coef glm_fixit
#' @export
coef.glm_fixit <- function(object, ...) {
    args <- list(...)
    if ("which_model" %in% names(args)) {
        if (!args$which_model %in% c("corrected", "feasible", "naive")) {
            stop("Unknown `which` value. Accepted values are \"corrected\", \"feasible\", \"naive\".")
        } else {
            which_model <- args$which_model
        }
    } else {
        which_model <- "corrected"
    }
    if (which_model == "corrected") {
        return(object$par)
    }
    if (which_model == "feasible") {
        return(coef(object$feasible))
    }
    if (which_model == "naive") {
        return(coef(object$naive))
    }
}

#' @method confint glm_fixit
#' @export
confint.glm_fixit <- function(object, parm, level = 0.95, ...) {
    ## ask the base people why "parm" is used here ?confint
    args <- list(...)
    if ("which_model" %in% names(args)) {
        if (!args$which_model %in% c("corrected", "feasible", "naive")) {
            stop("Unknown `which` value. Accepted values are \"corrected\", \"feasible\", \"naive\".")
        } else {
            which_model <- args$which_model
        }
    } else {
        which_model <- "corrected"
    }
    if (which_model == "corrected") {
        upper_prob <- 1 - ((1 - level) / 2)
        lower_prob <- ((1 - level) / 2)
        fisher.info <- solve(object$hessian)
        coef <- object$par
        ci.upper <- coef + sqrt(diag(fisher.info)) * qnorm(upper_prob)
        ci.lower <- coef - sqrt(diag(fisher.info)) * qnorm(upper_prob)
        res <- matrix(c(ci.lower, ci.upper), ncol = 2)
        colnames(res) <- paste0(100 * c(lower_prob, upper_prob), " %")
        rownames(res) <- names(coef)
        if (missing(parm)) {
            parm <- names(coef)
        }
        return(res[parm, ])
    }
    if (which_model == "feasible") {
        return(confint(object$feasible))
    }
    if (which_model == "naive") {
        return(confint(object$naive))
    }
}

#' @method summary glm_fixit
#' @export
summary.glm_fixit <- function(object, ...) {
    corrected_table <- cbind(coef(object), confint(object, level = .95, which_model = "corrected"))
    colnames(corrected_table)[1] <- "Estimate"
    cat("Coefficients (Corrected Estimator): \n")
    print(corrected_table)
    cat("\n\n")
    cat("Coefficients (Naive Estimator): \n")
    naive_table <- cbind(coef(object, which_model = "naive"), suppressMessages(confint(object, level = .95, which_model = "naive")))
    colnames(naive_table)[1] <- "Estimate"
    print(naive_table)
    cat("\n\n")
    cat("Coefficients (Feasible Estimator): \n")
    feasible_table <- cbind(coef(object, which_model = "feasible"), suppressMessages(confint(object, level = .95, which_model = "feasible")))
    colnames(feasible_table)[1] <- "Estimate"
    print(feasible_table)    
}

