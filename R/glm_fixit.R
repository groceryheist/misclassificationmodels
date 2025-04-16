.measrr_mle_nll <- function(params,
                            df,
                            outcome_formula,
                            outcome_family,
                            proxy_formulas,
                            truth_formulas,
                            proxy_families,
                            truth_families) {
  # First, calculate the outcome model log-likelihood
  response.var <- all.vars(outcome_formula)[1]
  y.obs <- model.response(model.frame(outcome_formula, df))
  outcome_mm <- model.matrix(outcome_formula, df)
  outcome.param.names <- colnames(outcome_mm)
  stopifnot(all(outcome.param.names %in% names(params)))
  outcome.params <- params[outcome.param.names]
  
  if (outcome_family$family == "gaussian") {
    if (!"sigma_y" %in% names(params)) stop("Missing sigma_y in parameter vector.")
    sigma_y <- params["sigma_y"]
  }
  
  y.ll <- switch(outcome_family$family,
                 "binomial" = {
                   y.linpred <- model.matrix(outcome_formula, df) %*% outcome.params
                   -sum(y.obs * y.linpred - log(1 + exp(y.linpred)))
                 },
                 "gaussian" = {
                   y.linpred <- outcome_mm %*% outcome.params
                   -sum(dnorm(y.obs, mean = y.linpred, sd = sigma_y, log = TRUE))
                 },
                 stop("Only binomial and gaussian families are supported.")
  )
  
  # Initialize total proxy/truth log-likelihood
  total_proxy_ll <- 0
  proxy_param_start <- n.outcome.model.covars + 1
  
  for (i in seq_along(proxy_formulas)) {
    proxy_formula <- proxy_formulas[[i]]
    truth_formula <- truth_formulas[[i]]
    proxy_family <- proxy_families[[i]]
    truth_family <- truth_families[[i]]
    
    # Determine number of parameters in proxy and truth models
    n_proxy_covars <- length(attr(terms(proxy_formula), "term.labels")) + 1
    n_truth_covars <- length(attr(terms(truth_formula), "term.labels")) + 1
    
    # Slice the relevant params
    proxy_params <- params[proxy_param_start:(proxy_param_start + n_proxy_covars - 1)]
    truth_params <- params[(proxy_param_start + n_proxy_covars):(proxy_param_start + n_proxy_covars + n_truth_covars - 1)]
    
    proxy_param_start <- proxy_param_start + n_proxy_covars + n_truth_covars
    
    # Get model frames and matrices
    proxy_mat <- model.matrix(proxy_formula, df)
    truth_mat <- model.matrix(truth_formula, df)
    
    y.proxy <- model.response(model.frame(proxy_formula, df))
    y.truth <- model.response(model.frame(truth_formula, df))
    
    proxy_linpred <- proxy_mat %*% proxy_params
    truth_linpred <- truth_mat %*% truth_params
    
    proxy_p <- switch(proxy_family$family,
                      "binomial" = plogis(proxy_linpred),
                      stop("Only binomial proxy families supported.")
    )
    truth_p <- switch(truth_family$family,
                      "binomial" = plogis(truth_linpred),
                      stop("Only binomial truth families supported.")
    )
    
    ll_proxy <- switch(proxy_family$family,
                       "binomial" = sum(dbinom(y.proxy, size = 1, prob = proxy_p, log = TRUE))
    )
    ll_truth <- switch(truth_family$family,
                       "binomial" = sum(dbinom(y.truth, size = 1, prob = truth_p, log = TRUE))
    )
    
    total_proxy_ll <- total_proxy_ll + ll_proxy + ll_truth
  }
  
  total_nll <- y.ll - total_proxy_ll  # Minimize negative log-likelihood
  return(total_nll)
}

.measerr_mle_iv <- function(df, outcome_formula, outcome_family = gaussian(),
                            truth_formulas, proxy_families, truth_families, 
                            maxit = 1e6, method = 'L-BFGS-B') {
  # Outcome model terms
  outcome_terms <- colnames(model.matrix(outcome_formula, df))
  lower <- rep(-Inf, length(outcome_terms))
  
  if (outcome_family$family == 'gaussian') {
    outcome_terms <- c(outcome_terms, "sigma_y")
    lower <- c(lower, 1 / 1e6)
  }
  
  # Store all parameter names
  all_param_names <- outcome_terms
  proxy_formulas <- .conv_formula(outcome_formula)
  truth_formulas <- .conv_formula(truth_formulas)
  
  # Storage for initial values
  start_values <- numeric()
  print("=== Start values ===")
  print(start_values)
  
  # Append starting values for outcome model
  outcome_fit <- glm(outcome_formula, family = outcome_family, data = df)
  start_values <- c(start_values, coef(outcome_fit))
  if (outcome_family$family == 'gaussian') {
    sigma_y <- summary(outcome_fit)$sigma
    start_values <- c(start_values, sigma_y)
  }
  
  # Go through each proxy-truth model
  for (i in seq_along(truth_formulas)) {
    truth_formula_i <- truth_formulas[[i]]
    proxy_formula_i <- proxy_formulas[[i]]
    truth_family_i <- truth_families[[i]]
    proxy_family_i <- proxy_families[[i]]
    
    # Fit truth model to get initial parameters
    truth_fit <- glm(truth_formula_i, family = truth_family_i, data = df)
    theta_names <- paste0("theta", i, ".", names(coef(truth_fit)))
    names_coef <- setNames(coef(truth_fit), theta_names)
    all_param_names <- c(all_param_names, theta_names)
    start_values <- c(start_values, names_coef)
    
    # Fit proxy model to get initial parameters
    proxy_fit <- glm(proxy_formula_i, family = proxy_family_i, data = df)
    gamma_names <- paste0("gamma", i, ".", names(coef(proxy_fit)))
    names_coef <- setNames(coef(proxy_fit), gamma_names)
    all_param_names <- c(all_param_names, gamma_names)
    start_values <- c(start_values, names_coef)
  }
  
  names(start_values) <- all_param_names
  
  # Final optimization
  fit <- optim(
    par = start_values,
    fn = .measrr_mle_nll,
    method = method,
    lower = lower,
    hessian = TRUE,
    control = list(maxit = maxit),
    df = df,
    outcome_formula = outcome_formula,
    outcome_family = outcome_family,
    proxy_formulas = proxy_formulas,
    proxy_families = proxy_families,
    truth_formulas = truth_formulas,
    truth_families = truth_families
  )
  
  return(fit)
}

.conv_formula <- function(formula) {
  formula_string <- paste(deparse(formula), collapse = " ")
  formula_string <- gsub("\\s+", " ", formula_string)
  
  formula_parts <- strsplit(formula_string, "\\|\\|")[[1]]
  formula_parts <- trimws(formula_parts)
  
  # Pad to length 3: outcome | proxy | naive
  while (length(formula_parts) < 3) {
    formula_parts <- c(formula_parts, "~ 1")
  }
  
  fix_formula_part <- function(part) {
    if (!grepl("~", part)) part <- paste("~", part)
    as.formula(part)
  }
  
  formulas <- lapply(formula_parts, fix_formula_part)
  
  outcome_formula <- formulas[[1]]
  proxy_formula   <- formulas[[2]]
  naive_formula   <- formulas[[3]]
  
  # Grab outcome variable from outcome_formula
  lhs <- all.vars(update(outcome_formula, . ~ 0))[1]
  
  # Fix naive_formula if it looks like y ~ NA or y ~ 
  rhs_naive <- tryCatch(as.character(naive_formula)[3], error = function(e) "1")
  if (is.na(rhs_naive) || rhs_naive == "") {
    rhs_naive <- "1"
  }
  naive_formula <- as.formula(paste(lhs, "~", rhs_naive))
  
  named_formulas <- list(
    outcome_formula = outcome_formula,
    proxy_formula   = proxy_formula,
    naive_formula   = naive_formula,
    truth_formula   = proxy_formula,
    yproxy          = !identical(deparse(proxy_formula), "~ 1")
  )
  
  stopifnot(all(sapply(named_formulas[c("outcome_formula", "proxy_formula", "naive_formula")], inherits, "formula")))
  
  return(named_formulas)
}




## glm(formula, family = gaussian, data, weights, subset,
##          na.action, start = NULL, etastart, mustart, offset,
##          control = list(...), model = TRUE, method = "glm.fit",
##          x = FALSE, y = TRUE, singular.ok = TRUE, contrasts = NULL, ...)


#' Misclassification correction of Generalized Linear Model with validation data
#'
#' This function provides the MLE-based misclassification correction method proposed by Carroll.
#' @param formula an object of class "formula". Please refer to [formula] for details. This package extends the formula syntax with the "||" symbol to indicate ground truth and proxy variables. For example, "x||w" indicates "w" is a proxy variable of the ground truth variable "x".  
#' @param family a description of the error distribution and link function to be used in the model. Currently, this function supports [gaussian()] and [binomial()].
#' @param data a data frame with the primary data
#' @param data2 a data frame with the validation data
#' @param proxy_formula an object of class "formula" to describe the data generating process of the proxy variable. Default to all columns in `data2`, i.e. "w ~ ."
#' @param proxy_family a description of the error distribution and link function to be used to model the proxy variable. Currently, this function supports [binomial()].
#' @param truth_formula an object of class "formula" to describe the data generating process of the ground truth variable. Default to an intercept only model (we don't know the data generating process), i.e. "x ~ 1"
#' @param truth_family a description of the error distribution and link function to be used to model the ground truth variable. Currently, this function supports [binomial()].
#' @param maxit variable get passed to [optim()]
#' @param method variable get passed to [optim()]
#' @return This function returns an object class "glm_fixit"
#' @seealso [research_data]
#' @examples
#' \donttest{
#' ## Confusion matrix
#' table(val_data$w, val_data$x)
#' ## default
#' glm_fixit(y ~ x || w + z, data = research_data, data2 = val_data)
#' ## proxy variable in the outcome
#' glm_fixit(y || w ~ x + z, data = research_data2,
#' data2 = val_data2, family = binomial("logit"))
#' }
#' @importFrom stats binomial coef confint dnorm gaussian glm model.frame model.matrix optim plogis qnorm rnorm
#' @export
glm_fixit <- function(formula, family = gaussian(), data, data2, 
                      proxy_family = binomial(link = 'logit'), 
                      truth_family = binomial(link = 'logit'), 
                      maxit = 1e6, method = 'L-BFGS-B') {
  
  # Basic input validation
  stopifnot(inherits(formula, "formula"))
  stopifnot(inherits(family, "family"))
  stopifnot(is.data.frame(data), is.data.frame(data2))
  
  # Convert formula early
  f_list <- .conv_formula(formula)
  outcome_formula <- f_list$outcome_formula
  naive_formula <- f_list$naive_formula
  proxy_formula <- f_list$proxy_formula

  # Family/link compatibility checks
  if ((proxy_family$family != "binomial") || (proxy_family$link != 'logit')) {
    stop("Unsupported `proxy_family`. The proxy family should be binomial(link='logit').", call. = FALSE)
  }
  if ((truth_family$family != "binomial") || (truth_family$link != 'logit')) {
    stop("Unsupported `truth_family`. The truth family should be binomial(link='logit').", call. = FALSE)
  }

  parsed_formula <- .conv_formula(formula)
  if (isTRUE(parsed_formula$yproxy) &&
      !(family$family == "binomial" && family$link == "logit") &&
      !(family$family == "gaussian")) {
    stop("Only logistic or Gaussian regression is supported for the outcome model with misclassification.", call. = FALSE)
  }

  df <- vctrs::vec_rbind(data, data2)

  if (isFALSE(parsed_formula$yproxy)) {
    res <- .measerr_mle_iv(
      df = df,
      outcome_formula = formula(parsed_formula$outcome_formula),
      outcome_family = family,
      truth_family = truth_family,
      maxit = maxit,
      method = method
    )
  } else {
    res <- .measerr_mle_dv(
      df = df,
      outcome_formula = formula(parsed_formula$outcome_formula),
      proxy_formula = formula(parsed_formula$proxy_formula),
      truth_formula = formula(parsed_formula$truth_formula),
      outcome_family = family,
      proxy_family = proxy_family,
      truth_family = truth_family,
      maxit = maxit,
      method = method
    )
  }
  
  stopifnot(inherits(parsed_formula$naive_formula, "formula"))
  message("Naive formula: ", deparse(parsed_formula$naive_formula))
  message("Outcome formula: ", deparse(parsed_formula$outcome_formula))
  naive <- glm(formula = parsed_formula$naive_formula, family = family, data = data)
  feasible <- glm(formula = parsed_formula$outcome_formula, family = family, data = data2)

  res$naive <- naive
  res$feasible <- feasible
  res$formula <- formula
  res$family <- family
  res$proxy_family <- proxy_family
  res$truth_family <- truth_family
  class(res) <- c("glm_fixit", class(res))

  return(res)
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

