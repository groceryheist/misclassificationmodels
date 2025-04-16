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

# ----------------------------
# Part 3: Aggregation into the Main Likelihood Function and Updated glm_fixit
# ----------------------------
.measerr_mle_nll <- function(params, df, outcome_formula, outcome_family = gaussian(),
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
  sum(-total_ll)
}

.measerr_mle_iv <- function(df, outcome_formula, outcome_family=gaussian(), proxy_formula, proxy_family=binomial(link='logit'), truth_formula, truth_family=binomial(link='logit'), maxit = 1e6, method = 'L-BFGS-B') {
    outcome.params <- colnames(model.matrix(outcome_formula,df))

    lower <- rep(-Inf, length(outcome.params))
    if (outcome_family$family == 'gaussian') {
        outcome.params <- c(outcome.params, "sigma_y")
        lower <- c(lower, 1/1e6)
    }
    params <- outcome.params
    proxy.params <- colnames(model.matrix(proxy_formula, df))
    params <- c(params, paste0('proxy_',proxy.params))
    lower <- c(lower, rep(-Inf, length(proxy.params)))
    truth.params <- colnames(model.matrix(truth_formula, df))
    params <- c(params, paste0('truth_', truth.params))
    lower <- c(lower, rep(-Inf, length(truth.params)))
    start <- rnorm(length(params))
    ##start <- rep(0.1, length(params))
    names(start) <- params
    fit <- optim(start, fn = .measerr_mle_nll, lower = lower, method = method, hessian = TRUE, control = list(maxit=maxit),
                 df = df, outcome_formula = outcome_formula, outcome_family = outcome_family, proxy_formula = proxy_formula,
                 proxy_family = proxy_family, truth_formula = truth_formula, truth_family = truth_family)
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
  
  # Combine primary and validation data
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
  
  ## # Choose the appropriate likelihood function based on the dependent variable type.
  ## mla_function <- if (isFALSE(parsed_formula$yproxy)) .measerr_mle_iv else .measerr_mle_dv        
  
  ## # Call our refactored likelihood function.
  ## res <- mla_function(df, outcome_formula = formula(parsed_formula$outcome_formula),
  ##                     outcome_family = family, proxy_formula = proxy_formula,
  ##                     truth_formula = truth_formula, truth_family = truth_family,
  ##                     maxit = maxit, method = method)
  
  # fit naive and feasible models for comparison
  naive <- glm(formula = formula(parsed_formula$naive_formula), family = family, data = data)
  feasible <- glm(formula = formula(parsed_formula$outcome_formula), family = family, data = data2)
  stopifnot(inherits(parsed_formula$naive_formula, "formula"))
  message("Naive formula: ", deparse(parsed_formula$naive_formula))
  message("Outcome formula: ", deparse(parsed_formula$outcome_formula))
  naive <- glm(formula = parsed_formula$naive_formula, family = family, data = data)
  feasible <- glm(formula = parsed_formula$outcome_formula, family = family, data = data2)

  res$naive <- naive
  res$feasible <- feasible
  res$formula <- formula
  res$family <- family
  res$outcome_formula <- formula(parsed_formula$outcome_formula)
  res$proxy_formula <- formula(parsed_formula$proxy_formula)
  res$truth_formula <- formula(parsed_formula$truth_formula)
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

