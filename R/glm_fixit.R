<<<<<<< HEAD
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
||||||| 0fe1229
.measrr_mle_nll <- function(params, df, outcome_formula, outcome_family=gaussian(), proxy_formula, proxy_family=binomial(link='logit'), truth_formula, truth_family=binomial(link='logit')) {
    df.obs <- model.frame(outcome_formula, df)
    
    # we'll have multiple proxy variables. So this part needs to go in a loop / function
    proxy.variable <- all.vars(proxy_formula)[1]
    proxy.model.matrix <- model.matrix(proxy_formula, df)

    response.var <- all.vars(outcome_formula)[1]
    y.obs <- with(df.obs,eval(parse(text=response.var)))
    
    if (outcome_family$family == "gaussian") {
        ## gaussian always has `sigma_y` added to `outcome.params`; shift the index by one
        index_shift <- 1
        outcome.llfun <- ll.gaussian
    }
    if ((outcome_family$family == "binomial") && (outcome_family$link == "logit")) {
        index_shift <- 0
        outcome.llfun <- ll.logistic
    }
    # keeps track of which parameters we've read so far. 
    param.idx <- 1
    outcome.model.matrix <- model.matrix(outcome_formula, df)

    ## likelihood for the fully observed data
    n.outcome.model.covars <- dim(outcome.model.matrix)[2]
    outcome.params <- params[param.idx:(n.outcome.model.covars + index_shift)]
    # update param.idx since we've used the parameters in the outcome model 
    param.idx <- param.idx + n.outcome.model.covars + index_shift

    # think about whether we needs.
    if ((proxy_family$family=="binomial") && (proxy_family$link=='logit')) {
        proxy.llfun <- ll.logistic
    }
    if ((truth_family$family=="binomial") && (truth_family$link=='logit')) {
        truth.llfun <- ll.logistic
    }   
    # gets the likelihood for the outcome model
    ll.y.obs <- outcome.llfun(y.obs, outcome.params, outcome.model.matrix)
    
    df.obs <- model.frame(proxy_formula,df)
    n.proxy.model.covars <- dim(proxy.model.matrix)[2]

    # gets the parameters for a proxy model. This we'll need to do in the loop. 
    proxy.params <- params[param.idx:(n.proxy.model.covars+param.idx - 1)]
    param.idx <- param.idx + n.proxy.model.covars

    # this gets the dataset for the observed data in a proxy model. (goes in loop)
    proxy.obs <- with(df.obs, eval(parse(text=proxy.variable)))

    # this get the likelihood for the observed data in a proxy model.
    ll.w.obs <- proxy.llfun(proxy.obs, proxy.params, proxy.model.matrix)

    df.obs <- model.frame(truth_formula, df)
    truth.variable <- all.vars(truth_formula)[1]
    truth.obs <- with(df.obs, eval(parse(text=truth.variable)))
    truth.model.matrix <- model.matrix(truth_formula,df)
    n.truth.model.covars <- dim(truth.model.matrix)[2]
    
    truth.params <- params[param.idx:(n.truth.model.covars + param.idx - 1)]
    ll.x.obs <- truth.llfun(truth.obs, truth.params, truth.model.matrix)
    ## add the three likelihoods
    ll.obs <- sum(ll.y.obs + ll.w.obs + ll.x.obs)

    ## likelihood for the predicted data
    ## integrate out the "truth" variable. 
    df.unobs <- df[is.na(df[[truth.variable]]),]
    df.unobs.x1 <- df.unobs
    df.unobs.x1[,truth.variable] <- 1
    df.unobs.x0 <- df.unobs
    df.unobs.x0[,truth.variable] <- 0
    outcome.unobs <- with(df.unobs, eval(parse(text=response.var)))
        
    outcome.model.matrix.x0 <- model.matrix(outcome_formula, df.unobs.x0)
    outcome.model.matrix.x1 <- model.matrix(outcome_formula, df.unobs.x1)

    ll.y.x0 <- outcome.llfun(outcome.unobs, outcome.params, outcome.model.matrix.x0)
    ll.y.x1 <- outcome.llfun(outcome.unobs, outcome.params, outcome.model.matrix.x1)

    proxy.model.matrix.x0 <- model.matrix(proxy_formula, df.unobs.x0)
    proxy.model.matrix.x1 <- model.matrix(proxy_formula, df.unobs.x1)
    proxy.unobs <- df.unobs[[proxy.variable]]
    ll.w.x0 <- proxy.llfun(proxy.unobs, proxy.params, proxy.model.matrix.x0)
    ll.w.x1 <- proxy.llfun(proxy.unobs, proxy.params, proxy.model.matrix.x1)

    truth.model.matrix <- model.matrix(truth_formula, df.unobs.x0)
                                        # likelihood of truth
    ll.x.x1 <- truth.llfun(df.unobs.x1[[truth.variable]], truth.params, truth.model.matrix)
   
    ll.x.x0 <- truth.llfun(df.unobs.x0[[truth.variable]], truth.params, truth.model.matrix)

    ll.x0 <- ll.y.x0 + ll.w.x0 + ll.x.x0
    ll.x1 <- ll.y.x1 + ll.w.x1 + ll.x.x1
    ll.unobs <- sum(matrixStats::colLogSumExps(rbind(ll.x0, ll.x1)))
    return(-(ll.unobs + ll.obs))
=======
.measrr_mle_nll <- function(
  params,
  df,
  outcome_formula,
  outcome_family = gaussian(),
  proxy_formula,
  proxy_family = binomial(link = 'logit'),
  truth_formula,
  truth_family = binomial(link = 'logit')
) {
  # ---- Validate input formulas ----
  stopifnot(inherits(outcome_formula, "formula"))
  stopifnot(inherits(proxy_formula, "formula"))
  stopifnot(inherits(truth_formula, "formula"))

  # ---- Step 1: Prepare data for outcome model ----
  df.obs <- model.frame(outcome_formula, df)  # dataframe with complete cases for outcome
  response.var <- all.vars(outcome_formula)[1]  # name of outcome variable
  y.obs <- with(df.obs, eval(parse(text = response.var)))  # observed response vector
  outcome.model.matrix <- model.matrix(outcome_formula, df)  # design matrix for outcome model

  # Determine which likelihood function to use for the outcome model
  if (outcome_family$family == "gaussian") {
    index_shift <- 1  # include extra parameter for sigma_y
    outcome.llfun <- ll.gaussian
  }
  if ((outcome_family$family == "binomial") && (outcome_family$link == "logit")) {
    index_shift <- 0
    outcome.llfun <- ll.logistic
  }

  # ---- Step 2: Extract outcome model parameters ----
  param.idx <- 1
  n.outcome.model.covars <- ncol(outcome.model.matrix)
  outcome.params <- params[param.idx:(n.outcome.model.covars + index_shift)]
  param.idx <- param.idx + n.outcome.model.covars + index_shift

  # ---- Step 3: Prepare data for proxy model ----
  proxy.variable <- all.vars(proxy_formula)[1]  # name of proxy variable
  df.obs <- model.frame(proxy_formula, df)  # subset of df for proxy model
  proxy.model.matrix <- model.matrix(proxy_formula, df)  # design matrix
  n.proxy.model.covars <- ncol(proxy.model.matrix)

  if ((proxy_family$family == "binomial") && (proxy_family$link == "logit")) {
    proxy.llfun <- ll.logistic
  }

  # Extract proxy model parameters
  proxy.params <- params[param.idx:(param.idx + n.proxy.model.covars - 1)]
  param.idx <- param.idx + n.proxy.model.covars

  # Evaluate proxy outcome
  proxy.obs <- with(df.obs, eval(parse(text = proxy.variable)))
  ll.w.obs <- proxy.llfun(proxy.obs, proxy.params, proxy.model.matrix)  # proxy likelihood

  # ---- Step 4: Prepare data for truth model ----
  df.obs <- model.frame(truth_formula, df)
  truth.variable <- all.vars(truth_formula)[1]
  truth.obs <- with(df.obs, eval(parse(text = truth.variable)))
  truth.model.matrix <- model.matrix(truth_formula, df)
  n.truth.model.covars <- ncol(truth.model.matrix)

  if ((truth_family$family == "binomial") && (truth_family$link == "logit")) {
    truth.llfun <- ll.logistic
  }

  # Extract truth model parameters
  truth.params <- params[param.idx:(param.idx + n.truth.model.covars - 1)]
  ll.x.obs <- truth.llfun(truth.obs, truth.params, truth.model.matrix)

  # ---- Step 5: Compute observed likelihood ----
  ll.y.obs <- outcome.llfun(y.obs, outcome.params, outcome.model.matrix)
  ll.obs <- sum(ll.y.obs + ll.w.obs + ll.x.obs)

  # ---- Step 6: Handle unobserved data (missing truth variable) ----
  df.unobs <- df[is.na(df[[truth.variable]]), ]
  df.unobs.x1 <- df.unobs
  df.unobs.x0 <- df.unobs
  df.unobs.x1[[truth.variable]] <- 1
  df.unobs.x0[[truth.variable]] <- 0

  # Extract unobserved outcome values
  outcome.unobs <- with(df.unobs, eval(parse(text = response.var)))

  # Compute outcome likelihood under both x=0 and x=1
  outcome.model.matrix.x0 <- model.matrix(outcome_formula, df.unobs.x0)
  outcome.model.matrix.x1 <- model.matrix(outcome_formula, df.unobs.x1)
  ll.y.x0 <- outcome.llfun(outcome.unobs, outcome.params, outcome.model.matrix.x0)
  ll.y.x1 <- outcome.llfun(outcome.unobs, outcome.params, outcome.model.matrix.x1)

  # Compute proxy likelihood under both x=0 and x=1
  proxy.model.matrix.x0 <- model.matrix(proxy_formula, df.unobs.x0)
  proxy.model.matrix.x1 <- model.matrix(proxy_formula, df.unobs.x1)
  proxy.unobs <- df.unobs[[proxy.variable]]
  ll.w.x0 <- proxy.llfun(proxy.unobs, proxy.params, proxy.model.matrix.x0)
  ll.w.x1 <- proxy.llfun(proxy.unobs, proxy.params, proxy.model.matrix.x1)

  # Compute truth likelihood under both x=0 and x=1
  truth.model.matrix <- model.matrix(truth_formula, df.unobs.x0)  # same model matrix
  ll.x.x0 <- truth.llfun(df.unobs.x0[[truth.variable]], truth.params, truth.model.matrix)
  ll.x.x1 <- truth.llfun(df.unobs.x1[[truth.variable]], truth.params, truth.model.matrix)

  # Combine likelihoods under each scenario
  ll.x0 <- ll.y.x0 + ll.w.x0 + ll.x.x0
  ll.x1 <- ll.y.x1 + ll.w.x1 + ll.x.x1

  # ---- Step 7: Marginalize over missing truth variable (log-sum-exp trick) ----
  ll.unobs <- sum(matrixStats::colLogSumExps(rbind(ll.x0, ll.x1)))

  # ---- Step 8: Return negative log-likelihood ----
  return(-(ll.unobs + ll.obs))
>>>>>>> 8463c13add1c52ddc315f40568fac8239f72139a
}

<<<<<<< HEAD
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
||||||| 0fe1229
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
    fit <- optim(start, fn = .measrr_mle_nll, lower = lower, method = method, hessian = TRUE, control = list(maxit=maxit),
                 df = df, outcome_formula = outcome_formula, outcome_family = outcome_family, proxy_formula = proxy_formula,
                 proxy_family = proxy_family, truth_formula = truth_formula, truth_family = truth_family)
    return(fit)
=======

.measerr_mle_iv <- function(df, outcome_formula, outcome_family = gaussian(), 
                                  proxy_families, truth_families, 
                                  maxit = 1e6, method = 'L-BFGS-B') {
  outcome.params <- colnames(model.matrix(outcome_formula, df))
  lower <- rep(-Inf, length(outcome.params))
  if (outcome_family$family == 'gaussian') {
    outcome.params <- c(outcome.params, "sigma_y")
    lower <- c(lower, 1/1e6)
  }
  params <- outcome.params
  
  proxy_formulas <- conv_formula_updated(outcome_formula)
  truth_formulas <- list()
  
  for (i in seq_along(proxy_formulas)) {
    proxy.params <- colnames(model.matrix(proxy_formulas[[i]], df))
    params <- c(params, paste0('proxy', i, '_', proxy.params))
    lower <- c(lower, rep(-Inf, length(proxy.params)))
  }
  
  for (i in seq_along(truth_families)) {
    truth.params <- colnames(model.matrix(truth_formulas[[i]], df))
    params <- c(params, paste0('truth', i, '_', truth.params))
    lower <- c(lower, rep(-Inf, length(truth.params)))
  }
  
  start <- rnorm(length(params))
  names(start) <- params
  
  fit <- optim(start, fn = .measrr_mle_nll, lower = lower, method = method, hessian = TRUE, 
               control = list(maxit = maxit), df = df, outcome_formula = outcome_formula, 
               outcome_family = outcome_family, proxy_formulas = proxy_formulas, 
               proxy_families = proxy_families, truth_families = truth_families)
  
  return(fit)
>>>>>>> 8463c13add1c52ddc315f40568fac8239f72139a
}

<<<<<<< HEAD

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
  result <- sum(-total_ll)
  print(result)
}

||||||| 0fe1229
##hacky

=======
>>>>>>> 8463c13add1c52ddc315f40568fac8239f72139a
.conv_formula <- function(formula) {
  # split the formula
    tokenized_formula <- strsplit(as.character(formula), " ")
    stopifnot("||" %in% unlist(tokenized_formula))
  formula_parts <- strsplit(deparse(formula), "\\|\\|")[[1]]
  formula_parts <- lapply(formula_parts, function(f) as.formula(trimws(f)))
  names(formula_parts) <- sapply(formula_parts, function(f) as.character(f[[2]]))
  return(formula_parts)
}


.measerr_mle_iv <- function(df, outcome_formula, outcome_family=gaussian(), proxy_formula, proxy_family=binomial(link='logit'), truth_formula, truth_family=binomial(link='logit'), maxit = 1e6, method = 'L-BFGS-B') {
    outcome.params <- colnames(model.matrix(outcome_formula,df))

<<<<<<< HEAD
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
||||||| 0fe1229
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
glm_fixit <- function(formula, family = gaussian(), data, data2, proxy_formula = NULL, proxy_family=binomial(link='logit'), truth_formula = NULL, truth_family=binomial(link='logit'), maxit = 1e6, method = 'L-BFGS-B') {
    if ((proxy_family$family != "binomial") && (proxy_family$link != 'logit')) {
        stop("Unsupported `proxy_family`. The proxy family should be binomial(link='logit').", call. = FALSE)
    }
    if ((truth_family$family != "binomial") && (truth_family$link != 'logit')) {
        stop("Unsupported `truth_family`. The truth family should be binomial(link='logit').", call. = FALSE)
    }
    parsed_formula <- .conv_formula(formula)
    if (isTRUE(parsed_formula$yproxy) && family$family != "binomial" && family$link != "logit") {
        stop("Only logistic regression is supported for dependent variable with misclassification.", call. = FALSE)
    }
    df <- vctrs::vec_rbind(data, data2)
    if (is.null(proxy_formula)) {
        proxy_formula <- formula(paste0(parsed_formula$proxy, "~."))
    }
    if (is.null(truth_formula)) {
        truth_formula <- formula(paste0(parsed_formula$truth, "~ 1"))
    }
    if(isFALSE(parsed_formula$yproxy)) {
        mla_function <- .measerr_mle_iv
    } else {
        mla_function <- .measerr_mle_dv        
    }
    res <- mla_function(df, outcome_formula = formula(parsed_formula$outcome_formula), outcome_family = family, proxy_formula = proxy_formula, truth_formula = truth_formula, truth_family = truth_family, maxit = maxit, method = method)
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
=======
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

  # Family/link compatibility checks
  if ((proxy_family$family != "binomial") && (proxy_family$link != 'logit')) {
    stop("Unsupported `proxy_family`. The proxy family should be binomial(link='logit').", call. = FALSE)
  }
  if ((truth_family$family != "binomial") && (truth_family$link != 'logit')) {
    stop("Unsupported `truth_family`. The truth family should be binomial(link='logit').", call. = FALSE)
  }

  parsed_formula <- .conv_formula(formula)
  if (isTRUE(parsed_formula$yproxy) && family$family != "binomial" && family$link != "logit") {
    stop("Only logistic regression is supported for dependent variable with misclassification.", call. = FALSE)
  }

  df <- vctrs::vec_rbind(data, data2)

  mla_function <- if (isFALSE(parsed_formula$yproxy)) .measerr_mle_iv else .measerr_mle_dv

  res <- mla_function(df, outcome_formula = formula(parsed_formula$outcome_formula), 
                      outcome_family = family, 
                      truth_family = truth_family, 
                      maxit = maxit, 
                      method = method)

  naive <- glm(formula = formula(parsed_formula$naive_formula), family = family, data = data)
  feasible <- glm(formula = formula(parsed_formula$outcome_formula), family = family, data = data2)

  res$naive <- naive
  res$feasible <- feasible
  res$formula <- formula
  res$family <- family
  res$proxy_family <- proxy_family
  res$truth_family <- truth_family
  class(res) <- c("glm_fixit", class(res))

  return(res)
>>>>>>> 8463c13add1c52ddc315f40568fac8239f72139a
}

<<<<<<< HEAD
glm_fixit <- function(formula, family = gaussian(), data, data2,
                      proxy_formula = NULL, proxy_family = binomial(link = 'logit'),
                      truth_formula = NULL, truth_family = binomial(link = 'logit'),
                      maxit = 1e6, method = 'L-BFGS-B') {
||||||| 0fe1229
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
=======
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
>>>>>>> 8463c13add1c52ddc315f40568fac8239f72139a

  print("in glm_fixit")
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
  mla_function <- if (isFALSE(parsed_formula$yproxy)) .measerr_mle_iv else .measerr_mle_dv        

  print("calling llik")
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
