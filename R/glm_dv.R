ll.logistic <- function(outcome, model.params, model.matrix) {
  ll <- numeric(length(outcome))
  
  if (any(outcome == 1)) {
    ll[outcome == 1] <- plogis(
      as.vector(model.matrix[outcome == 1, , drop = FALSE] %*% model.params),
      log.p = TRUE
    )
  }
  if (any(outcome == 0)) {
    ll[outcome == 0] <- plogis(
      as.vector(model.matrix[outcome == 0, , drop = FALSE] %*% model.params),
      log.p = TRUE,
      lower.tail = FALSE
    )
  }
  
  return(ll)
}

ll.gaussian_factory <- function(sigma) {
  function(outcome, model.params, model.matrix) {
    stopifnot(ncol(model.matrix) == length(model.params))
    mu <- as.vector(model.matrix %*% model.params)
    dnorm(outcome, mean = mu, sd = sigma, log = TRUE)
  }
}

.nll_dv <- function(params, df, outcome_formula, outcome_family = gaussian(), proxy_formula,
                    proxy_family = binomial(link = 'logit'), truth_formula,
                    truth_family = binomial(link = 'logit'), K = 20) {
  
  df.obs <- model.frame(outcome_formula, df)
  outcome.model.matrix <- model.matrix(outcome_formula, df.obs)
  response.var <- all.vars(outcome_formula)[1]
  y.obs <- df.obs[[response.var]]
  
  proxy.model.matrix <- model.matrix(proxy_formula, model.frame(proxy_formula, df))
  proxy.variable <- all.vars(proxy_formula)[1]
  proxy.obs <- model.frame(proxy_formula, df)[[proxy.variable]]
  
  # Construct expected parameter names
  outcome.linear.names <- paste0("outcome_", colnames(outcome.model.matrix))
  proxy.names <- paste0("proxy_", colnames(proxy.model.matrix))
  
  # Check presence BEFORE slicing
  if (outcome_family$family == "gaussian") {
    outcome.param.names <- c(outcome.linear.names, "sigma_y")
  } else {
    outcome.param.names <- outcome.linear.names
  }
  
  missing_outcome <- setdiff(outcome.param.names, names(params))
  missing_proxy <- setdiff(proxy.names, names(params))
  
  if (length(missing_outcome) > 0) {
    stop("Missing outcome parameters: ", paste(missing_outcome, collapse = ", "))
  }
  if (length(missing_proxy) > 0) {
    stop("Missing proxy parameters: ", paste(missing_proxy, collapse = ", "))
  }
  
  # Now safely extract
  outcome.params <- params[outcome.linear.names]
  if (outcome_family$family == "gaussian") {
    sigma <- params[["sigma_y"]]
  } else {
    sigma <- NULL
  }
  names(outcome.params) <- gsub("^outcome_", "", names(outcome.params))
  proxy.params <- params[proxy.names]
  names(proxy.params) <- gsub("^proxy_", "", names(proxy.params))
  
  # Select likelihood functions
  if ((outcome_family$family == "binomial") && (outcome_family$link == 'logit')) {
    outcome.llfun <- ll.logistic
  } else if (outcome_family$family == "gaussian") {
    outcome.llfun <- ll.gaussian_factory(sigma)
  } else {
    stop("Only logistic or Gaussian models are supported for the outcome model.")
  }
  
  if ((proxy_family$family == "binomial") && (proxy_family$link == 'logit')) {
    proxy.llfun <- ll.logistic
  } else {
    stop("Only logistic regression is supported for the proxy model.")
  }
  
  # Likelihood: observed
  # str(outcome.params)
  # str(outcome.model.matrix)
  ll.y.obs <- outcome.llfun(y.obs, outcome.params, outcome.model.matrix)
  ll.w.obs <- proxy.llfun(proxy.obs, proxy.params, proxy.model.matrix)
  if (any(!is.finite(ll.y.obs)) || any(!is.finite(ll.w.obs))) {
    return(1e10)
  }
  ll.obs <- sum(ll.y.obs + ll.w.obs)
  
  # Likelihood: unobserved
  df.unobs <- df[is.na(df[[response.var]]),]
  if (nrow(df.unobs) > 0) {
    if (outcome_family$family == "binomial") {
      df.unobs.y1 <- df.unobs; df.unobs.y1[[response.var]] <- 1
      df.unobs.y0 <- df.unobs; df.unobs.y0[[response.var]] <- 0
      
      mm_y1 <- model.matrix(outcome_formula, model.frame(outcome_formula, df.unobs.y1))
      mm_y0 <- model.matrix(outcome_formula, model.frame(outcome_formula, df.unobs.y0))
      
      ll.y.1 <- outcome.llfun(rep(1, nrow(df.unobs)), outcome.params, mm_y1)
      ll.y.0 <- outcome.llfun(rep(0, nrow(df.unobs)), outcome.params, mm_y0)
      
      proxy.mm.y1 <- model.matrix(proxy_formula, model.frame(proxy_formula, df.unobs.y1))
      proxy.mm.y0 <- model.matrix(proxy_formula, model.frame(proxy_formula, df.unobs.y0))
      proxy.unobs <- model.frame(proxy_formula, df.unobs)[[proxy.variable]]
      
      ll.w.1 <- proxy.llfun(proxy.unobs, proxy.params, proxy.mm.y1)
      ll.w.0 <- proxy.llfun(proxy.unobs, proxy.params, proxy.mm.y0)
      
      ll1 <- ll.y.1 + ll.w.1
      ll0 <- ll.y.0 + ll.w.0
      
      ll.unobs <- sum(matrixStats::colLogSumExps(rbind(ll1, ll0)))
      if (any(!is.finite(ll.y.obs)) || any(!is.finite(ll.w.obs))) {
        return(1e10)
      }
      
    } else if (outcome_family$family == "gaussian") {
      mm_y_unobs <- model.matrix(outcome_formula, model.frame(outcome_formula, df.unobs))
      mu <- as.vector(mm_y_unobs %*% outcome.params)
      
      if (!is.finite(sigma) || sigma <= 0) return(1e10)
      
      y_samples <- matrix(
        rnorm(nrow(df.unobs) * K, mean = rep(mu, each = K), sd = sigma),
        nrow = nrow(df.unobs), ncol = K
      )
      
      ll.proxy.samples <- apply(y_samples, 2, function(y_sim_col) {
        df.sim <- df.unobs
        df.sim[[response.var]] <- y_sim_col
        proxy.mm.sim <- model.matrix(proxy_formula, model.frame(proxy_formula, df.sim))
        proxy.llfun(model.frame(proxy_formula, df.unobs)[[proxy.variable]], proxy.params, proxy.mm.sim)
      })
      
      ll.unobs <- sum(matrixStats::colLogSumExps(ll.proxy.samples) - log(K))
      if (any(!is.finite(ll.proxy.samples))) return(1e10)
    } else {
      stop("Unsupported family for unobserved outcome modeling.")
    }
    
    ll <- ll.obs + ll.unobs
  } else {
    ll <- ll.obs
  }
  
  return(-ll) # Return negative log-likelihood for optimization
}


.measerr_mle_dv <- function(df, outcome_formula, outcome_family = binomial(link = 'logit'),
                            proxy_formula, proxy_family = binomial(link = 'logit'),
                            truth_formula, truth_family, maxit = 1e6, method = "L-BFGS-B",
                            K = 20) {
  
  # Model frames & matrices
  outcome_mf <- model.frame(outcome_formula, df)
  proxy_mf <- model.frame(proxy_formula, df)
  outcome_mm <- model.matrix(outcome_formula, outcome_mf)
  proxy_mm <- model.matrix(proxy_formula, proxy_mf)
  
  # Param counts
  n_outcome_params <- ncol(outcome_mm)
  n_proxy_params <- ncol(proxy_mm)
  
  # Param names
  outcome_param_names <- paste0("outcome_", colnames(outcome_mm))
  proxy_param_names <- paste0("proxy_", colnames(proxy_mm))
  
  # Append sigma_y for Gaussian
  if (outcome_family$family == "gaussian") {
    all_param_names <- c(outcome_param_names, proxy_param_names, "sigma_y")
  } else {
    all_param_names <- c(outcome_param_names, proxy_param_names)
  }
  
  # Start values and bounds
  start <- setNames(rnorm(length(all_param_names)), all_param_names)
  lower <- rep(-Inf, length(start))
  
  # Optimization
  fit <- optim(
    par = start,
    fn = .nll_dv,
    lower = lower,
    method = method,
    hessian = TRUE,
    control = list(maxit = maxit),
    df = df,
    outcome_formula = outcome_formula,
    outcome_family = outcome_family,
    proxy_formula = proxy_formula,
    proxy_family = proxy_family,
    truth_formula = truth_formula,
    truth_family = truth_family,
    K = K
  )
  
  return(fit)
}