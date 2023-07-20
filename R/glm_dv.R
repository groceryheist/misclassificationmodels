.nll_dv <- function(params, df, outcome_formula, outcome_family = gaussian(), proxy_formula,
                    proxy_family = binomial(link='logit'), truth_formula,
                    truth_family = binomial(link='logit')) {
    df.obs <- model.frame(outcome_formula, df)
    proxy.variable <- all.vars(proxy_formula)[1]
    proxy.model.matrix <- model.matrix(proxy_formula, df)
    response.var <- all.vars(outcome_formula)[1]
    y.obs <- with(df.obs,eval(parse(text=response.var)))
    outcome.model.matrix <- model.matrix(outcome_formula, df.obs)
    param.idx <- 1
    n.outcome.model.covars <- dim(outcome.model.matrix)[2]
    outcome.params <- params[param.idx:n.outcome.model.covars]
    param.idx <- param.idx + n.outcome.model.covars

    if ((outcome_family$family == "binomial") && (outcome_family$link == 'logit')) {
        outcome.llfun <- ll.logistic
    }##  else {
    ##     print("Only logistic regression is supported")
    ##     return()
    ## }

    if( (proxy_family$family=="binomial") && (proxy_family$link=='logit')) {
        proxy.llfun <- ll.logistic
    }##  else {
    ##     print("Only logistic regression is supported. The proxy family should be binomial(link='logit').")
    ##     return()
    ## }

    ll.y.obs <- outcome.llfun(y.obs, outcome.params, outcome.model.matrix)

    df.obs <- model.frame(proxy_formula,df)
    n.proxy.model.covars <- dim(proxy.model.matrix)[2]
    proxy.params <- params[param.idx:(n.proxy.model.covars+param.idx-1)]

    param.idx <- param.idx + n.proxy.model.covars
    proxy.obs <- with(df.obs, eval(parse(text=proxy.variable)))

    ll.w.obs <- outcome.llfun(proxy.obs, proxy.params, proxy.model.matrix)

    ll.obs <- sum(ll.y.obs + ll.w.obs)

    df.unobs <- df[is.na(df[[response.var]]),]
    df.unobs.y1 <- df.unobs
    df.unobs.y1[[response.var]] <- 1
    df.unobs.y0 <- df.unobs
    df.unobs.y0[[response.var]] <- 0
    
    ## integrate out y
    outcome.model.matrix.y1 <- model.matrix(outcome_formula, df.unobs.y1)
    outcome.model.matrix.y0 <- model.matrix(outcome_formula, df.unobs.y0)

    ll.y.unobs.1 <- outcome.llfun(df.unobs.y1[[response.var]], outcome.params, outcome.model.matrix.y1)
    ll.y.unobs.0 <- outcome.llfun(df.unobs.y0[[response.var]], outcome.params, outcome.model.matrix.y0)

    proxy.model.matrix.y1 <- model.matrix(proxy_formula, df.unobs.y1)
    proxy.model.matrix.y0 <- model.matrix(proxy_formula, df.unobs.y0)
    proxy.unobs <- with(df.unobs, eval(parse(text=proxy.variable)))

    ll.w.unobs.1 <- proxy.llfun(proxy.unobs, proxy.params, proxy.model.matrix.y1)
    ll.w.unobs.0 <- proxy.llfun(proxy.unobs, proxy.params,  proxy.model.matrix.y0)

    ll.unobs.1 <- ll.y.unobs.1 + ll.w.unobs.1
    ll.unobs.0 <- ll.y.unobs.0 + ll.w.unobs.0
    ll.unobs <- sum(matrixStats::colLogSumExps(rbind(ll.unobs.1,ll.unobs.0)))
    ll <- ll.unobs + ll.obs
    return(-ll)
}

.measerr_mle_dv <- function(df, outcome_formula, outcome_family=binomial(link='logit'), proxy_formula, proxy_family=binomial(link='logit'), truth_formula, truth_family, maxit = 1e6, method = "L-BFGS-B") {
    params <- colnames(model.matrix(outcome_formula, df))
    lower <- rep(-Inf, length(params))
    proxy.params <- colnames(model.matrix(proxy_formula, df))
    params <- c(params, paste0('proxy_', proxy.params))
    lower <- c(lower, rep(-Inf, length(proxy.params)))
    ##start <- rep(0.1,length(params))
    start <- rnorm(length(params))
    names(start) <- params
    fit <- optim(start, fn = .nll_dv, lower = lower, method = method, hessian = TRUE, control=list(maxit=maxit),
                 df = df, outcome_formula = outcome_formula, outcome_family = outcome_family, proxy_formula = proxy_formula,
                 proxy_family = proxy_family, truth_formula = truth_formula, truth_family = truth_family)
    return(fit)
}

