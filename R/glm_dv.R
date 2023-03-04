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

    if((outcome_family$family == "binomial") && (outcome_family$link == 'logit')) {
        ll.y.obs <- vector(mode='numeric', length=length(y.obs))
        ll.y.obs[y.obs==1] <- plogis(outcome.params %*% t(outcome.model.matrix[y.obs==1,]),log=TRUE)
        ll.y.obs[y.obs==0] <- plogis(outcome.params %*% t(outcome.model.matrix[y.obs==0,]),log=TRUE,lower.tail=FALSE)
    }

    df.obs <- model.frame(proxy_formula,df)
    n.proxy.model.covars <- dim(proxy.model.matrix)[2]
    proxy.params <- params[param.idx:(n.proxy.model.covars+param.idx-1)]

    param.idx <- param.idx + n.proxy.model.covars
    proxy.obs <- with(df.obs, eval(parse(text=proxy.variable)))

    if( (proxy_family$family=="binomial") && (proxy_family$link=='logit')) {
        ll.w.obs <- vector(mode='numeric',length=dim(proxy.model.matrix)[1])
        ll.w.obs[proxy.obs==1] <- plogis(proxy.params %*% t(proxy.model.matrix[proxy.obs==1,]),log=TRUE)
        ll.w.obs[proxy.obs==0] <- plogis(proxy.params %*% t(proxy.model.matrix[proxy.obs==0,]),log=TRUE, lower.tail=FALSE)
    }

    ll.obs <- sum(ll.y.obs + ll.w.obs)

    df.unobs <- df[is.na(df[[response.var]]),]
    df.unobs.y1 <- df.unobs
    df.unobs.y1[[response.var]] <- 1
    df.unobs.y0 <- df.unobs
    df.unobs.y0[[response.var]] <- 0
    
    ## integrate out y
    outcome.model.matrix.y1 <- model.matrix(outcome_formula, df.unobs.y1)

    if((outcome_family$family == "binomial") && (outcome_family$link == 'logit')) {
        ll.y.unobs.1 <- vector(mode='numeric', length=dim(outcome.model.matrix.y1)[1])
        ll.y.unobs.0 <- vector(mode='numeric', length=dim(outcome.model.matrix.y1)[1])
        ll.y.unobs.1 <- plogis(outcome.params %*% t(outcome.model.matrix.y1),log=TRUE)
        ll.y.unobs.0 <- plogis(outcome.params %*% t(outcome.model.matrix.y1),log=TRUE,lower.tail=FALSE)
    }

    proxy.model.matrix.y1 <- model.matrix(proxy_formula, df.unobs.y1)
    proxy.model.matrix.y0 <- model.matrix(proxy_formula, df.unobs.y0)
    proxy.unobs <- with(df.unobs, eval(parse(text=proxy.variable)))

    if( (proxy_family$family=="binomial") && (proxy_family$link=='logit')) {
        ll.w.unobs.1 <- vector(mode='numeric',length=dim(proxy.model.matrix.y1)[1])
        ll.w.unobs.0 <- vector(mode='numeric',length=dim(proxy.model.matrix.y0)[1])
        ll.w.unobs.1[proxy.unobs==1] <- plogis(proxy.params %*% t(proxy.model.matrix.y1[proxy.unobs==1,]),log=TRUE)
        ll.w.unobs.1[proxy.unobs==0] <- plogis(proxy.params %*% t(proxy.model.matrix.y1[proxy.unobs==0,]),log=TRUE, lower.tail=FALSE)

        ll.w.unobs.0[proxy.unobs==1] <- plogis(proxy.params %*% t(proxy.model.matrix.y0[proxy.unobs==1,]),log=TRUE)
        ll.w.unobs.0[proxy.unobs==0] <- plogis(proxy.params %*% t(proxy.model.matrix.y0[proxy.unobs==0,]),log=TRUE, lower.tail=FALSE)
    }

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
