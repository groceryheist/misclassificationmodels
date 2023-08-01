.measrr_mle_nll <- function(params, df, outcome_formula, outcome_family=gaussian(), proxy_formula, proxy_family=binomial(link='logit'), truth_formula, truth_family=binomial(link='logit')) {
    df.obs <- model.frame(outcome_formula, df)
    
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
    param.idx <- 1
    outcome.model.matrix <- model.matrix(outcome_formula, df)
    ## likelihood for the fully observed data
    n.outcome.model.covars <- dim(outcome.model.matrix)[2]
    outcome.params <- params[param.idx:(n.outcome.model.covars + index_shift)]
    param.idx <- param.idx + n.outcome.model.covars + index_shift

    if ((proxy_family$family=="binomial") && (proxy_family$link=='logit')) {
        proxy.llfun <- ll.logistic
    }
    if ((truth_family$family=="binomial") && (truth_family$link=='logit')) {
        truth.llfun <- ll.logistic
    }   
    ll.y.obs <- outcome.llfun(y.obs, outcome.params, outcome.model.matrix)
    
    df.obs <- model.frame(proxy_formula,df)
    n.proxy.model.covars <- dim(proxy.model.matrix)[2]
    proxy.params <- params[param.idx:(n.proxy.model.covars+param.idx - 1)]
    param.idx <- param.idx + n.proxy.model.covars
    proxy.obs <- with(df.obs, eval(parse(text=proxy.variable)))

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
    fit <- optim(start, fn = .measrr_mle_nll, lower = lower, method = method, hessian = TRUE, control = list(maxit=maxit),
                 df = df, outcome_formula = outcome_formula, outcome_family = outcome_family, proxy_formula = proxy_formula,
                 proxy_family = proxy_family, truth_formula = truth_formula, truth_family = truth_family)
    return(fit)
}

##hacky

.conv_formula <- function(formula) {
    res <- list()
    tokenized_formula <- strsplit(as.character(formula), " ")
    stopifnot("||" %in% unlist(tokenized_formula))
    stopifnot(sum(unlist(tokenized_formula) == "||") == 1)
    res$yproxy <- "||" %in% tokenized_formula[[2]]
    if (!res$yproxy) {
        sign_idx <- which(tokenized_formula[[3]] == "||")
        res$truth <- tokenized_formula[[3]][sign_idx - 1]
        res$proxy <- tokenized_formula[[3]][sign_idx + 1]
        res$outcome_formula <- paste(c(tokenized_formula[[2]], "~", tokenized_formula[[3]][setdiff(seq_along(tokenized_formula[[3]]), c(sign_idx, sign_idx + 1))]), collapse = " ")
        res$naive_formula <- paste(c(tokenized_formula[[2]], "~", tokenized_formula[[3]][setdiff(seq_along(tokenized_formula[[3]]), c(sign_idx, sign_idx - 1))]), collapse = " ")
    } else {
        sign_idx <- which(tokenized_formula[[2]] == "||")
        res$truth <- tokenized_formula[[2]][sign_idx - 1]
        res$proxy <- tokenized_formula[[2]][sign_idx + 1]
        res$outcome_formula <- paste(c(res$truth, "~", tokenized_formula[[3]]), collapse = " ")
        res$naive_formula <- paste(c(res$proxy, "~", tokenized_formula[[3]]), collapse = " ")
    }
    return(res)
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
