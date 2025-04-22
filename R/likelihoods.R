ll.logistic <- function(outcome, model.params, model.matrix) {
    ll <- vector(mode='numeric', length=length(outcome))
    ##print(model.params)
    ##print(dim(model.matrix))
    ll[outcome == 1] <- plogis(model.params %*% t(model.matrix[outcome==1,]), log.p = TRUE)
    ll[outcome == 0] <- plogis(model.params %*% t(model.matrix[outcome==0,]), log.p = TRUE, lower.tail=FALSE)
    return(ll)
}

ll.gaussian <- function(outcome, model.params, model.matrix) {
  beta <- model.params[1:(length(model.params) - 1)]
  sigma <- model.params[length(model.params)]
  stopifnot(ncol(model.matrix) == length(beta))
  mu <- as.vector(model.matrix %*% beta)
  dnorm(outcome, mean = mu, sd = sigma, log = TRUE)
}
