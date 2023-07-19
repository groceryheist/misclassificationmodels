ll.logistic <- function(outcome, model.params, model.matrix){
    ll <- vector(mode='numeric', length=length(outcome))
    print(model.params)
    print(dim(model.matrix))
    ll[outcome == 1] <- plogis(model.params %*% t(model.matrix[outcome==1,]), log=TRUE)
    ll[outcome == 0] <- plogis(model.params %*% t(model.matrix[outcome==0,]), log=TRUE, lower.tail=FALSE)
    return(ll)
}

# sigma is always the last model parameter
ll.gaussian <- function(outcome, model.params, model.matrix){
    ll <- vector(mode='numeric', length=length(outcome))
    ll[outcome == 1] <- dnorm(outcome, model.params[1:(length(model.params)-1)] %*% t(model.matrix), sd = model.params[length(model.params)], log=TRUE)
}
