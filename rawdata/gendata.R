require(data.table)
simulate_data <- function(N = 1000, m = 500, B0 = 0, Bxy = .3, Bzx = .3, Bzy = -.3, seed, y_explained_variance=0.1, prediction_accuracy=0.8, y_bias=-0.75, accuracy_imbalance_difference=0.3){
    set.seed(seed)
    # make w and y dependent
    z <- rbinom(N, 1, plogis(qlogis(0.5)))
    x <- rbinom(N, 1, plogis(Bzx * z + qlogis(0.5)))

    y.var.epsilon <- (var(Bzy * z) + var(Bxy *x) + 2*cov(Bzy*z,Bxy*x)) * ((1-y_explained_variance)/y_explained_variance)
    y.epsilon <- rnorm(N, sd = sqrt(y.var.epsilon))
    y <- Bzy * z + Bxy * x + y.epsilon
    
    df <- data.table::data.table(x=x,y=y,z=z)

    if(m < N){
        df <- df[sample(nrow(df), m), x.obs := x]
    } else {
        df <- df[, x.obs := x]
    }
    odds.x1 <- qlogis(prediction_accuracy) + y_bias*qlogis(pnorm(scale(df[x==1]$y)))
    odds.x0 <- qlogis(prediction_accuracy,lower.tail=F) + y_bias*qlogis(pnorm(scale(df[x==0]$y)))

    df[x==0,w:=plogis(rlogis(.N,odds.x0))]
    df[x==1,w:=plogis(rlogis(.N,odds.x1))]

    df[,w_pred := as.integer(w > 0.5)]

    ## print(mean(df[z==0]$x == df[z==0]$w_pred))
    ## print(mean(df[z==1]$x == df[z==1]$w_pred))
    ## print(mean(df$w_pred == df$x))
    ## print(mean(df[y>=0]$w_pred == df[y>=0]$x))
    ## print(mean(df[y<=0]$w_pred == df[y<=0]$x))
    return(df)
}

simulate_data_y <- function(N, m, B0 = 0, Bxy = .3, Bzy = -0.3, seed, prediction_accuracy=0.73, x_bias=-0.75) {
    set.seed(seed)

    # make w and y dependent
    z <- rbinom(N, 1, 0.5)
    x <- rbinom(N, 1, 0.5)

    ystar <- Bzy * z + Bxy * x + B0
    y <- rbinom(N,1,plogis(ystar))

    # glm(y ~ x + z, family="binomial")

    df <- data.table(x=x,y=y,ystar=ystar,z=z)

    if(m < N) {
        df <- df[sample(nrow(df), m), y.obs := y]
    } else {
        df <- df[, y.obs := y]
    }
    
    odds.y1 <- qlogis(prediction_accuracy) + x_bias*df[y==1]$x
    odds.y0 <- qlogis(prediction_accuracy,lower.tail = FALSE) + x_bias*df[y==0]$x

    df[y==0,w:=plogis(rlogis(.N,odds.y0))]
    df[y==1,w:=plogis(rlogis(.N,odds.y1))]

    df[,w_pred := as.integer(w > 0.5)]

    ## print(mean(df[x==0]$y == df[x==0]$w_pred))
    ## print(mean(df[x==1]$y == df[x==1]$w_pred))
    ## print(mean(df$w_pred == df$y))
    return(df)
}


require(dplyr)
require(data.table)
df <- simulate_data(seed = 12121, Bxy = 0, N = 5000, m = 300, prediction_accuracy = .6)
df$x <- df$x.obs

df %>% filter(is.na(x)) %>% select(-x, -x.obs, -w) %>% rename(w = `w_pred`) %>% as.data.frame -> research_data

df %>% filter(!is.na(x)) %>% select(-x.obs, -w) %>% rename(w = `w_pred`) %>% as.data.frame -> val_data

usethis::use_data(research_data, overwrite = TRUE)
usethis::use_data(val_data, overwrite = TRUE)

df2 <- simulate_data_y(seed = 12121, Bxy = 0.0, N = 7000, m = 500, prediction_accuracy = .8)

df2$y <- df2$y.obs

df2 %>% filter(!is.na(y)) %>% select(-y.obs, -w, -ystar) %>% rename(w = `w_pred`) %>% as.data.frame -> val_data2

df2 %>% filter(is.na(y)) %>% select(-y.obs, -w, -ystar) %>% rename(w = `w_pred`) %>% select(w, x, z) %>% as.data.frame -> research_data2
usethis::use_data(research_data2, overwrite = TRUE)
usethis::use_data(val_data2, overwrite = TRUE)
