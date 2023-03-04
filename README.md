
<!-- README.md is generated from README.Rmd. Please edit that file -->

WIP

## Installation

You can install the development version of `misclassificationmodels`
like so:

``` r
remotes::install_github("chainsawriot/misclassificationmodels")
```

## Example

This package provides an example dataset (5000 observations)

``` r
library(misclassificationmodels)
head(research_data)
#>             y z w
#> 1 -0.33569058 0 1
#> 2 -0.54095121 1 1
#> 3  0.13380991 1 0
#> 4 -0.03837528 0 0
#> 5 -0.38738200 1 1
#> 6 -0.23798152 0 1
```

Suppose we are interested in the relationship between `y` and `x`. `x`
is not in this dataset, because it is perhaps expensive to collect.
Instead, we have another variable `w`, which is from an automated
classifier trained to predict `x`. It is known that the automated
classifier misclassifies `x`. In order to find out the extent of
misclassification, we produced another dataset `val_data` (300
observations).

``` r
head(val_data)
#>   x          y z w
#> 1 1 -0.2335122 1 1
#> 2 0 -0.1899035 0 1
#> 3 0  0.3991066 0 0
#> 4 1  0.3038228 0 0
#> 5 1 -0.3231921 1 1
#> 6 0 -0.2279294 0 1
```

In this dataset, we have the misclassified variable `w` and the variable
`x` (e.g. from human coders using
[oolong](https://github.com/chainsawriot/oolong)). The variable `w` is a
**proxy**, whereas `x` is the **ground truth** of the proxy. We verify
that the automated classifer did actually misclassify quite badly:

``` r
caret::confusionMatrix(table(w = val_data$w, x = val_data$x), mode = "prec_recall", positive = "1")
#> Confusion Matrix and Statistics
#> 
#>    x
#> w     0   1
#>   0  82  62
#>   1  51 105
#>                                           
#>                Accuracy : 0.6233          
#>                  95% CI : (0.5658, 0.6784)
#>     No Information Rate : 0.5567          
#>     P-Value [Acc > NIR] : 0.01136         
#>                                           
#>                   Kappa : 0.2432          
#>                                           
#>  Mcnemar's Test P-Value : 0.34685         
#>                                           
#>               Precision : 0.6731          
#>                  Recall : 0.6287          
#>                      F1 : 0.6502          
#>              Prevalence : 0.5567          
#>          Detection Rate : 0.3500          
#>    Detection Prevalence : 0.5200          
#>       Balanced Accuracy : 0.6226          
#>                                           
#>        'Positive' Class : 1               
#> 
```

Yes: [“Validate, validate,
validate.”](https://web.stanford.edu/~jgrimmer/tad2.pdf)

## Our approach

The function `glm_fixit()` does regression analysis but also corrects
for misclassification in proxy using the validation data. The method is
based on the general likelihood modeling framework drawn from Carroll et
al. (2006). The function is very similar to `glm()` but with two
changes:

1.  The formula interface has been extended with the double-pipe
    operator to denote proxy variable. For example, `x || w` indicates
    `w` is the proxy of the ground truth `x`.
2.  The validation data (with both proxy and ground truth, as well as
    another variables, e.g. `y` and `z`) must be provided as `data2`

<!-- end list -->

``` r
res <- glm_fixit(formula = y ~ x || w + z, data = research_data, data2 = val_data)
res
#> Corrected Estimator:
#> (Intercept)           x           z 
#>  0.01516333 -0.03472420 -0.30309068 
#> Feasible Estimator:
#> (Intercept)           x           z 
#>  0.03213542 -0.02689811 -0.32240014 
#> Naive Estimator:
#> (Intercept)           w           z 
#>   0.1646739  -0.4094128  -0.2340211
```

``` r
summary(res)
#> Coefficients (Corrected Estimator): 
#>                      Estimate       2.5 %      97.5 %
#> (Intercept)        0.01516333 -0.04537934  0.07570600
#> x                 -0.03472420 -0.14086740  0.07141899
#> z                 -0.30309068 -0.32787711 -0.27830426
#> sigma_y            0.44670402  0.43772664  0.45568141
#> proxy_(Intercept) -1.10822797 -1.45402097 -0.76243497
#> proxy_y           -2.70806010 -2.98781776 -2.42830243
#> proxy_z            0.07215776 -0.07526944  0.21958496
#> proxy_x            1.17831538  0.63205182  1.72457893
#> truth_(Intercept)  0.19279212 -0.02817580  0.41376004
#> 
#> 
#> Coefficients (Naive Estimator): 
#>               Estimate      2.5 %     97.5 %
#> (Intercept)  0.1646739  0.1458282  0.1835196
#> w           -0.4094128 -0.4326380 -0.3861877
#> z           -0.2340211 -0.2572460 -0.2107961
#> 
#> 
#> Coefficients (Feasible Estimator): 
#>                Estimate      2.5 %     97.5 %
#> (Intercept)  0.03213542 -0.0509001  0.1151709
#> x           -0.02689811 -0.1240775  0.0702813
#> z           -0.32240014 -0.4192639 -0.2255364
```

If you have knowledge on the data generation processes of proxy and
ground truth, you can represent your knowledge as formulas and override
the default.

``` r
res2 <- glm_fixit(formula = y ~ x || w + z, data = research_data, data2 = val_data,
                  proxy_formula = w ~ x*y*z, proxy_family = binomial(),
                  truth_formula = x ~ z, truth_family = binomial())
summary(res2)
#> Coefficients (Corrected Estimator): 
#>                       Estimate       2.5 %      97.5 %
#> (Intercept)        0.005886621 -0.05031435  0.06208759
#> x                 -0.018856044 -0.12285301  0.08514092
#> z                 -0.301838080 -0.32784254 -0.27583362
#> sigma_y            0.446942726  0.43812015  0.45576530
#> proxy_(Intercept) -0.959800958 -1.33451622 -0.58508569
#> proxy_x            1.042336855  0.35782294  1.72685077
#> proxy_y           -2.174217554 -2.82216137 -1.52627374
#> proxy_z           -0.376796064 -1.08515272  0.33156059
#> proxy_x:y         -1.089530468 -2.45723102  0.27817008
#> proxy_x:z          0.510480082 -0.58732354  1.60828370
#> proxy_y:z         -0.807645419 -1.88807659  0.27278575
#> proxy_x:y:z        1.338816602 -0.61494703  3.29258024
#> truth_(Intercept)  0.053638670 -0.24453641  0.35181375
#> truth_z            0.297812662 -0.14333011  0.73895544
#> 
#> 
#> Coefficients (Naive Estimator): 
#>               Estimate      2.5 %     97.5 %
#> (Intercept)  0.1646739  0.1458282  0.1835196
#> w           -0.4094128 -0.4326380 -0.3861877
#> z           -0.2340211 -0.2572460 -0.2107961
#> 
#> 
#> Coefficients (Feasible Estimator): 
#>                Estimate      2.5 %     97.5 %
#> (Intercept)  0.03213542 -0.0509001  0.1151709
#> x           -0.02689811 -0.1240775  0.0702813
#> z           -0.32240014 -0.4192639 -0.2255364
```

## What is “Naive Estimator”?

One can model the relationship between `y` and `x` by assuming `w` to be
a misclassification-free proxy, which many studies did assume to be so.
This approach is called “naive”.

``` r
glm(y~w+z, data = research_data)
#> 
#> Call:  glm(formula = y ~ w + z, data = research_data)
#> 
#> Coefficients:
#> (Intercept)            w            z  
#>      0.1647      -0.4094      -0.2340  
#> 
#> Degrees of Freedom: 4699 Total (i.e. Null);  4697 Residual
#> Null Deviance:       1052 
#> Residual Deviance: 753.7     AIC: 4744
```

The “naive” estimator suggests the regression coefficient to be -.4
\[1\], which is heavily biased by misclassification. Also, it is not
biased towards zero (the so-called “attenuation bias”) but away from
zero. It is because the misclassification is differential, i.e. not
random.

## What is “Feasible Estimator”?

One can also analyze the validation data only. This approach gives
unbiased but imprecise estimates.

``` r
glm(y~x+z, data = val_data)
#> 
#> Call:  glm(formula = y ~ x + z, data = val_data)
#> 
#> Coefficients:
#> (Intercept)            x            z  
#>     0.03214     -0.02690     -0.32240  
#> 
#> Degrees of Freedom: 299 Total (i.e. Null);  297 Residual
#> Null Deviance:       61.68 
#> Residual Deviance: 53.79     AIC: 343.8
```

## Dependent variable

The same method can also be applied to regression analysis where the
dependent variable is a proxy.

``` r
head(research_data2)
#>   w x z
#> 1 0 0 0
#> 2 0 0 1
#> 3 1 1 1
#> 4 1 1 0
#> 5 1 1 0
#> 6 0 1 0
```

``` r
res3 <- glm_fixit(formula = y || w ~ x + z, data = research_data2,
                  data2 = val_data2, family = binomial("logit"))
res3
#> Corrected Estimator:
#> (Intercept)           x           z 
#> -0.05883197  0.06278834 -0.30812034 
#> Feasible Estimator:
#> (Intercept)           x           z 
#> -0.05023838  0.14891833 -0.31366643 
#> Naive Estimator:
#> (Intercept)           x           z 
#> -0.07540977 -0.40103564 -0.13383571
```

## References

1.  Carroll, R. J., Ruppert, D., Stefanski, L. A., & Crainiceanu, C. M.
    (2006). Measurement Error in Nonlinear Models (2nd ed.). Chapman &
    Hall/CRC.

-----

1.  We (the authors of this package) know that it is heavily biased
    because `research_data` and `val_data` are synthetic datasets by us.
    We set the regression coefficient of `x` at exactly **0**. If H0 is
    no association between `x` and `y` and `w` is assumed to be a
    misclassification-free proxy of `x`, one would commit a Type I
    error.
