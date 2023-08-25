
<!-- README.md is generated from README.Rmd. Please edit that file -->

WIP

## Installation

You can install the development version of `misclassificationmodels`
like so:

``` r
remotes::install_github("groceryheist/misclassificationmodels")
```

## Example

This package provides an example dataset (5000 observations)

``` r
library(misclassificationmodels)
head(research_data)
#>             y z w
#> 1 -0.33569058 0 1
#> 2 -0.54095121 1 1
#> 3  0.13380991 1 1
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
#>   0  91  58
#>   1  42 109
#>                                           
#>                Accuracy : 0.6667          
#>                  95% CI : (0.6102, 0.7198)
#>     No Information Rate : 0.5567          
#>     P-Value [Acc > NIR] : 6.623e-05       
#>                                           
#>                   Kappa : 0.3328          
#>                                           
#>  Mcnemar's Test P-Value : 0.1336          
#>                                           
#>               Precision : 0.7219          
#>                  Recall : 0.6527          
#>                      F1 : 0.6855          
#>              Prevalence : 0.5567          
#>          Detection Rate : 0.3633          
#>    Detection Prevalence : 0.5033          
#>       Balanced Accuracy : 0.6685          
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
#>  0.01270166 -0.02974907 -0.30309771 
#> Feasible Estimator:
#> (Intercept)           x           z 
#>  0.03213542 -0.02689811 -0.32240014 
#> Naive Estimator:
#> (Intercept)           w           z 
#>   0.1679598  -0.4073122  -0.2357524
```

``` r
summary(res)
#> Coefficients (Corrected Estimator): 
#>                      Estimate       2.5 %      97.5 %
#> (Intercept)        0.01270166 -0.04783119  0.07323451
#> x                 -0.02974907 -0.13428414  0.07478600
#> z                 -0.30309771 -0.32788368 -0.27831173
#> sigma_y            0.44679284  0.43787872  0.45570695
#> proxy_(Intercept) -1.45248070 -1.86290532 -1.04205609
#> proxy_y           -2.90747021 -3.29679876 -2.51814167
#> proxy_z            0.07512940 -0.09185432  0.24211311
#> proxy_x            1.78135107  1.18424878  2.37845335
#> truth_(Intercept)  0.22884418  0.01707423  0.44061413
#> 
#> 
#> Coefficients (Naive Estimator): 
#>               Estimate      2.5 %     97.5 %
#> (Intercept)  0.1679598  0.1489720  0.1869477
#> w           -0.4073122 -0.4305531 -0.3840713
#> z           -0.2357524 -0.2589914 -0.2125134
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
#> (Intercept)        0.005080719 -0.05094658  0.06110802
#> x                 -0.016964167 -0.11899696  0.08506863
#> z                 -0.302015485 -0.32796327 -0.27606770
#> sigma_y            0.446967552  0.43815861  0.45577649
#> proxy_(Intercept) -1.258269473 -1.70371876 -0.81282019
#> proxy_x            1.650970996  0.92383500  2.37810699
#> proxy_y           -2.406286695 -3.11132662 -1.70124677
#> proxy_z           -0.389169469 -1.20765302  0.42931408
#> proxy_x:y         -1.086160122 -2.34424353  0.17192329
#> proxy_x:z          0.416417440 -0.74920467  1.58203955
#> proxy_y:z         -0.721537156 -1.81562659  0.37255228
#> proxy_x:y:z        1.308084947 -0.48084458  3.09701447
#> truth_(Intercept)  0.087361579 -0.19612115  0.37084431
#> truth_z            0.298620190 -0.12475171  0.72199209
#> 
#> 
#> Coefficients (Naive Estimator): 
#>               Estimate      2.5 %     97.5 %
#> (Intercept)  0.1679598  0.1489720  0.1869477
#> w           -0.4073122 -0.4305531 -0.3840713
#> z           -0.2357524 -0.2589914 -0.2125134
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
#>      0.1680      -0.4073      -0.2358  
#> 
#> Degrees of Freedom: 4699 Total (i.e. Null);  4697 Residual
#> Null Deviance:       1052 
#> Residual Deviance: 755.5     AIC: 4755
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
#> -0.05961144  0.06306252 -0.30720561 
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
