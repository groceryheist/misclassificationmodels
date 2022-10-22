
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
#> 1 -0.38203402 0 1
#> 2 -0.58550439 1 1
#> 3  0.10200748 1 0
#> 4 -0.03910045 0 0
#> 5 -0.42903323 1 1
#> 6 -0.28247858 0 1
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
#> 1 1 -0.2722558 1 1
#> 2 0 -0.1934921 0 1
#> 3 0  0.4066484 0 0
#> 4 1  0.2695641 0 0
#> 5 1 -0.3636303 1 1
#> 6 0 -0.2322365 0 1
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
#>   0  82  63
#>   1  51 104
#>                                           
#>                Accuracy : 0.62            
#>                  95% CI : (0.5624, 0.6752)
#>     No Information Rate : 0.5567          
#>     P-Value [Acc > NIR] : 0.01537         
#>                                           
#>                   Kappa : 0.2371          
#>                                           
#>  Mcnemar's Test P-Value : 0.30290         
#>                                           
#>               Precision : 0.6710          
#>                  Recall : 0.6228          
#>                      F1 : 0.6460          
#>              Prevalence : 0.5567          
#>          Detection Rate : 0.3467          
#>    Detection Prevalence : 0.5167          
#>       Balanced Accuracy : 0.6196          
#>                                           
#>        'Positive' Class : 1               
#> 
```

Yes: [“Validate, validate,
validate.”](https://web.stanford.edu/~jgrimmer/tad2.pdf)

## Our approach

The function `glm_fixit()` does regression analysis but also corrects
for misclassification in proxy using the information in validation data.
The method is based on the general likelihood modeling framework drawn
from Carroll et al. (2006). The function is very simular to `glm()` but
with two changes:

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
#>  0.01820867 -0.07688855 -0.30603316 
#> Feasible Estimator:
#> (Intercept)           x           z 
#>  0.03274267 -0.06740639 -0.32282343 
#> Naive Estimator:
#> (Intercept)           w           z 
#>   0.1518150  -0.4246592  -0.2366182
```

``` r
summary(res)
#> Coefficients (Corrected Estimator): 
#>                      Estimate       2.5 %      97.5 %
#> (Intercept)        0.01820867 -0.04327340  0.07969073
#> x                 -0.07688855 -0.18390190  0.03012480
#> z                 -0.30603316 -0.33133190 -0.28073443
#> sigma_y            0.45475431  0.44483618  0.46467244
#> proxy_(Intercept) -1.06443964 -1.40485575 -0.72402354
#> proxy_y           -2.63902564 -2.88201828 -2.39603299
#> proxy_z            0.05370348 -0.09069282  0.19809979
#> proxy_x            1.02270589  0.48246000  1.56295178
#> truth_(Intercept)  0.20134684 -0.02070900  0.42340268
#> 
#> 
#> Coefficients (Naive Estimator): 
#>               Estimate      2.5 %     97.5 %
#> (Intercept)  0.1518150  0.1326380  0.1709919
#> w           -0.4246592 -0.4482473 -0.4010712
#> z           -0.2366182 -0.2602060 -0.2130304
#> 
#> 
#> Coefficients (Feasible Estimator): 
#>                Estimate       2.5 %      97.5 %
#> (Intercept)  0.03274267 -0.05186194  0.11734728
#> x           -0.06740639 -0.16642217  0.03160938
#> z           -0.32282343 -0.42151757 -0.22412928
```

If you have information on the data generation processes of proxy and
ground truth, you can overide the default.

``` r
res2 <- glm_fixit(formula = y ~ x || w + z, data = research_data, data2 = val_data,
                  proxy_formula = w ~ x*y*z, proxy_family = binomial(),
                  truth_formula = x ~ z, truth_family = binomial())
summary(res2)
#> Coefficients (Corrected Estimator): 
#>                       Estimate      2.5 %      97.5 %
#> (Intercept)        0.003416043 -0.0538049  0.06063698
#> x                 -0.053183012 -0.1587901  0.05242403
#> z                 -0.302314297 -0.3294259 -0.27520272
#> sigma_y            0.455559075  0.4461590  0.46495920
#> proxy_(Intercept) -0.962707699 -1.3321562 -0.59325923
#> proxy_x            0.946832241  0.2681144  1.62555007
#> proxy_y           -2.106621379 -2.7571816 -1.45606120
#> proxy_z           -0.335358234 -1.0372244  0.36650795
#> proxy_x:y         -1.147796409 -2.6035123  0.30791945
#> proxy_x:z          0.439663253 -0.6632072  1.54253371
#> proxy_y:z         -0.729543473 -1.8278959  0.36880895
#> proxy_x:y:z        1.266049189 -0.7992589  3.33135728
#> truth_(Intercept)  0.053447292 -0.2449874  0.35188194
#> truth_z            0.312339089 -0.1302410  0.75491914
#> 
#> 
#> Coefficients (Naive Estimator): 
#>               Estimate      2.5 %     97.5 %
#> (Intercept)  0.1518150  0.1326380  0.1709919
#> w           -0.4246592 -0.4482473 -0.4010712
#> z           -0.2366182 -0.2602060 -0.2130304
#> 
#> 
#> Coefficients (Feasible Estimator): 
#>                Estimate       2.5 %      97.5 %
#> (Intercept)  0.03274267 -0.05186194  0.11734728
#> x           -0.06740639 -0.16642217  0.03160938
#> z           -0.32282343 -0.42151757 -0.22412928
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
#>      0.1518      -0.4247      -0.2366  
#> 
#> Degrees of Freedom: 4699 Total (i.e. Null);  4697 Residual
#> Null Deviance:       1094 
#> Residual Deviance: 778.7     AIC: 4897
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
#>     0.03274     -0.06741     -0.32282  
#> 
#> Degrees of Freedom: 299 Total (i.e. Null);  297 Residual
#> Null Deviance:       64.17 
#> Residual Deviance: 55.85     AIC: 355
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
