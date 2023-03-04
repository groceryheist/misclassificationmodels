
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
for misclassification in proxy using the information in validation data.
The method is based on the general likelihood modeling framework drawn
from Carroll et al. (2006). The function is very similar to `glm()` but
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
#>  0.01503172 -0.03456093 -0.30304487 
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
#> (Intercept)        0.01503172 -0.04549873  0.07556217
#> x                 -0.03456093 -0.14069964  0.07157779
#> z                 -0.30304487 -0.32783188 -0.27825786
#> sigma_y            0.44671820  0.43774174  0.45569466
#> proxy_(Intercept) -1.10296624 -1.44774354 -0.75818894
#> proxy_y           -2.70476237 -2.98282545 -2.42669929
#> proxy_z            0.07240037 -0.07478795  0.21958868
#> proxy_x            1.16985653  0.62473209  1.71498097
#> truth_(Intercept)  0.19237950 -0.02869709  0.41345610
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

If you have information on the data generation processes of proxy and
ground truth, you can overide the default.

``` r
res2 <- glm_fixit(formula = y ~ x || w + z, data = research_data, data2 = val_data,
                  proxy_formula = w ~ x*y*z, proxy_family = binomial(),
                  truth_formula = x ~ z, truth_family = binomial())
summary(res2)
#> Coefficients (Corrected Estimator): 
#>                      Estimate       2.5 %      97.5 %
#> (Intercept)        0.00635533 -0.04988833  0.06259899
#> x                 -0.01963326 -0.12373290  0.08446637
#> z                 -0.30183593 -0.32787045 -0.27580140
#> sigma_y            0.44691813  0.43809122  0.45574504
#> proxy_(Intercept) -0.96020956 -1.33289921 -0.58751992
#> proxy_x            1.04534794  0.36417084  1.72652504
#> proxy_y           -2.15022321 -2.79430668 -1.50613974
#> proxy_z           -0.38414373 -1.09178126  0.32349380
#> proxy_x:y         -1.14373697 -2.50612613  0.21865219
#> proxy_x:z          0.51474059 -0.57797887  1.60746004
#> proxy_y:z         -0.87072794 -1.96159253  0.22013665
#> proxy_x:y:z        1.47133983 -0.50016855  3.44284820
#> truth_(Intercept)  0.05317137 -0.24481312  0.35115586
#> truth_z            0.30103182 -0.14024477  0.74230842
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
