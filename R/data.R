#' Synthetic datasets
#'
#' @description
#'`research_data`, `val_data`, `research_data2`, and `val_data2` are synthetic datasets for demonstrating how this package works.
#'
#'`research_data*` datasets represent the primary data wherein a **proxy** variable (*w*) contains misclassifications from an automated classifer. `val_data*` datasets represent the validation data wherein both **groundtruth** (e.g. from human annotation, can either be *x* or *y*) and *w* are included. Theoretically, we are not interested in studying *w* per se. Suppose the groundtruth is *x* and we are interested in the relationship between *x* and *y*; we want to estimate the regression coefficient \eqn{B_{xy}}. But we use *w* as a low-cost proxy of the groundtruth *x* and can only estimate \eqn{B_{wy}}.
#'
#' @details
#' The datasets were synthesized with the following parameters:
#' ## `research_data` and `val_data`
#' For this pair of datasets, we are interested in the relationship between `x` (independent variable) and `y` (dependent variable; outcome). The data was synthesized with \eqn{B_{xy}} being 0.
#' * `w`: proxy variable of `x` (independent variable) with misclassifications; the prediction accuracy is 0.6. The prediction error is not differential.
#' * `y`: dependent variable with no misclassifications
#' * `z`: another independent variable with no misclassifications
#' * `x`: groundtruth in the validation data
#' ## `research_data2` and `val_data2`
#" For this pair of datasets, we are interested in the relationship between `x` (independent variable) and `y` (dependent variable; outcome). The data was synthesized with \eqn{B_{xy}} being 0.
#' * `w`: proxy variable of `y` (dependent variable) with misclassifications; the prediction accuracy is 0.6. The prediction error is differential.
#' * `x`: independent variable with no misclassifications
#' * `z`: another independent variable with no misclassifications
#' * `y`: groundtruth in the validation data
#' @seealso [glm_fixit()]
#' @inherit glm_fixit examples
"research_data"

#' @rdname research_data
"val_data"

#' @rdname research_data
"val_data2"

#' @rdname research_data
"research_data2"
