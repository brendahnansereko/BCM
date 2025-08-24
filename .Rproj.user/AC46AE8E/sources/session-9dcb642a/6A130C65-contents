# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' Title
#'
#' @param outcome  numeric value for outcome measure at final time point
#' @param treatment categorical variable for treatment arm
#'
#' @returns A list with:
#' \describe{
#'   \item{treated_mean}{Mean outcome for treated group.}
#'   \item{control_mean}{Mean outcome for control group.}
#'   \item{ate}{Average Treatment Effect (treated_mean - control_mean).}
#' }
#'
#' @export
#' @examples
#' set.seed(123)
#' y <- rnorm(100, mean = 5) + rbinom(100, 1, 0.5) * 2
#' treat <- rbinom(100, 1, 0.5)
#' estimate_treatment_effect(y, treat)
#
estimate_treatment_effect <- function(outcome, treatment) {
  if(length(outcome) != length(treatment)) {
    stop("Outcome and treatment must have the same length")
  }
  if(!all(treatment %in% c(0,1))) {
    stop("Treatment must be coded as 0 or 1")
  }

  treated_mean <- mean(outcome[treatment == 1], na.rm = TRUE)
  control_mean <- mean(outcome[treatment == 0], na.rm = TRUE)

  ate <- treated_mean - control_mean

  return(list(
    treated_mean = treated_mean,
    control_mean = control_mean,
    ate = ate
  ))
}
