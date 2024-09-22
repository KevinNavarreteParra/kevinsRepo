#' Calculate odds ratios and confidence intervals
#'
#' @param model A logistic regression model of class glm
#'
#' @return A matrix with exponentiated coefficients and confidence intervals
#' @export
#'
#' @examples
#' # Example data
#' data <- data.frame(y = c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0),
#'                   x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
#' # Fit a logistic regression model
#' model <- glm(y ~ x, data = data, family = binomial)
#' # Calculate odds ratios and confidence intervals
#' ors(model)
ors <- function(model){

  # check if model is of class glm
  if(!inherits(model, "glm")){
    stop("Model must be of class glm")
  }
  # exponentiate values
  # deposit results in matrix
  a <- cbind(exp(coef(model)), exp(confint(model)))

  # name columns
  colnames(a) <- c("OR", "2.5 %", "97.5 %")

  return(a)
}
