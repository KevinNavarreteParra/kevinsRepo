#' Confusion Matrix
#'
#' @param class.obj A confusion matrix of class table
#'
#' @return A character string of the accuracy of the confusion matrix
#' @export
#'
#' @examples
#'
#' # Example data
#' data <- data.frame(y = c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0),
#'                   x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
#'
#' # Fit a logistic regression model
#' model <- glm(y ~ x, data = data, family = binomial)
#'
#' # Predict the model
#' data$pred <- ifelse(predict(model, data, type = "response") > 0.5, 1, 0)
#'
#' # Create a confusion matrix
#' class.obj <- table(data$y, data$pred)
#'
#' # Calculate the accuracy of the confusion matrix
#' tab_acc(class.obj)
tab_acc <- function(class.obj){

  if (!inherits(class.obj, "table")) {
    stop("class.obj must be of class table")
  }

  a <- (round(sum(diag(class.obj))/sum(class.obj), 2))*100
  paste0(a, "%")
}
