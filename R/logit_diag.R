#' Title: Common Logistic Regression Diagnostic Checks
#'
#' @param model A logit model of class glm
#' @param model_name character string for the prefix name that will accompany all objects generated
#' @param counter Integer corresponding to the model being evaluated
#'
#' @return A series of diagnostic checks for a logistic regression model exported to the global environment
#' @export logit_diag
#'
#' @importFrom rcompanion nagelkerke
#' @importFrom ResourceSelection hoslem.test
#' @importFrom car Predict
#'
#' @examples
#' data <- data.frame(y = c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0),
#'                    x1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#'                    x2 = c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1))
#'
#' model <- glm(y ~ x1 + x2, data = data, family = binomial)
#'
#' logit_diag(model, "model", 1)
logit_diag <- function(model, model_name, counter) {

  if (!inherits(model, "glm")) {
    stop("model must be of class glm")
  }

  # Calculate odds ratios and print
  ors_object <- ors(model)
  assign(paste0(model_name, ".ors", counter), ors_object, envir = .GlobalEnv)
  print(ors_object)

  # ANOVA test and print
  aod_object <- anova(model, test = "Chisq")
  assign(paste0(model_name, ".aod", counter), aod_object, envir = .GlobalEnv)
  print(aod_object)

  # Nagelkerke's R-squared and print
  rsq_object <- rcompanion::nagelkerke(model)
  assign(paste0(model_name, ".rsq", counter), rsq_object, envir = .GlobalEnv)
  print(rsq_object)

  # Hosmer-Lemeshow test and print
  hos_object <- ResourceSelection::hoslem.test(model$y, round(fitted(model), 1), g = 10)
  assign(paste0(model_name, ".hos", counter), hos_object, envir = .GlobalEnv)
  print(hos_object)

  # Prediction and confusion matrix
  pred_object <- car::Predict(model, newdata = d, type = "response")
  bpred_object <- ifelse(pred_object > 0.5, 1, 0)
  class_object <- table(d$res_outcome, bpred_object)
  assign(paste0(model_name, ".class", counter), class_object, envir = .GlobalEnv)
  print(class_object)

  # Accuracy table
  tab_acc_object <- tab_acc(class_object)
  assign(paste0(model_name, ".tab_acc", counter), tab_acc_object, envir = .GlobalEnv)
  print(tab_acc_object)
}
