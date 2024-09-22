#' Test Function
#'
#' This is a simple test function that adds two numbers for testing purposes.
#'
#' @param x A numeric value.
#' @param y A numeric value.
#'
#' @return The sum of x and y.
#' @export
#'
#' @examples
#' # 1 + 2 = 3
#' test_function(1, 2)
test_function <- function(x, y) {
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("Both x and y must be numeric.")
  }
  a <- x + y
  return(a)
}
