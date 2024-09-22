tab_acc <- function(class.obj){

  if (!inherits(class.obj, "table")) {
    stop("class.obj must be of class table")
  }

  a <- (round(sum(diag(class.obj))/sum(class.obj), 2))*100
  paste0(a, "%")
}
