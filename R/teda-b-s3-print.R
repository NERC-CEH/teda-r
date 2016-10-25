#' Print the tedab object
#'
#' Takes a tedab object and prints out the values within
#'
#' @export
print.tedab <- function(x, ...){
  cat("Eccentricity:\n")
  cat(x$eccentricity)
  cat("\nTypicality:\n")
  cat(x$typicality)
  cat("\nNormalised Eccentricity:\n")
  cat(x$norm_eccentricity)
  cat("\nNormalised Typicality:\n")
  cat(x$norm_typicality)
}
