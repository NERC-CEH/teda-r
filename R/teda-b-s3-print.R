#' Print the tedab object
#'
#' @description Takes a tedab object and prints out the values within
#'
#' @details
#'
#' Takes a tedab object and prints out each vector in order of:
#' eccentricity, typicality, normalised eccentricity, and normalised typicality.
#'
#' @param x The teda batch (tedab) object with which to create the printed output.
#' @param ... additional arguments affecting the summary produced.
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
