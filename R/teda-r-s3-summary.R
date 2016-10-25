#' Summarize the tedar object
#'
#' @description Takes a tedar object and prints out the summary values.
#'
#' @details
#'
#' Takes a tedar object and prints out the summary values.
#'
#' @param object The teda recursive (tedar) object with which to create the summary output.
#' @param ... additional arguments affecting the summary produced.
#'
#'
#'
#' @export
summary.tedar <- function(object, ...){
  cat("Current observation stream position (K):\n")
  cat(object$next_k - 1)

  if (object$next_k > 3) {
    cat("\nEccentricity:\n")
    cat(object$curr_eccentricity)
    cat("\nTypicality:\n")
    cat(object$curr_typicality)
    cat("\nNormalised Eccentricity:\n")
    cat(object$curr_norm_eccentricity)
    cat("\nNormalised Typicality:\n")
    cat(object$curr_norm_typicality)
    cat("\nIs the current observation an outlier?\n")
    cat(object$outlier)
    cat("\nNormalised Eccentricity Threshold:\n")
    cat(object$ecc_threshold)
  }else{
    cat("\nThere are not enough observations to generate output.")
  }
}

