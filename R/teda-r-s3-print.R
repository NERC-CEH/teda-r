#' Print the tedar object
#'
#' Takes a tedar object and prints out the values within (currently the same as summarize).
#'
#' @export
print.tedar <- function(x, ...){
  cat("Current observation stream position (K):\n")
  cat(x$next_k - 1)

  if (x$next_k > 3) {
    cat("\nEccentricity:\n")
    cat(x$curr_eccentricity)
    cat("\nTypicality:\n")
    cat(x$curr_typicality)
    cat("\nNormalised Eccentricity:\n")
    cat(x$curr_norm_eccentricity)
    cat("\nNormalised Typicality:\n")
    cat(x$curr_norm_typicality)
    cat("\nIs the current observation an outlier?\n")
    cat(x$outlier)
    cat("\nNormalised Eccentricity Threshold:\n")
    cat(x$ecc_threshold)
  }else{
    cat("There are not enough observations to generate output.")
  }
}

