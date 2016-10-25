#' Summarise the tedab object
#'
#' Takes a tedab object and prints out the following summary details:
#' \itemize{
#'  \item{the number of observations}
#'  \item{the number of observations that exceed the normalised eccentricity limit}
#'  \item{the normalised eccentricity threshold}
#' }
#'
#'
#' @export
summary.tedab <- function(x, ...){
  cat("TEDA - Batch Output \n")
  cat("------------------- \n")
  cat("Number of observations:\n")
  cat(length(x$eccentricity))
  cat("\nNumber observations that exceed normalised eccentricity limit:\n")
  cat(sum(x$outlier))
  cat("\nNormalised eccentricity threshold:\n")
  cat(x$ecc_threshold)
}
