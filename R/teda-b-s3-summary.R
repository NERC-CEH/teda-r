#' Summarise the tedab object
#'
#' @description Summarises the teda batch object using an S3 method
#'
#' @details
#'
#' Takes a tedab object and prints out the following summary details:
#' \itemize{
#'  \item{the number of observations}
#'  \item{the number of observations that exceed the normalised eccentricity limit}
#'  \item{the normalised eccentricity threshold}
#' }
#'
#' @param object The teda batch (tedab) object with which to create the summary output.
#' @param ... additional arguments affecting the summary produced.
#'
#' @export
summary.tedab <- function(object, ...){
  cat("TEDA - Batch Output \n")
  cat("------------------- \n")
  cat("Number of observations:\n")
  cat(length(object$eccentricity))
  cat("\nNumber observations that exceed normalised eccentricity limit:\n")
  cat(sum(object$outlier))
  cat("\nNormalised eccentricity threshold:\n")
  cat(object$ecc_threshold)
}
