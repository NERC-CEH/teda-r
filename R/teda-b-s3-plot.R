#' Plot the tedab object
#'
#' @description Takes a tedab object and plots each metric individually
#'
#' @details
#'
#' Takes a tedab object and creates four plots in order of:
#' eccentricity, typicality, normalised eccentricity, and normalised typicality.
#'
#' @param x The teda batch (tedab) object with which to create the plot output.
#' @param ... additional arguments affecting the summary produced.
#'
#'
#' @import graphics
#' @export
plot.tedab <- function(x, ...){


  par(mfrow = c(2,2))
  plot(x = x$observations,
       y = x$eccentricity,
       main = "Eccentricity",
       xlab = "Obs. Idx",
       ylab = "Eccentricity",
       type = "h",
       ylim = c(0,1))

  plot(x = x$observations,
       y = x$typicality,
       main = "Typicality",
       xlab = "Obs. Idx",
       ylab = "Typicality",
       type = "h",
       ylim = c(0,1))

  plot(x = x$observations,
       y = x$norm_eccentricity,
       main = "Norm. Eccentricity",
       xlab = "Obs. Idx",
       ylab = "Norm. Eccentricity",
       type = "h",
       ylim = c(0,1))
  abline(h = x$ecc_threshold)

  plot(x = x$observations,
       y = x$norm_typicality,
       main = "Norm. Typicality",
       xlab = "Obs. Idx",
       ylab = "Norm. Typicality",
       type = "h",
       ylim = c(0,1))
  abline(h = x$ecc_threshold)
  par(mfrow = c(1,1))
}


