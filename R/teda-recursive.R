#' Create teda recursive object from observation (+ state)
#'
#'@description
#'
#' A recursive method that takes the state variables of previous mean,
#'  previous variance, and the current timestep position, along with the
#'  current observation.  It returns a teda recursive object.  Currently only a
#'  univariate implementation.
#'
#'@details
#'
#' The function has two intended ways of use: on the first pass, it only takes
#' the observation value as a paramter and the rest are provided by defaults,
#' on all other passes, it takes the current observation, the previous mean and
#' variance values, and the current k (number of observations) which includes the
#' current observation.
#'
#' On return, the teda recursive object holds:
#' \itemize{
#'  \item{the current observation}
#'  \item{the current mean}
#'  \item{the current variance}
#'  \item{the current observation's eccentricity}
#'  \item{the current observation's typicality}
#'  \item{the current observation's normalised eccentricity}
#'  \item{the current observation's normalised typicality}
#'  \item{whether the current observation is an outlier}
#'  \item{the current outlier threshold}
#'  \item{the next timestep value, k+1}
#' }
#'
#' It provides generic functions for print and summary, at this moment both
#' provide the same outout.
#'
#'@references
#'
#' Bezerra, C.G., Costa, B.S.J., Guedes, L.A. and Angelov, P.P., 2016, May.
#' A new evolving clustering algorithm for online data streams.
#' In Evolving and Adaptive Intelligent Systems (EAIS),
#' 2016 IEEE Conference on (pp. 162-168). IEEE.
#' DOI: 10.1109/EAIS.2016.7502508
#'
#' @param curr_observation A single observation, the most recent in a series
#' @param previous_mean The mean value returned by the previous call to this
#'     function, if no previous calls, default value is used.
#' @param previous_var The variance value returned by the previous call to this
#'     function, if no previous calls, default value is used.
#' @param k The count of observations processed by the recursive function,
#'     including the current observation
#' @param dist_type A string representing the distance metric to use, default
#'     value (and currently only supported value) is "Euclidean"
#' @return The teda recursive object
#'
#' @examples
#' vec = c(20, 12, 10, 20)
#' a = teda_r(vec[1])
#' b = teda_r(vec[2],
#'            a$curr_mean,
#'            a$curr_var,
#'            a$next_k)
#' c = teda_r(vec[3],
#'            b$curr_mean,
#'            b$curr_var,
#'            b$next_k)
#'
#' d = teda_r(vec[4],
#'            c$curr_mean,
#'            c$curr_var,
#'            c$next_k)
#' summary(d)
#'
#' @family TEDA functions
#' @export
teda_r = function(curr_observation,
                  previous_mean = curr_observation,
                  previous_var = 0,
                  k = 1,
                  dist_type = "Euclidean"){

  # Calculate the recursive mean value
  rec_mean =  (((k - 1)  / k) * previous_mean) + ((1 / k) * curr_observation)

  # Calculate the recursive variance value
  rec_var = (((k - 1) / k) * previous_var) + (1 / (k - 1)) * ((curr_observation - rec_mean) ^ 2)

  # Calculate the recursive eccentricity value
  rec_ecc = (1 / k) +  (((rec_mean - curr_observation) ^ 2) / (k * rec_var))

  # If the initial timestep, set the initial parameter values,
  #  else, use the previous timestep values
  ret_obj = c()

  ret_obj$curr_observation = curr_observation
  ret_obj$curr_mean = round(rec_mean, 3)

  if (k == 1)
    ret_obj$curr_var = 0
  else
    ret_obj$curr_var = round(rec_var, 3)

  ret_obj$curr_eccentricity = round(rec_ecc, 3)
  ret_obj$curr_typicality = round(1 - rec_ecc, 3)
  ret_obj$curr_norm_eccentricity = round(ret_obj$curr_eccentricity / 2, 3)
  ret_obj$curr_norm_typicality = round(ret_obj$curr_typicality / (k - 2), 3)
  ret_obj$outlier = ret_obj$curr_norm_eccentricity > (1 / k)

  ret_obj$ecc_threshold = round(1 / k, 3)
  ret_obj$next_k = k + 1
  class(ret_obj) = "tedar"

  return(ret_obj)
}
