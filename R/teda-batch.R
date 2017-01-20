#' Create teda batch object from a vector
#'
#' @description
#'
#' Takes a vector of observations and return a teda batch object, which
#' holds the eccentricity and typicality values, both original and normalised
#' versions.
#'
#' @details
#'
#' Uses the algorithm from Angelov (2014) to create a teda batch object.  This
#' contains a vector for the eccentricity (standard and normalised), typicality
#' (standard and normalised), the outlier threshold, and whether each observation
#' is or is not an outlier.  Also provides the original vector of values.
#'
#' @references
#' Angelov, P., 2014. Outside the box: an alternative data analytics framework.
#' Journal of Automation Mobile Robotics and Intelligent Systems, 8(2), pp.29-35.
#' DOI: 10.14313/JAMRIS_2-2014/16
#'
#' @param observations A vector of numeric observations
#' @param dist_type A string representing the distance metric to use, default
#'     value (and currently only supported value) is "Euclidean"
#' @return The teda batch object
#'
#' @examples
#' vec = c(20, 12, 10)
#' teda_b(vec)
#' # same as
#' a = teda_b(vec,"Euclidean")
#' summary(a)
#' plot(a)
#'
#' @family TEDA functions
#' @seealso \code{\link{teda_r}} for the recursive version of the TEDA framework.
#' @import stats
#' @export
teda_b = function(observations, dist_type = "Euclidean"){

  # Identify the type of the observations
  if (is.vector(observations)) {

    # Accumulated Proximity: The sum of the distances
    #   between all observation points in both directions
    spi = sum(as.matrix(dist(observations,diag = TRUE, upper = TRUE)))

    # Distance between point of interest and all other points,
    #   create the matrix.  `upper` is not set to true to match
    #   the paper equations - if it was set to true, the 2*
    #   operation could be removed from the eccentricity
    #   calculation
    distance_matrix = as.matrix(dist(observations,
                                     diag = TRUE))

    # Eccentricity: the relative (normalised) accumulated
    #   proximity of that data sample as a fraction of the
    #   accumulated proximity of all other data samples.
    #
    # Calculate eccentricity for each position
    ecc = unname((2 * apply(distance_matrix, 1, sum)) / spi)

    # Record the vector length for normalisation and outlier
    #  identification methods
    obs_len = length(observations)

    # Create the return object holding all the required output,
    #   and set the class for use with generic functions
    ret_obj = c()
    ret_obj$observations = observations
    ret_obj$eccentricity = round(ecc,3)
    ret_obj$typicality = round(1 - ecc,3)
    ret_obj$norm_eccentricity = round(ecc / 2,3)
    ret_obj$norm_typicality = round(ret_obj$typicality / (obs_len - 2), 3)
    ret_obj$outlier = ret_obj$norm_eccentricity > (1 / obs_len)
    ret_obj$ecc_threshold = round(1 / obs_len, 3)
    class(ret_obj) = "tedab"

    return(ret_obj)

  }else{

    # If a matrix or dataframe is past to the function, set the
    #  return object positions to be NA
    ret_obj = c()
    ret_obj$observations = NA
    ret_obj$eccentricity = NA
    ret_obj$typicality = NA
    ret_obj$norm_eccentricity = NA
    ret_obj$norm_typicality = NA
    ret_obj$outlier = NA
    ret_obj$ecc_threshold = NA
    class(ret_obj) = "tedab"

    return(ret_obj)
  }
}
