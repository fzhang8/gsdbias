
#' Transform from drift scale to hazard ratio scale
#'
#' Calculate lower bound of expectation of hazard ratio estimate
#' 
#' @param drift expectation of drift estimate
#' @param trtfrac treatment randomization fraction, e.g., \code{trtfrac} = 0.5 for balanced randomization
#' @param maxevents maximum event number
#'
#' @return lower bound of expectation of hazard ratio estimate corresponding to drift expectation, treatment randomization fraction and
#' 								maximum event number.
#' @export
#'
#' @examples
drift2hr <- function(drift,trtfrac,maxevents){
	exp(drift / sqrt(trtfrac * (1 - trtfrac) * maxevents))
}