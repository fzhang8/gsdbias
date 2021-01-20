
#' Transform from hazard ratio scale to drift scale
#'
#' @param hr hazard ratio
#' @param trtfrac treatment randomization fraction, e.g., \code{trtfrac} = 0.5 for balanced randomization
#' @param maxevents maximum event number
#'
#' @return drift value corresponding to associated hazard ratio, treatment randomization fraction and
#' 								maximum event number.
#' @export
#'
#' @examples
hr2drift <- function(hr,trtfrac,maxevents){
	log(hr)*sqrt(trtfrac * (1 - trtfrac) * maxevents)
}
