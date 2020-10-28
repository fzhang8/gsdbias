

#' Title
#'
#' @param hr
#' @param trtfrac
#' @param maxevents
#'
#' @return
#' @export
#'
#' @examples
hr2drift <- function(hr,trtfrac,maxevents){
	log(hr)*sqrt(trtfrac * (1 - trtfrac) * maxevents)
}
