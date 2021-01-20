#' Calculate \eqn{\sigma}
#'
#' @param t_vec timing vector of interim looks and final analysis in information fraction. For example, 
#' 		a trial design of one interim look at half information and final analysis has \code{t_vec=c(0.5,1)}
#'
#' @return vector of \eqn{\sigmai}, same length as \code{t_vec}.
#' @export
#'
#' @examples
gen_sigma_vec <- function(t_vec){
	sqrt(c(t_vec[1],t_vec[2:length(t_vec)] - t_vec[1:(length(t_vec)-1)]))
}
