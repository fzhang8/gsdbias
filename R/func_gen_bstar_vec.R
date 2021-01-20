#' Lower boundary for \eqn{W(T)-\muT}
#'
#' @param t_vec Timing vector of interim looks and final analysis in information fraction. For example, 
#' 		a trial design of one interim look at half information and final analysis has \code{t_vec=c(0.5,1)}
#' @param c_vec Symmetric boundaries for \eqn{W(T)/(T)^{1/2}}
#' @param mu drift parameter
#'
#' @return Vector of lower boundaries for \eqn{W(T)-\muT} with same length as \code{c_vec}
#' @export
#'
#' @examples
gen_bstar_vec <- function(t_vec,c_vec,mu){
		-sqrt(t_vec)*c_vec - mu*t_vec}
