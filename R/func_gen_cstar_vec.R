#' Upper boundaries for \eqn{W(T)-\mu T}
#'
#' Calculate vector of upper boundaries for \eqn{W(T)-\mu T}
#' 
#' @param t_vec timing vector of interim looks and final analysis in information fraction. For example, 
#' 		a trial design of one interim look at half information and final analysis has \code{t_vec=c(0.5,1)}
#' @param c_vec vector of symmetric boundaries for \eqn{W(T)/(T)^{1/2}}
#' @param mu drift parameter
#'
#' @return vector of upper boundaries for \eqn{W(T)-\mu T}
#' @export
#'
#' @examples
gen_cstar_vec <- function(t_vec,c_vec,mu){
		sqrt(t_vec)*c_vec - mu*t_vec}


