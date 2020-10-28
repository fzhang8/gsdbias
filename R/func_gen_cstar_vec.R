#' Title
#'
#' @param t_vec
#' @param c_vec
#' @param mu
#'
#' @return
#' @export
#'
#' @examples
gen_cstar_vec <- function(t_vec,c_vec,mu){
		sqrt(t_vec)*c_vec - mu*t_vec}
