#' Title
#'
#' @param t_vec
#'
#' @return
#' @export
#'
#' @examples
gen_sigma_vec <- function(t_vec){
	sqrt(c(t_vec[1],t_vec[2:length(t_vec)] - t_vec[1:(length(t_vec)-1)]))
}
