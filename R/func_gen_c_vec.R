#' Title
#'
#' @param t_vec
#' @param option
#' @param alphalevel
#' @param phivalue
#'
#' @return
#' @export
#'
#' @examples
gen_c_vec <- function(t_vec,option,alphalevel,phivalue){
	if(option == 3){
		ldbounds::bounds(t_vec,iuse=c(option,option),alpha=c(alphalevel/2,alphalevel/2),phi = c(phivalue,phivalue))$upper.bounds
	}else{
		ldbounds::bounds(t_vec,iuse=c(option,option),alpha=c(alphalevel/2,alphalevel/2))$upper.bounds
	}
}
