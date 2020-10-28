

#' Title
#'
#' @param t_vec
#' @param spd
#' @param alpha
#' @param mu
#'
#' @return
#' @export
#'
#' @examples
ovrlbias <- function(t_vec,spd,alpha,mu){
	if(length(t_vec) == 1){
		return(0)
	}else{

		c_vec <- gen_c_vec(t_vec = t_vec,option=spd,alphalevel=alpha,phivalue=1)
		sigmas <- gen_sigma_vec(t_vec = t_vec)
		cstars <- gen_cstar_vec(t_vec = t_vec,c_vec = c_vec,mu = mu)
		bstars <- gen_bstar_vec(t_vec = t_vec,c_vec = c_vec,mu = mu)

		if(cstars[1] == -bstars[1]){
			return(0)
		}else{
			overallbias <- 0
			for(stp in seq(1,length(t_vec))){
				cat("stop at:",stp,"\n")
				overallbias <- overallbias + wtmut(i = stp,sigmas = sigmas,cstars = cstars,bstars = bstars) / t_vec[stp]
			}
			return(overallbias)
		}
	}
}
