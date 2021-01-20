#' Overall bias of group sequential design
#' 
#' Calculate the overall bias
#'
#' @param t_vec timing vector of interim looks and final analysis in information fraction. For example, 
#' 		a trial design of one interim look at half information and final analysis has \code{t_vec=c(0.5,1)}
#' @param spd 1 and 2 correspond to alpha spending functions which give O'Brien Fleming and Pocock 
#'						type boundaries, respectively. A value of 3 is the power family. See below for details.
#' @param alpha type I error, default is 0.05. See below for details.
#' @param mu drift parameter
#'
#' @return overall bias in drift scale.
#' @section Details: Argument \code{spd} and \code{alpha} essentially utilize arguments \code{iuse} and 
#'									\code{alpha} of function \code{bounds}	from package "\code{ldbounds}". Please refer to 
#'									package "\code{ldbounds}" for more details.
#' @export
#'
#' @examples #The overall bias for a trial with 4 interim looks at information
#'#fraction 0.2,0.4,0.6 and 0.8, with Pocock type boundary, and drift 3
#'> ovrlbias(t_vec = c(0.2,0.4,0.6,0.8,1),spd = 2,alpha = 0.05,mu = 3)
#'stop at: 1 
#'stop at: 2 
#'stop at: 3 
#'stop at: 4 
#'stop at: 5 
#'[1] 0.6308207
 
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
