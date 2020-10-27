
#' Conditional bias of group sequential design
#'
#' Calculate the conditional bias given that a trial stopped at an interim look
#'
#' @param i Index of interim look, e.g., \code{i=2} for the second interim look. 
#'					\code{i=length(t_vec)} for the final analysis.
#' @param t_vec Timing vector of interim looks and final analysis in information fraction. For example, 
#' 		a trial design of one interim look at half information and final analysis has \code{t_vec=c(0.5,1)}
#' @param spd 1 and 2 correspond to alpha spending functions which give O'Brien Fleming and Pocock 
#'						type boundaries, respectively. A value of 3 is the power family. See below for details.
#' @param alpha Type I error, default is 0.05.
#' @param mu drift parameter
#'
#' @return Conditional bias in drift scale given the trial has stopped at interim analysis of information
#' fraction \code{t_vec[i]}.
#' @section Details: Argument \code{spd} essentially utilizes argument \code{iuse} of function \code{bounds}
#'									from package "\code{ldbounds}". Please refer package "\code{ldbounds}" for more details.
#'
#' @examples example
cndbias <- function(i,t_vec,spd,alpha=0.05,mu){
	if(length(t_vec) == 1){
		return(0)
	}else{

		c_vec <- gen_c_vec(t_vec = t_vec,option=spd,alphalevel=alpha,phivalue=1)
		sigmas <- gen_sigma_vec(t_vec = t_vec)
		cstars <- gen_cstar_vec(t_vec = t_vec,c_vec = c_vec,mu = mu)
		bstars <- gen_bstar_vec(t_vec = t_vec,c_vec = c_vec,mu = mu)
		iprobs <- ldbounds::drift(zb = c_vec,t = t_vec,drft = mu)$exit.probs

		if(cstars[1] == -bstars[1]){
			return(0)
		}else{
			iprob <- ifelse(i != length(iprobs),iprobs[i],1 - sum(iprobs[1:(length(iprobs)-1)]))
			numerator <- wtmut(i = i,sigmas = sigmas,cstars = cstars,bstars = bstars)
			return(numerator / iprob / t_vec[i])
		}
	}
}
