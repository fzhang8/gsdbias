#' Symmetric boundaries for \eqn{W(T)/(T)^{1/2}}
#'
#' @param t_vec Timing vector of interim looks and final analysis in information fraction. For example, 
#' 		a trial design of one interim look at half information and final analysis has \code{t_vec=c(0.5,1)}
#' @param option 1 and 2 correspond to alpha spending functions which give O'Brien Fleming and Pocock 
#'						type boundaries, respectively. A value of 3 is the power family. See below for details.
#' @param alphalevel Type I error, default is 0.05 (See below).
#' @param phivalue value used when option=3 or 4 (See below).
#'
#' @return The vector of upper boundaries calculated, with same length as \code{t_vec}
#' @section Details: Argument \code{option}, \code{alphalevel} and \code{phivalue} essentially utilize 
#'									arguments \code{iuse}, \code{alpha} and \code{phi} of function \code{bounds}	from 
#'									package "\code{ldbounds}". Please refer to package "\code{ldbounds}" for more details.
#' @export
#'
#' @examples #The upper boundaries of Pocock type for a trial with 2 interim looks at information
#'#fraction 0.3 and 0.6
#'> gen_c_vec(t_vec = c(0.3,0.6,1),option = 2,alphalevel = 0.05)
#'[1] 2.311835 2.320930 2.268854
gen_c_vec <- function(t_vec,option,alphalevel=0.05,phivalue=1){
	if(option == 3){
		ldbounds::bounds(t_vec,iuse=c(option,option),alpha=c(alphalevel/2,alphalevel/2),phi = c(phivalue,phivalue))$upper.bounds
	}else{
		ldbounds::bounds(t_vec,iuse=c(option,option),alpha=c(alphalevel/2,alphalevel/2))$upper.bounds
	}
}
