
#' Recursive density function describing the probability of \eqn{(W(T)-\mu T; T)}
#'
#' Recursive density function describing the probability of \eqn{(W(T)-\mu T; T)}
#' 
#' @param u Input of function
#' @param i Index of interim look, e.g., \code{i=2} for the second interim look. 
#'					\code{i=length(t_vec)} for the final analysis.
#' @param sigmas Vector of \eqn{\sigmai}, same length as \code{cstars} and \code{bstars}.
#' @param cstars Vector of upper boundaries for \eqn{W(T)-\mu T}.
#' @param bstars Vector of lower boundaries for \eqn{W(T)-\mu T}.
#'
#' @return 
#' @export
#'
#' @examples
redensity <- function(u,i,sigmas,cstars,bstars){
	sigma <- sigmas[i]
	if(i == 1){
			return(dnorm(u/sigma)/sigma)
	}
	else{
		cstar <- cstars[i-1]
		bstar <- bstars[i-1]
		integrand <- function(x){sigma^(-1) * dnorm(u/sigma - x/sigma) * redensity(u=x,i=i-1,sigmas=sigmas,cstars=cstars,bstars=bstars)}
		outt <- cubature::adaptIntegrate(f = integrand,lowerLimit = bstar,upperLimit = cstar)$integral
		return(outt)
	}
}
