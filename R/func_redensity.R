
#' Title
#'
#' @param u
#' @param i
#' @param sigmas
#' @param cstars
#' @param bstars
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
