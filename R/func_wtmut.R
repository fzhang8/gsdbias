
#' Expectation of \eqn{(W(T)-\mu T; T=ti)}
#' 
#' Expectation of \eqn{(W(T)-\mu T; T=ti)} given \code{ti}
#' 
#' @param i Index of interim look, e.g., \code{i=2} for the second interim look. 
#'					\code{i=length(t_vec)} for the final analysis.
#' @param sigmas Vector of \eqn{\sigmai}, same length as \code{cstars} and \code{bstars}.
#' @param cstars Vector of upper boundaries for \eqn{W(T)-\mu T}.
#' @param bstars Vector of lower boundaries for \eqn{W(T)-\mu T}.
#'
#' @return Expectation of \eqn{W(T)-\mu T; T=ti} depending on \code{i}.
#' @export
#'
#' @examples
wtmut <- function(i,sigmas,cstars,bstars){
				M <- length(sigmas)
				sigma <- sigmas[i]
				cstar <- cstars[i]
				bstar <- bstars[i]
				if(i == 1){
					return(sigma * (dnorm(cstar/sigma) - dnorm(bstar/sigma)))
				}else if(i %in% seq(2,M-1)){
					q <- function(x){dnorm((cstar-x)/sigma) - dnorm((bstar-x)/sigma)}
					r <- function(x){1 - pnorm((cstar-x)/sigma) + pnorm((bstar-x)/sigma)}
					integrand <- function(x){(sigma * q(x) + x * r(x)) * redensity(u = x,i = i - 1,sigmas=sigmas,cstars=cstars,bstars=bstars)}
		return(cubature::adaptIntegrate(f = integrand,lowerLimit = bstars[i-1],upperLimit = cstars[i-1])$integral)
				}else if(i == M){
					return(cubature::adaptIntegrate(f = function(x){x * redensity(u=x,i=i-1,sigmas=sigmas,cstars=cstars,bstars=bstars)},
													lowerLimit = bstars[i-1],
													upperLimit = cstars[i-1])$integral)
			}
}

