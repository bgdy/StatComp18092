#' @title Use integration to estimate cdf of Cauchy distribution using R.
#' @description Use integration to estimate cdf of Cauchy distribution.
#' @param y the number you want to estimate (numeric)
#' @param eta1 the parameter of cauchy (numeric)
#' @param theta1 the parameter of cauchy (numeric)
#' @return a number
#' @examples
#' \dontrun{
#' cdf.cau(4)
#' cdf.cau(2,3,5)
#' }
#' @export
cdf.cau <- function(y,eta1=0,theta1=1){  #integrate
  f=function(x,eta=eta1,theta=theta1){   # function
    return(1/(theta*pi*(1+((x-eta)/theta)^2)))
  }
  integrate(f, lower=-Inf, upper=y, rel.tol=.Machine$double.eps^0.25,eta=eta1,theta=theta1)$value
}
