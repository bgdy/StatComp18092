
#' @title Use Monte Carlo to estimate cdf of Beta(a, b) using R.
#' @description Use Monte Carlo to estimate integration to estimate cdf of Beta(a, b).
#' @param x the number you want to estimate (numeric)
#' @param n the number of simulations (numeric)
#' @param a the parameter of Beta (numeric)
#' @param b the parameter of Beta (numeric)
#' @return a number or text
#' @examples
#' \dontrun{
#' cdf.bet(0.2)
#' cdf.bet(0.5,2,3,1e4)
#' }
#' @export
cdf.bet=function(x,a=1,b=1,n=1e4){
  if(a<=0||b<=0){
    return("a,b should bigger than 0")
  }
  if(x<=0) return(0)
  if(x>=1) return(1)
  t=runif(n,0,x)
  y=mean(t^(a-1)*(1-t)^(b-1)/beta(a,b))*x #Use a frequency to approximation the expectation
  return(y)
}

