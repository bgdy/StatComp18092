#' @title generate a random sample from the Beta(a, b) distribution using R.
#' @description generate a random sample of size n from the Beta(a, b) distribution by the acceptance-rejection method.
#' @param n size of random sample (numeric)
#' @param a the parameter of Beta (numeric)
#' @param b the parameter of Beta (numeric)
#' @return a random sample or a text
#' @examples
#' \dontrun{
#' get.beta(10,3,2)
#' get.beta(1e2,2,3)
#' }
#' @export
get.beta=function(n,a=1,b=1){
  if(a<=0||b<=0){
    return("a,b should bigger than 0")
  }
  else{
    y=numeric(n)
    k=0
    while(k<n){
      u=runif(1,0,1/beta(a,b))
      x=runif(1)
      if(u<x^(a-1)*(1-x)^(b-1)/beta(a,b)){
        #we accept x
        k=k+1
        y[k]=x
      }
    }
    return(y)
  }
}
