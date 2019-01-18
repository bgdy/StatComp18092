## ----echo=FALSE----------------------------------------------------------
set.seed(12345)
x <- rnorm(10)
y <- rnorm(10)
plot(x, y)

## ----echo=FALSE----------------------------------------------------------
seq(1,10)

## ----echo=FALSE----------------------------------------------------------
a= matrix(runif(8),4,2)
dimnames(a)=list(NULL,c("x","y"))
summary(lm(a[,1]~a[,2]))

## ----echo=FALSE----------------------------------------------------------
x <- rpois(40, lambda=5)
table(x)

## ------------------------------------------------------------------------
set.seed(12345);
# x 
x=c(0,1,2,3,4);
# p
p=c(0.1,0.2,0.2,0.2,0.3);
cp=cumsum(p);
m=1000;r=numeric(m);
# inverse transform method
r=x[findInterval(runif(m),cp)+1];
r

## ------------------------------------------------------------------------
table(r)/m

## ------------------------------------------------------------------------
a=sample(c(0,1,2,3,4),size=1000,replace=TRUE,prob=c(0.1,0.2,0.2,0.2,0.3));
table(a)/1000

## ------------------------------------------------------------------------
# theoretical probabilities
c(0.1,0.2,0.2,0.2,0.3)

## ------------------------------------------------------------------------
get.beta=function(n,a,b){
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

## ------------------------------------------------------------------------
# ues the function I just created
a=get.beta(1000,3,2)
a

## ------------------------------------------------------------------------
# Graph the histogram
hist(a,probability = TRUE , main = expression(f(x)==x^(3-1)*(1-x)^(2-1)/beta(3,2)))
x=seq(0,1,0.01)
lines(x,x^(3-1)*(1-x)^(2-1)/beta(3,2))

## ------------------------------------------------------------------------
r=4;
beta=2;
# n is the number of random observations
n=1000;
# generate a from gamma distribution
a=rgamma(n,r,beta)
# generate x from exp distribution
x=rexp(n,a)
# print random observations
x

## ------------------------------------------------------------------------
hist(x,probability = TRUE )

## ------------------------------------------------------------------------
set.seed(12345)
cdf.beta=function(x){
  n=1e4
  t=runif(n,0,x)
  a=mean(t^2*(1-t)^2/beta(3,3))*x #Use a frequency to approximation the expectation
  return(a)
}

## ------------------------------------------------------------------------
x=seq(0,1,.1)
y1=numeric(10)
for(i in seq(0,1,.1)){
  y1[i*10+1]=cdf.beta(i)
}
y2=pbeta(x,3,3)
plot(x,y1,type = "l",lwd=2,xlim=c(0,1))
lines(x, y2, lty = 2,lwd=2, col=2)

## ------------------------------------------------------------------------
a=numeric(9)
for(i in seq(1,9,1)){
  a[i]=cdf.beta(i/10) # use the function to estimate F(x) for x = 0.1, 0.2, . . . , 0.9
}
a

## ------------------------------------------------------------------------
b=a=numeric(9)
for(i in seq(1,9,1)){
  b[i]=pbeta(i/10,3,3) #use pbeta function
}
b

## ------------------------------------------------------------------------
anti.sample=function(a,  n=1e4, antithetic = TRUE){
  u <- runif(n/2,0,1)
  if (!antithetic) v <- runif(n/2) else v <- 1 - u # use antithetic variables
  u <- c(u, v)
  x=sqrt(-2*a^2*log(1-u)) #Inverse transformation
  return(x)
}

## ------------------------------------------------------------------------
x=anti.sample(3)

## ------------------------------------------------------------------------
hist(x,xlim=c(0,10),probability = TRUE )

## ------------------------------------------------------------------------
hist(x,xlim=c(0,10),probability = TRUE )
x=seq(0,10,.01)
a=3
y=x/a^2*exp(-x^2/(2*a^2)) #probability density function
lines(x,y)

## ------------------------------------------------------------------------
x1=anti.sample(3)
v1=var((x1[1:(length(x1)/2)]+x1[(length(x1)/2+1):length(x1)])/2)

x2=anti.sample(3, antithetic = FALSE)
v2=var((x2[1:(length(x2)/2)]+x2[(length(x2)/2+1):length(x2)])/2)
c(v1,v2)

## ------------------------------------------------------------------------
(v2-v1)/v2

## ------------------------------------------------------------------------
x <- seq(1, 5, .01)
g=x^2/sqrt(2*pi)*exp(-x^2/2) #g(x)
plot(x,g,type = "l",lwd=2,xlim=c(1,5))

## ------------------------------------------------------------------------
x <- seq(1, 5, .01)
g=x^2/sqrt(2*pi)*exp(-x^2/2) #g(x)
plot(x,g,type = "l",lwd=2,xlim=c(1,5))
f1=dcauchy(x,1.4,1.1) #f1(x)
lines(x, f1, lty = 2,lwd=2, col=2)
f2=dnorm(x,1.4,.8)*.6 #f2(x)
lines(x, f2, lty = 3,lwd=2,col=3)  

## ------------------------------------------------------------------------
n=1e5
x=rnorm(n,1.4,.8)
g=x^2/sqrt(2*pi)*exp(-x^2/2)*I(x>1) #g(x)
f=dnorm(x,1.4,0.8) #f(x)
fg=g/f #g(x)/f(x)

## ------------------------------------------------------------------------

c(mean(fg),var(fg)) # estimation and variance of estimation

## ------------------------------------------------------------------------
set.seed(12345)
n=1e3
m=1e3
G=numeric(m)
for(i in 1:m){
  x=rlnorm(n) #lognormal
  G[i]=1/(n^2*mean(x))*sum((2*seq(1,n)-n-1)*x[order(x)]) # caculate gini ratio
}

## ------------------------------------------------------------------------
c(mean(G),median(G))

## ------------------------------------------------------------------------
quantile(G, probs = seq(0, 1, 0.1))

## ------------------------------------------------------------------------
hist(G,probability = TRUE )

## ------------------------------------------------------------------------
G=numeric(m)
for(i in 1:m){
  x=runif(n) #uniform
  G[i]=1/(n^2*mean(x))*sum((2*seq(1,n)-n-1)*x[order(x)]) # caculate gini ratio
}

## ------------------------------------------------------------------------
c(mean(G),median(G))

## ------------------------------------------------------------------------
quantile(G, probs = seq(0, 1, 0.1))

## ------------------------------------------------------------------------
hist(G,probability = TRUE )

## ------------------------------------------------------------------------
G=numeric(m)
for(i in 1:m){
  x=rbinom(n, 1, 0.1) #bernoulli
  G[i]=1/(n^2*mean(x))*sum((2*seq(1,n)-n-1)*x[order(x)]) # caculate gini ratio
}

## ------------------------------------------------------------------------
c(mean(G),median(G))

## ------------------------------------------------------------------------
quantile(G, probs = seq(0, 1, 0.1))

## ------------------------------------------------------------------------
hist(G,probability = TRUE )

## ------------------------------------------------------------------------
m <- 1e3; n <- 1e3;
confi.interval=function(me,s){
  g.hat=numeric(m)
  for(i in 1:m){
    x <- rlnorm(n,me,s) #lognormal
    g.hat[i]=1/(n^2*mean(x))*sum((2*seq(1,n)-n-1)*x[order(x)]) #caculate Gini ratio
  }
  a=quantile(g.hat, probs = 0.025,names=FALSE)
  b=quantile(g.hat, probs = 0.975,names=FALSE)
  c(a,b) #95% confidence interva
}

## ------------------------------------------------------------------------
me=0
s=1
c=confi.interval(me,s)
a=c[1]
b=c[2]
c #95% confidence interva

## ------------------------------------------------------------------------
I=numeric(m)
g.hat=numeric(m)
for(i in 1:m){
  x <- rlnorm(n,me,s) # lognormal
  g.hat[i]=1/(n^2*mean(x))*sum((2*seq(1,n)-n-1)*x[order(x)]) #caculate gini ratio
  if(g.hat[i]>a && g.hat[i]<b){ #see if it in 95% confidence interval
    I[i]=1 # in 95% confidence interval
  }
  else{
    I[i]=0 # not in 95% confidence interval
  }
}
mean(I)

## ------------------------------------------------------------------------
library(MASS)
a <- mvrnorm(1e2,rep(0,2),matrix(c(1,0.5,0.5,1),2))
x=a[,1]
y=a[,2]
cor.test(x,y,method = "pearson")
cor.test(x,y,method = "spearman")
cor.test(x,y,method = "kendall")

## ------------------------------------------------------------------------
x=rexp(1e3) #X~exp(1)
z=sample(c(0,1e3), size=1e3, replace = TRUE, prob = c(0.9,0.1))
y=x+z

cor.test(x,y,method = "pearson")
cor.test(x,y,method = "spearman")
cor.test(x,y,method = "kendall")

## ------------------------------------------------------------------------
library(bootstrap)
set.seed(1)
x=matrix(c(576,635,558,578,666,580,555,661,651,605,653,575,545,572,594,339,330,281,303,344,307,300,343,336,313,312,274,276,288,296),ncol=2)
x #print x

## ------------------------------------------------------------------------
b.cor <- function(x,i) cor(x[i,1],x[i,2])
n=nrow(x) #length
theta.hat <- b.cor(x,1:n)
theta.jack <- numeric(n)
for(i in 1:n){
  theta.jack[i] <- b.cor(x,(1:n)[-i]) #jackknife
}
bias.jack <- (n-1)*(mean(theta.jack)-theta.hat) #bias
se.jack <- sqrt((n-1)*mean((theta.jack-theta.hat)^2)) #se
round(c(bias=bias.jack, se=se.jack),3) # estimation, bias and the standard error

## ------------------------------------------------------------------------
library(boot)
theta.boot <- function(dat,i) {
#function to compute the statistic
  y <- dat[i, 1]
  mean(y)
}
boot.obj <- boot(aircondit, statistic = theta.boot, R = 2000)
print(boot.ci(boot.obj,type = c("norm","basic",  "perc","bca")))

## ------------------------------------------------------------------------
pca <- function(x,i){
  lambda1=rev(sort(eigen(cov(x[i,]))$values))[1]
  sig=sum(rev(sort(eigen(cov(x[i,]))$values)))
  lambda1/sig
}
x=scor
n=nrow(x) #length
theta.hat <- pca(x,1:n)
theta.jack <- numeric(n)
for(i in 1:n){
  theta.jack[i] <- pca(x,(1:n)[-i]) #jackknife
}
bias.jack <- (n-1)*(mean(theta.jack)-theta.hat) #bias
se.jack <- sqrt((n-1)*mean((theta.jack-theta.hat)^2)) #se
round(c(bias=bias.jack, se=se.jack),3) # estimation, bias and the standard error

## ------------------------------------------------------------------------
library(DAAG,quietly=TRUE)
attach(ironslag)
    n <- length(magnetic)   #in DAAG ironslag
    e1 <- e2 <- e3 <- e4 <- numeric(n)

    # for n-fold cross validation
    # fit models on leave-one-out samples
    for (k in 1:n) {
        y <- magnetic[-k]
        x <- chemical[-k]

        J1 <- lm(y ~ x)
        yhat1 <- J1$coef[1] + J1$coef[2] * chemical[k]
        e1[k] <- magnetic[k] - yhat1

        J2 <- lm(y ~ x + I(x^2))
        yhat2 <- J2$coef[1] + J2$coef[2] * chemical[k] +
                J2$coef[3] * chemical[k]^2
        e2[k] <- magnetic[k] - yhat2

        J3 <- lm(log(y) ~ x)
        logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[k]
        yhat3 <- exp(logyhat3)
        e3[k] <- magnetic[k] - yhat3

        J4 <- lm(log(y) ~ log(x))
        logyhat4 <- J4$coef[1] + J4$coef[2] * log(chemical[k])
        yhat4 <- exp(logyhat4)
        e4[k] <- magnetic[k] - yhat4
    }

    c(mean(e1^2), mean(e2^2), mean(e3^2), mean(e4^2))

## ------------------------------------------------------------------------
library(DAAG)
    n <- length(magnetic);   #in DAAG ironslag
    e1 <- e2 <- e3 <- e4 <- numeric(n)

    # for n-fold cross validation
    # fit models on leave-one-out samples
    for (k in 1:n) {
      for(j in 1:n){
        y <- magnetic[c(-k,-j)]
        x <- chemical[c(-k,-j)]

        J1 <- lm(y ~ x)
        yhat1 <- J1$coef[1] + J1$coef[2] * chemical[k]
        e1[k] <- magnetic[k] - yhat1

        J2 <- lm(y ~ x + I(x^2))
        yhat2 <- J2$coef[1] + J2$coef[2] * chemical[k] +
                J2$coef[3] * chemical[k]^2
        e2[k] <- magnetic[k] - yhat2

        J3 <- lm(log(y) ~ x)
        logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[k]
        yhat3 <- exp(logyhat3)
        e3[k] <- magnetic[k] - yhat3

        J4 <- lm(log(y) ~ log(x))
        logyhat4 <- J4$coef[1] + J4$coef[2] * log(chemical[k])
        yhat4 <- exp(logyhat4)
        e4[k] <- magnetic[k] - yhat4
    }
    }

    c(mean(e1^2), mean(e2^2), mean(e3^2), mean(e4^2))

## ------------------------------------------------------------------------
set.seed(1)
attach(chickwts)
x <- sort(as.vector(weight[feed == "soybean"]))
y <- sort(as.vector(weight[feed == "linseed"]))
detach(chickwts)
R <- 999 #number of replicates
z <- c(x, y) #pooled sample
K <- 1:26
D <- numeric(R) #storage for replicates

CM=function(x,y){
  lx=length(x)
  ly=length(y)
  s1=sum((ecdf(x)(x)-ecdf(y)(x))^2)
  s2=sum((ecdf(x)(y)-ecdf(y)(y))^2)
  return((lx*ly)/(lx+ly)^2*(s1+s2))
}

D0 <- CM(x,y)
for (i in 1:R) {
#generate indices k for the first sample
k <- sample(K, size = 14, replace = FALSE)
x1 <- z[k]
y1 <- z[-k] #complement of x1
D[i] <- CM(x1,y1)
}



## ------------------------------------------------------------------------
p <- mean(c(D0, D) >= D0)
print(p)

## ------------------------------------------------------------------------
library(RANN)
library(energy)
library(Ball)
library(boot)

com=function(x,y){
  z <- c(x, y)
  Tn <- function(z, ix, sizes,k) {
  n1 <- sizes[1]; n2 <- sizes[2]; n <- n1 + n2
  if(is.vector(z)) z <- data.frame(z,0);
  z <- z[ix, ];
  NN <- nn2(data=z, k=k+1) # what's the first column?
  block1 <- NN$nn.idx[1:n1,-1]
  block2 <- NN$nn.idx[(n1+1):n,-1]
  i1 <- sum(block1 < n1 + .5); i2 <- sum(block2 > n1+.5)
  (i1 + i2) / (k * n)
  }
  set.seed(12345); N <- c(length(x), length(y))
  boot.obj <- boot(data = z, statistic = Tn, R = 999,
  sim = "permutation", sizes = N,k=3)
  ts <- c(boot.obj$t0,boot.obj$t)
  pnn.value <- mean(ts>=ts[1])
  
  boot.obs <- eqdist.etest(z, sizes=N, R=999)
  pen.value <- boot.obs$p.value
  
  pba.value = bd.test(x = x, y = y, R=999)$p.value
  return(c(pnn.value, pen.value, pba.value))
}


## ------------------------------------------------------------------------
x=rnorm(100,0,1)
y=rnorm(100,0,1.1)
com(x,y)

## ------------------------------------------------------------------------
x=rnorm(100,0,1)
y=rnorm(100,0.1,1.1)
com(x,y)

## ------------------------------------------------------------------------
x=rt(100,1)
y=c(rnorm(50,0.1),rnorm(50,-0.1))
print(com(x,y))

## ------------------------------------------------------------------------
x=rnorm(5,0,1)
y=rnorm(100,0,1.1)
com(x,y)

## ------------------------------------------------------------------------
f <- function(x) {
  return(1/(pi*(1+x^2))) # standard Cauchy distribution
}

m <- 10000
x <- numeric(m)
x[1] <- rnorm(1)
u <- runif(m)

for (i in 2:m) {
  xt <- x[i-1]
  y <- rnorm(1,xt)
  num <- f(y) * dnorm(xt,y)
  den <- f(xt) * dnorm(y, xt)
  if (u[i] <= num/den) x[i] <- y else {
    x[i] <- xt
    k <- k+1     #y is rejected
  }
}

b <- 1001      #discard the burnin sample
y <- x[b:m]
a <- seq(-10,10,0.1)

## ------------------------------------------------------------------------

hist(y, breaks="scott", main="", xlab="", freq=FALSE,xlim=c(-10,10),ylim=c(0,0.4))
lines(a, f(a))

## ------------------------------------------------------------------------
quantile(y,probs=seq(0.1,0.9,0.1))
qcauchy(seq(0.1,0.9,0.1))

## ------------------------------------------------------------------------
w <- .25 #width of the uniform support set
m <- 5000 #length of the chain
win=c(125,18,20,34)
x <- numeric(m) #the chain

prob <- function(y, win) {
  # computes (without the constant) the target density
  if (y < 0 || y >= 0.8)
  return (0)
  return((1/2+y/4)^win[1] *
  ((1-y)/4)^win[2] * ((1-y)/4)^win[3] *
  (y/4)^win[4] )
}

u <- runif(m) #for accept/reject step
v <- runif(m, -w, w) #proposal distribution
x[1] <- .25

for (i in 2:m) {
  y <- x[i-1] + v[i]
  if (u[i] <= prob(y, win) / prob(x[i-1], win))
  x[i] <- y else
  x[i] <- x[i-1]
}

index=1001:m


## ------------------------------------------------------------------------
theta.hat=mean(x[index])
print(theta.hat)

## ------------------------------------------------------------------------
set.seed(1)
Gelman.Rubin <- function(psi) {
  # psi[i,j] is the statistic psi(X[i,1:j])
  # for chain in i-th row of X
  psi <- as.matrix(psi)
  n <- ncol(psi)
  k <- nrow(psi)
  psi.means <- rowMeans(psi) #row means
  B <- n * var(psi.means) #between variance est.
  psi.w <- apply(psi, 1, "var") #within variances
  W <- mean(psi.w) #within est.
  v.hat <- W*(n-1)/n + (B/n) #upper variance est.
  r.hat <- v.hat / W #G-R statistic
  return(r.hat)
}

w <- .25 #width of the uniform support set
m <- 5000 #length of the chain
win=c(125,18,20,34)
x <- numeric(m) #the chain
prob <- function(y, win) {
  # computes (without the constant) the target density
  if (y < 0 || y >= 0.9)
  return (0)
  return((1/2+y/4)^win[1] *
  ((1-y)/4)^win[2] * ((1-y)/4)^win[3] *
  (y/4)^win[4] )
}

u <- runif(m) #for accept/reject step
v <- runif(m, -w, w) #proposal distribution
x[1] <- .25

this.chain <- function(win, N, X1) {
  #starting value X1
  x <- rep(0, N)
  x[1] <- X1
  u <- runif(N)
  
  for (i in 2:m) {
    y <- x[i-1] + v[i]
    if (u[i] <= prob(y, win) / prob(x[i-1], win))
    x[i] <- y else
    x[i] <- x[i-1]
  }
  return(x)
}

k <- 4 #number of chains to generate
n <- 5000 #length of chains
b <- 500 #burn-in length
#choose overdispersed initial values
x0 <- c(0.2, 0.4, 0.6, 0.8)
#generate the chains
X <- matrix(0, nrow=k, ncol=n)
for (i in 1:k)
  X[i, ] <- this.chain(win, n, x0[i])

psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi))
psi[i,] <- psi[i,] / (1:ncol(psi))
print(Gelman.Rubin(psi))
#plot psi for the four chains
par(mfrow=c(2,2))
#for (i in 1:k)
#plot(psi[i, (b+1):n], type="l",
#xlab=i, ylab=bquote(psi))
par(mfrow=c(1,1)) #restore default
#plot the sequence of R-hat statistics
rhat <- rep(0, n)
for (j in (b+1):n)
rhat[j] <- Gelman.Rubin(psi[,1:j])
#plot(rhat[(b+1):n], type="l", xlab="", ylab="R")
#abline(h=1.1, lty=2)

## ------------------------------------------------------------------------
u=c(seq(4,25),100,500,1000)
b=numeric(length(u))
c=numeric(length(u))
s1=function(a){   #S_k-1
  1-pt(sqrt(a^2*(k-1)/(k-a^2)),k-1)
}
s2=function(a){   #S_k
  1-pt(sqrt(a^2*k/(k+1-a^2)),k)
}
s=function(a){   #s1-s2
  1-pt(sqrt(a^2*(k-1)/(k-a^2)),k-1)-(1-pt(sqrt(a^2*k/(k+1-a^2)),k))
}
for(i in 1:length(u)){   
  k=u[i]
  b[i]=uniroot(s,c(0.01,2))$root    #get root
  c[i]=s1(b[i])
}

## ------------------------------------------------------------------------
matrix(c(u,b,c),length(u))

## ------------------------------------------------------------------------
set.seed(1)
f=function(x,eta=0,theta=1){   # function
  return(1/(theta*pi*(1+((x-eta)/theta)^2)))
}

res <- function(y,eta1=0,theta1=1){  #integrate
  integrate(f, lower=-Inf, upper=y, rel.tol=.Machine$double.eps^0.25,eta=eta1,theta=theta1)$value
}


## ------------------------------------------------------------------------
res(-2)
pcauchy(-2)

res(0)
pcauchy(0)

res(-2,2,3)
pcauchy(-2,2,3)

## ------------------------------------------------------------------------
library('nloptr')
library(lpSolve);suppressMessages(library(quantreg))
N=1000
na=28
nb=24
noo=41
nab=70
p=0.6   #initial est. for p
q=0.3
r=0.1
pm=numeric(0)
qm=numeric(0)
rm=numeric(0)
lofm=numeric(0)
lof=function(p,q,r){   #log maximum likelihood values
  return(log(choose(n,naa)*choose(n-naa,nbb)*choose(n-naa-nbb,noo)*choose(nao+nbo+nab,nao)*choose(nbo+nab,nbo))+(nao+nbo+nab)*log(2)+(2*naa+nao+nab)*log(p)+(2*nbb+nbo+nab)*log(q)+(2*noo+nao+nbo)*log(r))
}
for (j in 1:N) {
  naa=round(na*p^2/(p^2+2*p*r))
  nbb=round(nb*q^2/(q^2+2*q*r))
  nao=na-naa
  nbo=nb-nbb
  n=naa+nbb+noo+nao+nbo+nab
  if(abs(p-(2*naa+nao+nab)/2/n)<0.000000001&&abs(q-(2*nbb+nbo+nab)/2/n)<0.000000001&&abs(r-(2*noo+nbo+nao)/2/n)<0.000000001&&j>5) break
  p=(2*naa+nao+nab)/2/n  #update estimation
  q=(2*nbb+nbo+nab)/2/n
  r=(2*noo+nbo+nao)/2/n
  pm=c(pm,p)
  qm=c(qm,q)
  rm=c(rm,r)
  lofm=c(lofm,lof(p,q,r))
}

## ------------------------------------------------------------------------
c(p,q,r)

## ------------------------------------------------------------------------
exp(lofm)

## ------------------------------------------------------------------------
formulas <- list(
mpg ~ disp,
mpg ~ I(1 / disp),
mpg ~ disp + wt,
mpg ~ I(1 / disp) + wt
)

a=lapply(formulas,function(i){lm(i,mtcars)})
a

## ------------------------------------------------------------------------
bootstraps <- lapply(1:10, function(i) {
rows <- sample(1:nrow(mtcars), rep = TRUE)
mtcars[rows, ]
})

b=lapply(bootstraps,function(i){lm(mpg~disp,i)})
b

## ------------------------------------------------------------------------
rsq <- function(mod) summary(mod)$r.squared

lapply(c(a,b),rsq)

## ------------------------------------------------------------------------
trials <- replicate(
100,
t.test(rpois(10, 10), rpois(7, 10)),
simplify = FALSE
)
sapply(trials,function(i){i$p.value})

## ------------------------------------------------------------------------
set.seed(1)
library(parallel)
library(foreach)

#parallel
mcvMap=function(f, FUN.VALUE , ...) {
    out=Map(f, ...)
    vapply(out, FUN, FUN.VALUE)
}


#foreach
y=foreach(x=1:1000,.combine='rbind') %do% exp(x)



## ------------------------------------------------------------------------
fas.chi=function (x, y = NULL, correct = TRUE, p = rep(1/length(x), length(x)), 
          rescale.p = FALSE, simulate.p.value = FALSE, B = 2000) 
{
  DNAME <- deparse(substitute(x))
  if (!is.matrix(x) && !is.null(y)) {
    if (length(x) != length(y)) 
      stop("'x' and 'y' must have the same length")
    DNAME2 <- deparse(substitute(y))
    xname <- if (length(DNAME) > 1L || nchar(DNAME, "w") > 
                 30) 
      ""
    else DNAME
    yname <- if (length(DNAME2) > 1L || nchar(DNAME2, "w") > 
                 30) 
      ""
    else DNAME2
    OK <- complete.cases(x, y)
    x <- factor(x[OK])
    y <- factor(y[OK])
    if ((nlevels(x) < 2L) || (nlevels(y) < 2L)) 
      stop("'x' and 'y' must have at least 2 levels")
    x <- table(x, y)
    names(dimnames(x)) <- c(xname, yname)
    DNAME <- paste(paste(DNAME, collapse = "\n"), "and", 
                   paste(DNAME2, collapse = "\n"))
  }
  if (any(x < 0) || anyNA(x)) 
    stop("all entries of 'x' must be nonnegative and finite")
  if ((n <- sum(x)) == 0) 
    stop("at least one entry of 'x' must be positive")
  if (is.matrix(x)) {
    METHOD <- "Pearson's Chi-squared test"
    nr <- as.integer(nrow(x))
    nc <- as.integer(ncol(x))
    if (is.na(nr) || is.na(nc) || is.na(nr * nc)) 
      stop("invalid nrow(x) or ncol(x)", domain = NA)
    sr <- rowSums(x)
    sc <- colSums(x)
    E <- outer(sr, sc, "*")/n
    v <- function(r, c, n) c * r * (n - r) * (n - c)/n^3
    V <- outer(sr, sc, v, n)
    dimnames(E) <- dimnames(x)
    if (simulate.p.value && all(sr > 0) && all(sc > 0)) {
      setMETH()
      tmp <- .Call(C_chisq_sim, sr, sc, B, E)
      STATISTIC <- sum(sort((x - E)^2/E, decreasing = TRUE))
      PARAMETER <- NA
      PVAL <- (1 + sum(tmp >= almost.1 * STATISTIC))/(B + 
                                                        1)
    }
    else {
      if (simulate.p.value) 
        warning("cannot compute simulated p-value with zero marginals")
      if (correct && nrow(x) == 2L && ncol(x) == 2L) {
        YATES <- min(0.5, abs(x - E))
        if (YATES > 0) 
          METHOD <- paste(METHOD, "with Yates' continuity correction")
      }
      else YATES <- 0
      STATISTIC <- sum((abs(x - E) - YATES)^2/E)
      PARAMETER <- (nr - 1L) * (nc - 1L)
      PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
    }
  }
  else {
    if (length(dim(x)) > 2L) 
      stop("invalid 'x'")
    if (length(x) == 1L) 
      stop("'x' must at least have 2 elements")
    if (length(x) != length(p)) 
      stop("'x' and 'p' must have the same number of elements")
    if (any(p < 0)) 
      stop("probabilities must be non-negative.")
    if (abs(sum(p) - 1) > sqrt(.Machine$double.eps)) {
      if (rescale.p) 
        p <- p/sum(p)
      else stop("probabilities must sum to 1.")
    }
    METHOD <- "Chi-squared test for given probabilities"
    E <- n * p
    V <- n * p * (1 - p)
    STATISTIC <- sum((x - E)^2/E)
    names(E) <- names(x)
  }
  names(STATISTIC) <- "X-squared"
  names(PARAMETER) <- "df"
  return(STATISTIC)
}

## ------------------------------------------------------------------------
a=c(1,24,5,6,7,434)
b=c(325,645,75,7,23,352)
fas.chi(a,b)
chisq.test(a,b)

## ------------------------------------------------------------------------
fas.tab=function (...,   dnn = list.names(...), deparse.level = 1) 
{
    list.names <- function(...) {
        l <- as.list(substitute(list(...)))[-1L]
        nm <- names(l)
        fixup <- if (is.null(nm)) 
            seq_along(l)
        else nm == ""
        dep <- vapply(l[fixup], function(x) switch(deparse.level + 
            1, "", if (is.symbol(x)) as.character(x) else "", 
            deparse(x, nlines = 1)[1L]), "")
        if (is.null(nm)) 
            dep
        else {
            nm[fixup] <- dep
            nm
        }
    }
    
    args <- list(...)
    if (!length(args)) 
        stop("nothing to tabulate")
    if (length(args) == 1L && is.list(args[[1L]])) {
        args <- args[[1L]]
        if (length(dnn) != length(args)) 
            dnn <- if (!is.null(argn <- names(args))) 
                argn
            else paste(dnn[1L], seq_along(args), sep = ".")
    }
    bin <- 0L
    lens <- NULL
    dims <- integer()
    pd <- 1L
    dn <- NULL
    for (a in args) {
        if (is.null(lens)) 
            lens <- length(a)
        else if (length(a) != lens) 
            stop("all arguments must have the same length")
        cat <- if (is.factor(a)) {
            if (any(is.na(levels(a)))) 
                a

        }
        else {
            a <- factor(a)
            a
        }
        nl <- length(ll <- levels(cat))
        dims <- c(dims, nl)
        if (prod(dims) > .Machine$integer.max) 
            stop("attempt to make a table with >= 2^31 elements")
        dn <- c(dn, list(ll))
        bin <- bin + pd * (as.integer(cat) - 1L)
        pd <- pd * nl
    }
    names(dn) <- dnn
    bin <- bin[!is.na(bin)]
    if (length(bin)) 
        bin <- bin + 1L
    y <- array(tabulate(bin, pd), dims, dimnames = dn)
    class(y) <- "table"
    y
}

## ------------------------------------------------------------------------
fas.tab(a,b)
table(a,b)

## ------------------------------------------------------------------------
fas.chi2=function (x, y = NULL, correct = TRUE, p = rep(1/length(x), length(x)), 
          rescale.p = FALSE, simulate.p.value = FALSE, B = 2000) 
{
  DNAME <- deparse(substitute(x))
  if (!is.matrix(x) && !is.null(y)) {
    if (length(x) != length(y)) 
      stop("'x' and 'y' must have the same length")
    DNAME2 <- deparse(substitute(y))
    xname <- if (length(DNAME) > 1L || nchar(DNAME, "w") > 
                 30) 
      ""
    else DNAME
    yname <- if (length(DNAME2) > 1L || nchar(DNAME2, "w") > 
                 30) 
      ""
    else DNAME2
    OK <- complete.cases(x, y)
    x <- factor(x[OK])
    y <- factor(y[OK])
    if ((nlevels(x) < 2L) || (nlevels(y) < 2L)) 
      stop("'x' and 'y' must have at least 2 levels")
    x <- table(as.numeric(as.vector(x)), as.numeric(as.vector(y)))
    names(dimnames(x)) <- c(xname, yname)
    DNAME <- paste(paste(DNAME, collapse = "\n"), "and", 
                   paste(DNAME2, collapse = "\n"))
  }
  if (any(x < 0) || anyNA(x)) 
    stop("all entries of 'x' must be nonnegative and finite")
  if ((n <- sum(x)) == 0) 
    stop("at least one entry of 'x' must be positive")
  if (is.matrix(x)) {
    METHOD <- "Pearson's Chi-squared test"
    nr <- as.integer(nrow(x))
    nc <- as.integer(ncol(x))
    if (is.na(nr) || is.na(nc) || is.na(nr * nc)) 
      stop("invalid nrow(x) or ncol(x)", domain = NA)
    sr <- rowSums(x)
    sc <- colSums(x)
    E <- outer(sr, sc, "*")/n
    v <- function(r, c, n) c * r * (n - r) * (n - c)/n^3
    V <- outer(sr, sc, v, n)
    dimnames(E) <- dimnames(x)
    if (simulate.p.value && all(sr > 0) && all(sc > 0)) {
      setMETH()
      tmp <- .Call(C_chisq_sim, sr, sc, B, E)
      STATISTIC <- sum(sort((x - E)^2/E, decreasing = TRUE))
      PARAMETER <- NA
      PVAL <- (1 + sum(tmp >= almost.1 * STATISTIC))/(B + 
                                                        1)
    }
    else {
      if (simulate.p.value) 
        warning("cannot compute simulated p-value with zero marginals")
      if (correct && nrow(x) == 2L && ncol(x) == 2L) {
        YATES <- min(0.5, abs(x - E))
        if (YATES > 0) 
          METHOD <- paste(METHOD, "with Yates' continuity correction")
      }
      else YATES <- 0
      STATISTIC <- sum((abs(x - E) - YATES)^2/E)
      PARAMETER <- (nr - 1L) * (nc - 1L)
      PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
    }
  }
  else {
    if (length(dim(x)) > 2L) 
      stop("invalid 'x'")
    if (length(x) == 1L) 
      stop("'x' must at least have 2 elements")
    if (length(x) != length(p)) 
      stop("'x' and 'p' must have the same number of elements")
    if (any(p < 0)) 
      stop("probabilities must be non-negative.")
    if (abs(sum(p) - 1) > sqrt(.Machine$double.eps)) {
      if (rescale.p) 
        p <- p/sum(p)
      else stop("probabilities must sum to 1.")
    }
    METHOD <- "Chi-squared test for given probabilities"
    E <- n * p
    V <- n * p * (1 - p)
    STATISTIC <- sum((x - E)^2/E)
    names(E) <- names(x)
  }
  names(STATISTIC) <- "X-squared"
  names(PARAMETER) <- "df"
  return(STATISTIC)
}

## ------------------------------------------------------------------------
a=c(1,24,5,6,7,434)
b=c(325,645,75,7,23,352)
fas.chi2(a,b)

