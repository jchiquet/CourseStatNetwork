rm(list=ls())

get.cloglik <- function(X, Z, alpha, mu, sigma) {
    n <- length(X); Q <- length(alpha)
    x.scaled <- sweep(sweep(matrix(X,n,Q), 2,  mu, "-"), 2, sigma, "/")
    return(sum(Z * ( log(alpha) - log (sigma) - .5 * (log(2*pi) + x.scaled^2))))
}

EM.mixture <- function(X, Q, init.cl=kmeans(X,Q)$cl, max.iter=100, eps=1e-5) {

    ## INITIALIZATIONS
    n <- length(X)
    tau <- matrix(0,n,Q)
    tau[cbind(1:n,init.cl)] <- 1
    loglik  <- vector("numeric", max.iter)
    cloglik <- vector("numeric", max.iter)
    iter <- 0
    cond <- FALSE

    while (!cond) {
        iter <- iter + 1

        ## M step
        alpha  <- colMeans(tau)
        mu     <- colMeans(tau * matrix(X,n,Q)) / alpha
        sigma  <- sqrt(colMeans(tau *sweep(matrix(X,n,Q), 2,  mu, "-")^2) / alpha)
        
        ## E step    
        prob <- sapply(1:Q, function(q) alpha[q] * dnorm(x, mu[q], sigma[q]))
        likelihoods <- rowSums(prob)
        tau <-  prob / likelihoods 
        Z <- 1*(tau >= .5)

        loglik[iter]  <- sum(log(likelihoods))
        cloglik[iter] <- get.cloglik(X, Z, alpha, mu, sigma)
        if (iter > 1) 
            cond <- (iter >= max.iter) | cloglik[iter] <= cloglik[iter-1] + eps
        
    }
    return(list(alpha = alpha , 
                mu    = mu    , 
                sigma = sigma, 
                tau   = tau   ,
                cl    = apply(Z,1,which.max),
                cloglik = cloglik[1:iter],
                loglik  = loglik[1:iter]))
}
 

## checking against mixtools results
library(mixtools)

mu1 <- 5   ; sigma1 <- 1; n1 <- 100
mu2 <- 10  ; sigma2 <- 1; n2 <- 200
mu3 <- 2.5 ; sigma3 <- 2; n3 <- 50
mu4 <- 20  ; sigma4 <- 4; n4 <- 100
x <- sample(c(rnorm(n1,mu1,sigma1),rnorm(n2,mu2,sigma2),
              rnorm(n3,mu3,sigma3),rnorm(n4,mu4,sigma4)))
n <- length(x)

seq.Q <- 2:10

crit.EM <- sapply(seq.Q, function(Q) {
    out <- EM.mixture(x, Q)
    df <- Q-1 + 2 * Q
    return(c(BIC = -2*tail(out$loglik,1)  + log(n)*df,
             ICL = -2*tail(out$cloglik,1) + log(n)*df ))
})

EM.Q2 <- EM.mixture(x, Q=2)

EM.ref <- normalmixEM(x)
EM.ref$mu
EM.ref$sigma
EM.ref$lambda
