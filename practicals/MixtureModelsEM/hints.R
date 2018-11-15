EM.mixture <- function(X, Q, init.cl=kmeans(X,Q)$cl, max.iter=100, eps=1e-5) {
    ## INITIALIZATIONS
    n <- length(X)
    tau <- matrix(0,n,Q)
    tau[cbind(1:n,init.cl)] <- 1
    cloglik <- vector("numeric", max.iter)
    iter <- 0; cond <- FALSE

    while (!cond) {
        iter <- iter + 1
        ## M step
        alpha  <- 
        mu     <- 
        sigma  <- 
        ## E step    
        [...]
        tau <- 
        Z <- 1*(tau >= .5) # sloppy when clusters are not well separated
        ## check consistency
        cloglik[iter] <- get.cloglik(X, Z, alpha, mu, sigma)
        if (iter > 1) 
            cond <- (iter>=max.iter) | abs(cloglik[iter]-cloglik[iter-1]) < eps        
    }

    return(list(alpha = alpha , 
                mu    = mu    , 
                sigma = sigma, 
                tau   = tau   ,
                cl    = apply(Z,1,which.max),
                cloglik = cloglik[1:iter],
                loglik  = loglik[1:iter]))
}
