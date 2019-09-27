EM_mixture <- function(X, Q,
                       init.cl = kmeans(X,Q)$cl, max.iter=100, eps=1e-5) {
    n <- length(X); tau <- matrix(0,n,Q); tau[cbind(1:n,init.cl)] <- 1
    loglik  <- vector("numeric", max.iter)
    Eloglik <- vector("numeric", max.iter)
    iter <- 0; cond <- FALSE

    while (!cond) {
        iter <- iter + 1
        ## M step
        theta <- M_step(X, tau)
        ## E step
        res_Estep <- E_step(X, theta)
        tau <- res_Estep$tau
        ## check consistency
        loglik[iter]  <- res_Estep$loglik
        Eloglik[iter] <- get_cloglik(X, tau, theta)
        if (iter > 1)
            cond <- (iter>=max.iter) | Eloglik[iter]-Eloglik[iter-1] < eps
    }

    res <- list(alpha = theta$alpha,  mu = theta$mu,  sigma = theta$sigma,
                tau   = tau, cl = apply(tau, 1, which.max),
                Eloglik = Eloglik[1:iter],
                loglik  = loglik[1:iter])
    res
}
