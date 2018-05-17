library(igraph)

rNetwork <- function(n, pi, alpha = c(1)) {
  sizes <- rowSums(rmultinom(n, 1, prob = alpha))
  G <- sample_sbm(n, pi.comm, block.sizes = sizes)
  vertex_attr(G, "membership") <- rep(1:length(alpha), sizes)
  G
}

entropy <- function(distr) {
  ## handle x log(x) = 0 when x = 0
  - sum(distr * log(distr + 1*(distr == 0)))
}

VEM_SBM <- function(G, Q, cl.init, nstart=5, eps=1e-5, maxIter=50){
  
  stopifnot(is.igraph(G))
  
  ## Initialization
  n <- gorder(G)
  J <- vector("numeric", maxIter)
  zero <- .Machine$double.eps
  mat_edges     <- as_adj(G, sparse=FALSE)  
  mat_edges_bar <- (1 - mat_edges); diag(mat_edges_bar) <- 0 
  
  ### variational lower bound
  get_J <- function(theta, tau){
    J <- sum( tau %*% log(theta$alpha)) +
      .5 * sum( mat_edges     * (tau %*% log( theta$pi ) %*% t(tau))  +
                mat_edges_bar * (tau %*% log(1-theta$pi) %*% t(tau))) +
        entropy(tau)
    J
  }
  ### M step: update theta (pi and alpha)
  M_step <- function(tau){
    alpha = colMeans(tau)
    pi    = (t(tau) %*% mat_edges %*% tau) / (t(tau) %*% (1-diag(n)) %*% tau)
    alpha[alpha < zero] <- zero
    pi[pi < zero] <- zero
    pi[pi > 1- zero] <- 1- zero
    list(pi = pi, alpha = alpha)
  }
  ### E step: update the clustering parameters (tau)
  E_step <- function(theta, tau){
    alpha <- theta$alpha
    pi <- theta$pi
    tau <- exp( matrix(log(alpha), n, Q, byrow = TRUE) + 
                mat_edges     %*% (tau %*% t(log(pi))  ) +
                mat_edges_bar %*% (tau %*% t(log(1-pi))))
    tau <- tau/rowSums(tau)
    tau
  }
  
  VEM <- function(tau) {
    cond <- FALSE; iter <- 0
    while(!cond){
      iter <- iter +1
      theta <- M_step(tau)
      # E step
      tau <- E_step(theta, tau) # M step
      
      J[iter] <- get_J(theta, tau) # assess convergence
      if (iter > 1)
        cond <- (iter > maxIter) | (abs((J[iter] - J[iter-1])) < eps)
    }
    return(list(theta = theta, tau = tau, J = J[1:iter]))
  }
  
  
  if (missing(cl.init)) {
    out <- replicate(nstart, {
      cl0 <- sample(1:Q, n, replace = TRUE)
      tau <- matrix(0,n,Q); tau[cbind(1:n, cl0)] <- 1
      VEM(tau)
    })
    best <- out[,which.max(sapply(out['J',], max))]
  } else {
    tau <- matrix(0,n,Q); tau[cbind(1:n, cl.init)] <- 1
    best <- VEM(tau)
  }
  
  vBIC <- best$J[length(best$J)] - .5*(Q*(Q+1)/2)*log(n*(n-1)/2) + (Q-1)*log(n)
  vICL <- vBIC - entropy(best$tau)  
  return(list(theta = best$theta,
              tau = best$tau, membership = apply(best$tau, 1, which.max),
              J = best$J, vICL = vICL, vBIC= vBIC))
}    


############# SIMPLE EXAMPLE ON A COMMUNITY NETWORK
p_in <- 0.5
p_out <- 0.01
pi.comm <- matrix(p_out, 3, 3); diag(pi.comm) <- p_in
n <- 100
alpha <- c(.5, .25, .25)

G <- rNetwork(n, pi, alpha)
plot(G, vertex.color=V(G)$membership, vertex.label=NA)

## run VEM for SBM for various number of clusters
seq.Q <- 2:6
library(pbmcapply)
VEM_out <- pbmclapply(seq.Q, function(Q) {
  VEM_SBM(G, Q)
})
vBIC <- sapply(VEM_out, function(model) model$vBIC)
vICL <- sapply(VEM_out, function(model) model$vICL)

plot(seq.Q,vBIC, type="l", col="red")
lines(seq.Q,vICL, col="blue")
legend("bottomright", legend=c("vBIC","vICL"), lty=c(1,1), col=c("blue", "red"))
best.model <- VEM_out[[which.max(vICL)]]
aricode::ARI(best.model$membership, V(G)$membership) # yes !

############# ANALYSIS of the FRENCH POLITICAL BLOGSPHERE
library(sand)
fblog <- upgrade_graph(fblog)
seq.Q <- 2:15
library(pbmcapply)
VEM_out <- pbmclapply(seq.Q, function(Q) {
  VEM_SBM(fblog, Q)
})
vBIC <- sapply(VEM_out, function(model) model$vBIC)
vICL <- sapply(VEM_out, function(model) model$vICL)
plot(seq.Q,vBIC, type="l", col="red")
lines(seq.Q,vICL, col="blue")
legend("bottomright", legend=c("vBIC","vICL"), lty=c(1,1), col=c("blue", "red"))
best.model.fblog <- VEM_out[[which.max(vICL)]]
aricode::ARI(best.model.fblog$membership, V(fblog)$PolParty) # yes !

plot(fblog, vertex.color = best.model.fblog$membership, vertex.label=NA)
