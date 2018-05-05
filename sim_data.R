# function to simulate bernoulli GLMM with random intercepts
sim_data <- function(K=100, nk=5, beta, tau, theta, sigma) {
    gamma0 <- rnorm(K, 0, tau)
    gamma0s <- rep(gamma0, each=nk)
    x <- rnorm(K * nk, 0, sigma)
    p <- plogis(beta[1] + beta[2] * x + gamma0s)
    y <- rbinom(K * nk, 1, p)

    cluster <- rep(1:K, each=nk)
    ds <- data.frame(x, y, cluster)
    ds$fcluster <- as.factor(cluster) # must be factor for mcemGLMM
    return(ds)
}
