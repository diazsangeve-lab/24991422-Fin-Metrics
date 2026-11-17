# Four long-only portfolio rules that all respect the mandate, namely the constrained equal weight portfolio, minimum variance, constrained risk parity and maximum Sharpe ratio.

# The portfolio closest to equal weights that satisfies every constraint, a naive baseline that uses no estimates at all.
Opt_Equal <- function(Sigma, Mu, Cons){
    n <- ncol(Sigma) # the number of assets
    quadprog::solve.QP(Dmat = diag(2, n), dvec = rep(2 / n, n), Amat = Cons$Amat, bvec = Cons$bvec, meq = Cons$meq)$solution # minimise the squared distance from equal weights
}

# The portfolio with the lowest volatility that satisfies every constraint, which uses only the covariance matrix.
Opt_MinVar <- function(Sigma, Mu, Cons){
    n <- ncol(Sigma) # the number of assets
    quadprog::solve.QP(Dmat = 2 * Sigma + diag(1e-10, n), dvec = rep(0, n), Amat = Cons$Amat, bvec = Cons$bvec, meq = Cons$meq)$solution # minimise the portfolio variance
}

# The portfolio with the highest ratio of expected return to volatility, solved through the standard rescaling that turns it into a quadratic programme, with every cap rewritten relative to the scale.
Opt_MaxSharpe <- function(Sigma, Mu, Cons){
    n <- ncol(Sigma) # the number of assets
    if( max(Mu) <= 0 ) return(Opt_MinVar(Sigma, Mu, Cons)) # with no positive expected return the ratio is not defined, so fall back to minimum variance
    Amat <- cbind(Mu, # expected return of the scaled weights equals one
                  diag(n), # scaled weights are at least zero
                  Cons$Single_Cap - diag(n), # each scaled weight is at most the cap times the scale
                  matrix(rep(Cons$Class_Caps, each = n), n) - Cons$Class_Mat, # each class is at most its cap times the scale
                  Cons$Global_Cap - Cons$Global_Vec) # offshore assets are at most the global cap times the scale
    y <- quadprog::solve.QP(Dmat = 2 * Sigma + diag(1e-10, n), dvec = rep(0, n), Amat = Amat, bvec = c(1, rep(0, ncol(Amat) - 1)), meq = 1)$solution # minimise the variance of the scaled weights
    pmax(y, 0) / sum(pmax(y, 0)) # rescale back to weights that sum to one
}

# The portfolio whose risk contributions are as close to equal as the constraints allow, found by numerical optimisation from the minimum variance starting point.
Opt_RiskParity <- function(Sigma, Mu, Cons){
    n <- ncol(Sigma) # the number of assets
    Obj <- function(w) { RC <- w * (Sigma %*% w) / as.numeric(t(w) %*% Sigma %*% w); sum((RC - 1 / n)^2) * 1e3 } # the squared distance of each asset's share of risk from an equal share
    Ineq <- function(w) c(as.numeric(t(Cons$Class_Mat) %*% w), sum(Cons$Global_Vec * w)) # the class and global exposures
    w0 <- Opt_Equal(Sigma, Mu, Cons) # start from the constrained equal weight portfolio, which is feasible
    Fit <- Rsolnp::solnp(pars = w0, fun = Obj, eqfun = function(w) sum(w), eqB = 1, ineqfun = Ineq, ineqLB = rep(0, length(Cons$Class_Caps) + 1), ineqUB = c(Cons$Class_Caps, Cons$Global_Cap), LB = rep(0, n), UB = rep(Cons$Single_Cap, n), control = list(trace = 0)) # minimise the objective subject to every constraint
    w <- pmax(Fit$pars, 0) # remove tiny negative values left by the solver
    w / sum(w) # make sure the weights sum to one
}
