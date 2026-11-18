# Traces the efficient frontier on one date, under the full mandate and under the long only constraint alone, by minimising variance for a grid of target returns.
Frontier_Calc <- function(Sigma, Mu, Cons, N_Points = 40){ # define Frontier_Calc, with inputs Sigma, Mu, Cons and N_Points

    n <- ncol(Sigma) # the number of assets
    Loose <- list(Amat = cbind(1, diag(n)), bvec = c(1, rep(0, n)), meq = 1) # long only weights that sum to one, with no other limits

    Trace <- function(Amat, bvec, Label){ # the frontier for one set of constraints
        w_min <- quadprog::solve.QP(2 * Sigma + diag(1e-10, n), rep(0, n), Amat, bvec, meq = 1)$solution # the minimum variance portfolio
        w_max <- quadprog::solve.QP(diag(1e-6, n), Mu, Amat, bvec, meq = 1)$solution # the highest expected return portfolio, with a tiny penalty that keeps the problem well posed
        Targets <- seq(sum(w_min * Mu), sum(w_max * Mu), length.out = N_Points) # target returns from the bottom to the top of the frontier
        map_df(Targets, function(m) { # repeat for each target
            w <- tryCatch(quadprog::solve.QP(2 * Sigma + diag(1e-10, n), rep(0, n), cbind(Mu, Amat), c(m, bvec), meq = 2)$solution, error = function(e) rep(NA, n)) # the lowest variance portfolio with that expected return
            tibble(Ret = sum(w * Mu) * 12, Vol = sqrt(as.numeric(t(w) %*% Sigma %*% w) * 12), Frontier = Label) # annualised return and volatility
        })
    }

    bind_rows(Trace(Cons$Amat, Cons$bvec, "Within the mandate"), Trace(Loose$Amat, Loose$bvec, "Long only, no other limits")) %>% filter(!is.na(Vol)) # both frontiers

}
