# Traces the efficient frontier within the mandate on one rebalance date and records the weights of every frontier portfolio, following the frontier construction in Practical 3 bonus 2.
Frontier_Weights_Calc <- function(R_Long, Meta, Date, Est_Start = lubridate::ymd(20100101), Min_Months = 60, N_Points = 40){

    Hist <- R_Long %>% select(date, Asset, Returns) %>% spread(Asset, Returns) %>% arrange(date) %>% filter(date <= Date) # returns known on the date
    Eligible <- names(Hist)[-1][colSums(!is.na(Hist[, -1])) >= Min_Months] # assets with enough history
    Est <- Cov_Mu_Estimate(Hist %>% filter(date >= Est_Start) %>% select(all_of(Eligible)) %>% as.matrix()) # the covariance matrix and expected returns
    Cons <- Constraint_Build(Meta %>% filter(Asset %in% Eligible) %>% arrange(match(Asset, Eligible))) # the mandate's constraints
    n <- length(Eligible) # the number of assets

    w_min <- quadprog::solve.QP(2 * Est$Sigma + diag(1e-10, n), rep(0, n), Cons$Amat, Cons$bvec, meq = 1)$solution # the minimum variance portfolio
    w_max <- quadprog::solve.QP(diag(1e-6, n), Est$Mu, Cons$Amat, Cons$bvec, meq = 1)$solution # the highest expected return portfolio
    Targets <- seq(sum(w_min * Est$Mu), sum(w_max * Est$Mu), length.out = N_Points) # target returns along the frontier

    map_df(Targets, function(m){ # repeat for each target
        w <- quadprog::solve.QP(2 * Est$Sigma + diag(1e-10, n), rep(0, n), cbind(Est$Mu, Cons$Amat), c(m, Cons$bvec), meq = 2)$solution # the lowest variance portfolio with that expected return
        tibble(Vol = sqrt(as.numeric(t(w) %*% Est$Sigma %*% w) * 12), Ret = m * 12, Asset = Eligible, Weight = pmax(w, 0)) # its annualised risk, return and weights
    }) %>%
        group_by(Vol) %>% mutate(Weight = Weight / sum(Weight)) %>% ungroup() # rescale away numerical dust

}
