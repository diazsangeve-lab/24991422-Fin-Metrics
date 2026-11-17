# Traces the efficient frontier on every rebalance date, within the mandate and under the long only constraint alone, and records each asset's expected return and volatility on those dates.
Frontier_Path_Calc <- function(R_Long, Meta, Rebal_Dates, Est_Start = lubridate::ymd(20100101), Min_Months = 60, N_Points = 30){

    R_Wide <- R_Long %>% select(date, Asset, Returns) %>% spread(Asset, Returns) %>% arrange(date) # one column per asset

    Out <- map(Rebal_Dates, function(d) { # repeat for each rebalance date
        Hist <- R_Wide %>% filter(date <= d) # all returns known on the date
        Eligible <- names(Hist)[-1][colSums(!is.na(Hist[, -1])) >= Min_Months] # assets with enough history
        Est <- Cov_Mu_Estimate(Hist %>% filter(date >= Est_Start) %>% select(all_of(Eligible)) %>% as.matrix()) # the covariance matrix and expected returns
        Cons <- Constraint_Build(Meta %>% filter(Asset %in% Eligible) %>% arrange(match(Asset, Eligible))) # the mandate's constraints
        list(Frontiers = Frontier_Calc(Est$Sigma, Est$Mu, Cons, N_Points) %>% mutate(date = d), # both frontiers on the date
             Assets = tibble(date = d, Asset = Eligible, Ret = Est$Mu * 12, Vol = sqrt(diag(Est$Sigma) * 12))) # each asset's annualised expected return and volatility
    })

    list(Frontiers = map_df(Out, "Frontiers"), Assets = map_df(Out, "Assets")) # stack the dates

}
