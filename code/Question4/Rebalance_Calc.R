# Calculates the weights of each portfolio rule on every rebalance date, using all monthly returns from the estimation start up to that date and only assets with enough history, and records the ex ante risk contributions.
Rebalance_Calc <- function(R_Long, Meta, Rebal_Dates, Strategies, Est_Start = lubridate::ymd(20100101), Min_Months = 60, Min_Years = 10, ...){ # define Rebalance_Calc, with inputs R_Long, Meta, Rebal_Dates, Strategies, Est_Start, Min_Months, Min_Years and ...

    # For stepping through uncomment:
    # R_Long <- Asset_Rets ; Meta <- Meta ; Rebal_Dates <- Rebal_Dates ; Strategies <- Strategies ; Est_Start <- ymd(20100101) ; Min_Months <- 60 ; Min_Years <- 10

    if( as.numeric(min(Rebal_Dates) - Est_Start) / 365.25 < Min_Years - 0.01 ) stop(glue::glue("... the first rebalance date must leave a look-back of at least {Min_Years} years")) # enforce the minimum look-back

    R_Wide <- R_Long %>% select(date, Asset, Returns) %>% spread(Asset, Returns) %>% arrange(date) # one column per asset

    map_df(Rebal_Dates, function(d) { # repeat for each rebalance date

        Hist <- R_Wide %>% filter(date <= d) # all returns known on the rebalance date
        Eligible <- names(Hist)[-1][colSums(!is.na(Hist[, -1])) >= Min_Months] # assets with at least the minimum number of monthly returns
        Window <- Hist %>% filter(date >= Est_Start) %>% select(all_of(Eligible)) %>% as.matrix() # the estimation window from the start date, for eligible assets
        Est <- Cov_Mu_Estimate(Window, ...) # the covariance matrix and expected returns
        Meta_Elig <- Meta %>% filter(Asset %in% Eligible) %>% arrange(match(Asset, Eligible)) # the meta data in the same order as the matrix
        Cons <- Constraint_Build(Meta_Elig) # the mandate's constraints for these assets

        imap_dfr(Strategies, function(Fn, Nm) { # repeat for each portfolio rule
            w <- Fn(Est$Sigma, Est$Mu, Cons) # the rule's weights
            Port_Var <- as.numeric(t(w) %*% Est$Sigma %*% w) # the ex ante portfolio variance
            tibble(date = d, Strategy = Nm, Asset = Eligible, Weight = w, # the weights
                   Risk_Share = w * as.numeric(Est$Sigma %*% w) / Port_Var, # each asset's share of portfolio risk
                   ExAnte_Vol = sqrt(Port_Var * 12), # the ex ante annualised volatility
                   ExAnte_Ret = sum(w * Est$Mu) * 12) # the ex ante annualised expected return
        })
    }) %>% # close the inner function and pass its results on
        mutate(Weight = ifelse(abs(Weight) < 1e-6, 0, Weight)) %>% # treat numerical dust as zero
        group_by(date, Strategy) %>% mutate(Weight = Weight / sum(Weight)) %>% ungroup() %>% # rescale so that the weights still sum to exactly one
        left_join(Meta %>% select(Asset, Class, Region), by = "Asset") # attach each asset's class and region

}
