# Draws random long-only portfolios that satisfy every condition of the mandate on one rebalance date and records their expected risk and return, in the spirit of the random bounded weights of Practical 2.
Random_Portfolios_Calc <- function(R_Long, Meta, Date, N_Draws = 20000, Est_Start = lubridate::ymd(20100101), Min_Months = 60, Seed = 4321){

    set.seed(Seed) # make the draws reproducible
    Hist <- R_Long %>% select(date, Asset, Returns) %>% spread(Asset, Returns) %>% arrange(date) %>% filter(date <= Date) # returns known on the date
    Eligible <- names(Hist)[-1][colSums(!is.na(Hist[, -1])) >= Min_Months] # assets with enough history
    Est <- Cov_Mu_Estimate(Hist %>% filter(date >= Est_Start) %>% select(all_of(Eligible)) %>% as.matrix()) # the covariance matrix and expected returns
    M <- Meta %>% filter(Asset %in% Eligible) %>% arrange(match(Asset, Eligible)) # the meta data in the same order

    Draw_One <- function(){ # one random portfolio
        Bond <- runif(1, 0, 0.25); Comm <- runif(1, 0, 0.15); Eq <- 1 - Bond - Comm # random class budgets within the class limits, with equities taking the rest
        w <- numeric(nrow(M)) # start with zero weights
        for( g in c("Bonds", "Commodity", "Equity") ){ # spread each class budget across its members
            idx <- which(M$Class == g) # the members of the class
            e <- rexp(length(idx)) # random draws that become uniform shares once scaled
            w[idx] <- e / sum(e) * c(Bonds = Bond, Commodity = Comm, Equity = Eq)[[g]] # the members' weights
        }
        w # the portfolio
    }

    Draws <- replicate(N_Draws, Draw_One()) # many random portfolios, one per column
    Ok <- apply(Draws, 2, function(w) max(w) <= 0.35 && sum(w[M$Region == "Global"]) <= 0.60 && sum(w[M$Class == "Equity"]) <= 0.75) # keep those that meet the single asset, global and equity limits
    Draws <- Draws[, Ok, drop = FALSE] # the feasible portfolios

    tibble(Vol = sqrt(colSums(Draws * (Est$Sigma %*% Draws)) * 12), # each portfolio's expected annualised volatility
           Ret = as.numeric(Est$Mu %*% Draws) * 12) %>% # and expected annualised return
        mutate(Sharpe = Ret / Vol, Accepted = mean(Ok)) # return per unit of risk and the share of draws that met the mandate

}
