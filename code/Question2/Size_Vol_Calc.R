# Calculates, on each month end, the weighted average realised volatility of an index's constituents and the volatility of the index built from them, using trailing daily stock returns.
Size_Vol_Calc <- function(Holdings, Stock_Rets, Dates, Lookback = 63, Ret_Cap = 0.25, Min_Coverage = 0.8){

    # For stepping through uncomment:
    # Holdings <- Size_Hold ; Stock_Rets <- Stock_Returns ; Dates <- Size_Dates ; Lookback <- 63

    Wide <-
        Stock_Rets %>%
        filter(Tickers %in% unique(Holdings$Tickers)) %>% # only stocks that ever appear in the indexes
        mutate(Returns = pmax(pmin(Returns, Ret_Cap), -Ret_Cap)) %>% # cap extreme daily returns
        select(date, Tickers, Returns) %>% # keep only what the calculation needs
        spread(Tickers, Returns) %>% # one column per stock, built once so that each window is a fast row subset
        arrange(date) # order by date
    Days <- Wide$date # the trading days, matching the rows of the matrix
    R_All <- as.matrix(Wide[, -1]) # the return matrix itself

    tidyr::expand_grid(Fund = unique(Holdings$Fund), date = Dates) %>% # every index and month end
        pmap_df(function(Fund, date){ # repeat the calculation for each pair
            Rows <- tail(which(Days <= date), Lookback) # the rows of the trailing window ending on the month end
            if( length(Rows) < Lookback ) return(NULL) # skip the date if the window is not yet full
            W <- Holdings %>% filter(Fund == !!Fund, date == !!date, Tickers %in% colnames(R_All)) # the index's holdings on the month end that have return data
            if( nrow(W) == 0 ) return(NULL) # skip if the index has no holdings on that date
            R <- R_All[Rows, W$Tickers, drop = FALSE] # daily returns of the holdings in the window
            Keep <- colMeans(!is.na(R)) >= Min_Coverage # stocks with enough history in the window
            if( sum(W$Weight[Keep]) < Min_Coverage ) return(NULL) # skip the month if the stocks with enough data hold less than the minimum share of the index weight
            R <- R[, Keep, drop = FALSE] ; w <- W$Weight[Keep] / sum(W$Weight[Keep]) # drop thin stocks and rescale the weights
            S <- apply(R, 2, sd, na.rm = TRUE) * sqrt(252) # each stock's annualised realised volatility
            Sigma <- cov(R, use = "pairwise.complete.obs") * 252 # the annualised covariance matrix
            Sigma[is.na(Sigma)] <- 0 # pairs with no overlapping history contribute no covariance
            Index_Vol <- sqrt(max(as.numeric(t(w) %*% Sigma %*% w), 0)) # the volatility of the index built from these weights
            tibble(Fund = Fund, date = date, # the index and month end
                   Avg_Vol = sum(w * S), # the weighted average constituent volatility
                   Median_Vol = median(S), # the volatility of the typical constituent, ignoring weights
                   Index_Vol = Index_Vol, # the volatility of the index itself
                   Div_Ratio = sum(w * S) / Index_Vol) # the diversification ratio, how many times more volatile the average holding is than the index
        })

}
