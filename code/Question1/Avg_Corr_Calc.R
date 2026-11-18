# Calculates the average pairwise correlation and the cap-weighted implied correlation of the largest index constituents over a trailing window on chosen dates.
Avg_Corr_Calc <- function(Constituents, Dates, Lookback = 63, Top_N = 40, Ret_Cap = 0.5, Min_Coverage = 0.8){ # define Avg_Corr_Calc, with inputs Constituents, Dates, Lookback, Top_N, Ret_Cap and Min_Coverage

    # For stepping through uncomment:
    # Constituents <- SWIX_Cons ; Dates <- Month_Ends ; Lookback <- 63 ; Top_N <- 40

    Cons <- Constituents %>% mutate(Return = pmax(pmin(Return, Ret_Cap), -Ret_Cap)) # winsorise extreme daily returns
    All_Days <- sort(unique(Cons$date)) # every trading day, used to find the start of each window

    Dates %>% # start from the calculation dates
        map_df(function(d){ # repeat for every chosen date
            Window_Days <- tail(All_Days[All_Days <= d], Lookback) # the trailing Lookback trading days ending on the chosen date
            if( length(Window_Days) < Lookback ) return(NULL) # skip the date if the window is not yet full

            Top <- Cons %>% filter(date == max(Window_Days)) %>% slice_max(weight, n = Top_N, with_ties = FALSE) # the largest constituents on the last day of the window
            w <- Top$weight / sum(Top$weight) # their weights, rescaled to sum to one

            R <- Cons %>% filter(date %in% Window_Days, Tickers %in% Top$Tickers) %>% select(date, Tickers, Return) %>% spread(Tickers, Return) %>% select(all_of(Top$Tickers)) # daily returns in the window, one column per stock in weight order
            Keep <- colMeans(!is.na(R)) >= Min_Coverage # stocks with enough history in the window
            R <- R[, Keep, drop = FALSE] ; w <- w[Keep] / sum(w[Keep]) # drop thin stocks and rescale the weights

            C <- cor(R, use = "pairwise.complete.obs") # the pairwise correlation matrix
            S <- apply(R, 2, sd, na.rm = TRUE) * sqrt(252) # each stock's annualised volatility
            Sigma <- C * outer(S, S) # the annualised covariance matrix built from the two
            Port_Var <- as.numeric(t(w) %*% Sigma %*% w) # the variance of the cap-weighted basket
            Own_Var <- sum(w^2 * S^2) # the part of that variance that comes from each stock on its own

            tibble(date = d, # the chosen date
                   N = ncol(R), # the number of stocks used
                   Avg_Corr = mean(C[upper.tri(C)], na.rm = TRUE), # the simple average of all pairwise correlations
                   Implied_Corr = (Port_Var - Own_Var) / (sum(w * S)^2 - Own_Var), # the single correlation that, applied to every pair, reproduces the basket's variance, which weights large stocks more
                   Avg_Vol = sum(w * S), # the weighted average stock volatility
                   Basket_Vol = sqrt(Port_Var), # the volatility of the basket itself
                   Avg_Var = sum(w * S^2), # the weighted average stock variance, the total risk an investor in single stocks faces
                   Common_Var = Port_Var, # the part of that variance shared with the basket, which no stock picker can avoid
                   Dispersion_Var = max(sum(w * S^2) - Port_Var, 0)) # the remainder, which is exactly the weighted cross-sectional variance and therefore the alpha opportunity
        })

}
