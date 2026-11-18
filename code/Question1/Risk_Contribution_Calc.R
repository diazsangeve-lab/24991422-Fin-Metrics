# Decomposes a portfolio's volatility into the contribution of each holding on one rebalance date, using the marginal contribution maths from Practical 2.
Risk_Contribution_Calc <- function(Holdings, Stock_Rets, Date_Sel, Lookback_Months = 12, Ret_Cap = 0.25){ # define Risk_Contribution_Calc, with inputs Holdings, Stock_Rets, Date_Sel, Lookback_Months and Ret_Cap

    # For stepping through uncomment:
    # Holdings <- SafeCore ; Stock_Rets <- Stock_Returns ; Date_Sel <- ymd(20260831) ; Lookback_Months <- 12

    W <- # build W through the steps below
        Holdings %>% # start from the holdings
        filter(date == Date_Sel) %>% # the holdings on the chosen rebalance date
        filter(Tickers %in% unique(Stock_Rets$Tickers)) %>% # only holdings with return data
        mutate(Weight = Weight / sum(Weight)) # rescale so that the weights sum to one

    R_wide <- # build R_wide through the steps below
        Stock_Rets %>% # start from the daily stock returns
        filter(Tickers %in% W$Tickers, date > Date_Sel %m-% months(Lookback_Months), date <= Date_Sel) %>% # the trailing window of returns for the current holdings
        mutate(Returns = pmax(pmin(Returns, Ret_Cap), -Ret_Cap)) %>% # cap extreme daily returns
        select(date, Tickers, Returns) %>% # keep only what the covariance needs
        spread(Tickers, Returns) %>% # one column per stock
        select(-date) # the covariance matrix needs numbers only

    if( nrow(R_wide) < 60 ) return(NULL) # skip the date if there is not enough history, so that the loop simply moves on

    Sigma <- cov(R_wide, use = "pairwise.complete.obs") * 252 # annualised covariance matrix, using all available pairs of returns
    Sigma[is.na(Sigma)] <- 0 # a stock with no overlapping history contributes no covariance
    Sigma <- as.matrix(Matrix::nearPD(Sigma)$mat) # make sure the matrix is positive definite, as in Practical 3

    W <- W %>% filter(Tickers %in% colnames(Sigma)) %>% arrange(match(Tickers, colnames(Sigma))) # put the weights in the same order as the covariance matrix
    W <- W %>% mutate(Weight = Weight / sum(Weight)) # rescale again, since a holding with no returns in the window has dropped out of the covariance matrix
    w <- W$Weight # the weight vector
    Sigma <- Sigma[W$Tickers, W$Tickers] # the matching covariance matrix

    sigma_p <- sqrt(as.numeric(t(w) %*% Sigma %*% w)) # the portfolio's annualised volatility
    mctr <- as.numeric(Sigma %*% w) / sigma_p # each stock's marginal contribution to risk
    cctr <- w * mctr # each stock's total contribution to risk, which sums to the portfolio volatility

    W %>% # start from W, built above
        mutate(Risk_Contrib = cctr, # the contribution in volatility units
               Risk_Share = cctr / sigma_p, # the contribution as a share of total portfolio volatility
               Port_Vol = sigma_p, # the portfolio volatility on this date
               date = Date_Sel) # the rebalance date

}
