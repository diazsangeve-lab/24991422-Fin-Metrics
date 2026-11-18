# Builds daily portfolio returns from monthly holdings and daily stock returns, using the safe portfolio return function from the practicals.
Holdings_Returns_Calc <- function(Holdings, Stock_Rets, Start_Date = lubridate::ymd(20150101), Ret_Cap = 0.25, Port_Name = "Portfolio"){ # define Holdings_Returns_Calc, with inputs Holdings, Stock_Rets, Start_Date, Ret_Cap and Port_Name

    # For stepping through uncomment:
    # Holdings <- SafeCore ; Stock_Rets <- Stock_Returns ; Start_Date <- ymd(20150101) ; Ret_Cap <- 0.25 ; Port_Name <- "SafeCore"

    if( !all(c("date", "Tickers", "Weight") %in% names(Holdings)) ) stop("... Holdings must have columns called 'date', 'Tickers' and 'Weight'") # stop early if the holdings are not in the expected shape
    if( !all(c("date", "Tickers", "Returns") %in% names(Stock_Rets)) ) stop("... Stock_Rets must have columns called 'date', 'Tickers' and 'Returns'") # stop early if the returns are not in the expected shape

    W <- # build W through the steps below
        Holdings %>% # start from the holdings
        filter(date >= Start_Date %m-% months(1)) %>% # start one month early so that the first month of the sample has weights
        filter(Tickers %in% unique(Stock_Rets$Tickers)) %>% # only keep holdings for which return data exists
        group_by(date) %>% # rescale within each rebalance date
        mutate(Weight = Weight / sum(Weight, na.rm = T)) %>% # assume the fund is fully invested, so the weights must sum to one
        ungroup() %>% # remove the grouping again
        select(date, Tickers, Weight) # keep only what the portfolio calculation needs

    R <- # build R through the steps below
        Stock_Rets %>% # start from the daily stock returns
        filter(Tickers %in% unique(W$Tickers), date > min(W$date)) %>% # only the stocks that the portfolio ever holds, from the first rebalance onwards
        mutate(Returns = pmax(pmin(Returns, Ret_Cap), -Ret_Cap)) # cap extreme daily returns, which are mostly data errors around corporate actions

    W_xts <- W %>% tbl2xts::tbl_xts(cols_to_xts = Weight, spread_by = Tickers, Colnames_Exact = T) # wide weights, one column per stock
    R_xts <- R %>% tbl2xts::tbl_xts(cols_to_xts = Returns, spread_by = Tickers, Colnames_Exact = T) # wide returns, one column per stock

    W_xts[is.na(W_xts)] <- 0 # a stock that is not held on a rebalance date has a zero weight
    R_xts[is.na(R_xts)] <- 0 # a missing return is set to zero, which only matters for stocks with no weight
    R_xts <- R_xts[, colnames(W_xts)] # make sure the columns of the two inputs line up exactly

    Port <- rmsfuns::Safe_Return.portfolio(R = R_xts, weights = W_xts, lag_weights = FALSE, geometric = TRUE) # daily portfolio returns, with the weights drifting between rebalances and each month end's holdings earning returns only from the next trading day

    Port %>% # start from Port, built above
        tbl2xts::xts_tbl() %>% # back to a tidy tibble
        rename(Returns = portfolio.returns) %>% # a clearer column name
        mutate(date = as.Date(date), Name = Port_Name) %>% # label the series
        filter(date >= Start_Date) # keep the sample from the chosen start date

}
