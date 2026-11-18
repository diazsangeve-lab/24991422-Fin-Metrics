# Calculates the monthly cross-sectional dispersion of benchmark constituent returns, weighting each stock by its index weight at the previous month end.
Market_Dispersion_Calc <- function(Holdings, Stock_Rets, Ret_Cap = 0.25){ # define Market_Dispersion_Calc, with inputs Holdings, Stock_Rets and Ret_Cap

    # For stepping through uncomment:
    # Holdings <- Monthly_Constituents %>% filter(Fund == "CappedSWIX") ; Stock_Rets <- Stock_Returns ; Ret_Cap <- 0.25

    Monthly_Stock <- # build Monthly_Stock through the steps below
        Stock_Rets %>% # start from the daily stock returns
        filter(!is.na(Returns)) %>% # drop missing daily returns
        mutate(Returns = pmin(pmax(Returns, -Ret_Cap), Ret_Cap), # winsorise implausible daily returns
               YM = format(date, "%Y%B")) %>% # a year and month label
        group_by(Tickers, YM) %>% # one return per stock per month
        summarise(R = prod(1 + Returns) - 1, .groups = "drop") # the compounded monthly return

    Holdings %>% # start from the holdings
        filter(!is.na(Weight)) %>% # drop holdings without a weight
        mutate(date = lubridate::add_with_rollback(lubridate::floor_date(date, "month"), lubridate::period(2, "month")) - 1, # the month end of the following month, in which the weights are held
               YM = format(date, "%Y%B")) %>% # its year and month label
        inner_join(Monthly_Stock, by = c("Tickers", "YM")) %>% # the stock's return over that month
        group_by(date, YM) %>% # one value per month
        mutate(w = Weight / sum(Weight)) %>% # rescale the weights of stocks with returns to sum to one
        summarise(Disp = sqrt(sum(w * (R - sum(w * R))^2)), .groups = "drop") # the weighted standard deviation of stock returns around the index return

}
