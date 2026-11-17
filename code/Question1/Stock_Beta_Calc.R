# Calculates each stock's market beta, downside beta and volatility over a trailing window, used to diagnose what the third party scores measure.
Stock_Beta_Calc <- function(Stock_Rets, Mkt_Rets, Dates, Months = 12, Ret_Cap = 0.25, Min_Days = 180){

    if( !all(c("date", "Mkt") %in% names(Mkt_Rets)) ) stop("... Mkt_Rets must have columns called 'date' and 'Mkt'") # stop early if the market returns are not in the expected shape

    Rets <-
        Stock_Rets %>%
        mutate(Returns = pmax(pmin(Returns, Ret_Cap), -Ret_Cap)) %>% # cap extreme daily returns
        inner_join(Mkt_Rets, by = "date") # attach the market return on each day

    Dates %>%
        map_df(function(d){ # repeat for every month end
            Rets %>%
                filter(date > d %m-% months(Months), date <= d) %>% # the trailing window
                group_by(Tickers) %>% # one estimate per stock
                filter(n() >= Min_Days) %>% # require enough history for a stable estimate
                summarise(Beta = cov(Returns, Mkt) / var(Mkt), # sensitivity to the market on all days
                          Downside_Beta = cov(Returns[Mkt < 0], Mkt[Mkt < 0]) / var(Mkt[Mkt < 0]), # sensitivity to the market on days when it falls
                          Vol = sd(Returns) * sqrt(252), # annualised realised volatility
                          .groups = "drop") %>%
                mutate(date = d) # label the month end
        })

}
