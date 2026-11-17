# Builds raw momentum and realised volatility measures for every stock at each chosen month end, purely from price behaviour.
Own_Factor_Scores <- function(Stock_Rets, Dates, Mom_Months = 6, Skip_Months = 1, Vol_Months = 6, Ret_Cap = 0.25, Min_Coverage = 0.8){

    # For stepping through uncomment:
    # Stock_Rets <- Stock_Returns ; Dates <- sort(unique(Factor_Scores$date)) ; Mom_Months <- 6 ; Skip_Months <- 1 ; Vol_Months <- 6

    if( !all(c("date", "Tickers", "Returns") %in% names(Stock_Rets)) ) stop("... Stock_Rets must have columns called 'date', 'Tickers' and 'Returns'") # stop early if the returns are not in the expected shape

    Rets <- Stock_Rets %>% mutate(Returns = pmax(pmin(Returns, Ret_Cap), -Ret_Cap)) # cap extreme daily returns so that data errors do not drive the scores

    Dates %>%
        map_df(function(d){ # repeat the calculation for every month end
            Rets %>%
                filter(date > d %m-% months(max(Mom_Months, Vol_Months)), date <= d) %>% # the longest window needed, ending on the month end
                group_by(Tickers) %>% # one score per stock
                summarise(N = n(), # the number of trading days the stock has in the window
                          Mom_Raw = prod(1 + Returns[date > d %m-% months(Mom_Months) & date <= d %m-% months(Skip_Months)]) - 1, # the return over the momentum window, skipping the most recent month to avoid short term reversal
                          Vol_Raw = sd(Returns[date > d %m-% months(Vol_Months)]) * sqrt(252), # the annualised realised volatility over the volatility window
                          .groups = "drop") %>%
                filter(N >= Min_Coverage * max(N)) %>% # drop stocks that traded on too few days to give a reliable score
                mutate(date = d) %>% # label the month end
                select(date, Tickers, Mom_Raw, Vol_Raw) # keep only the scores
        })

}
