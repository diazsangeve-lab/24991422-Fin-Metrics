# Calculates the annualised return, volatility and maximum drawdown of every series in each of a set of sub-periods, from monthly returns.
Size_Period_Calc <- function(df, Periods){

    if( !all(c("date", "Name", "Returns") %in% names(df)) ) stop("... df must have columns called 'date', 'Name' and 'Returns'") # stop early if the returns are not in the expected shape
    if( !all(c("Period", "Start", "End") %in% names(Periods)) ) stop("... Periods must have columns called 'Period', 'Start' and 'End'") # stop early if the periods are not defined properly

    Periods %>%
        pmap_df(function(Period, Start, End){ # repeat for each sub-period
            df %>%
                filter(date >= Start, date <= End) %>% # the monthly returns in the sub-period
                group_by(Name) %>% # one row per series
                summarise(Ret = prod(1 + Returns)^(12 / n()) - 1, # the annualised compounded return
                          Vol = sd(Returns) * sqrt(12), # the annualised volatility
                          MDD = as.numeric(PerformanceAnalytics::maxDrawdown(xts::xts(Returns, order.by = date))), # the deepest peak to trough loss
                          .groups = "drop") %>%
                mutate(Period = Period) # label the sub-period
        }) %>%
        mutate(Period = factor(Period, levels = Periods$Period)) %>% # keep the sub-periods in time order
        arrange(Name, Period) # order each series' rows in time

}
