# Calculates rolling cross-sectional return dispersion of index constituents, weighted and unweighted, following the dispersion practical.
Dispersion_Calc <- function(Constituents, NSel = 21, Ret_Cap = 0.5, Max_Gap_Days = NULL){

    # For stepping through uncomment:
    # Constituents <- Local_Index_Constituents %>% filter(Index == "Capped SWIX") ; NSel <- 21 ; Ret_Cap <- 0.5

    if( !all(c("date", "Tickers", "Return", "weight") %in% names(Constituents)) ) stop("... Constituents must have columns called 'date', 'Tickers', 'Return' and 'weight'") # stop early if the data is not in the expected shape

    if( is.null(Max_Gap_Days) ) Max_Gap_Days <- ceiling(NSel * 1.6) # the longest calendar span a window of NSel trading days may cover before it is treated as broken

    Rolled <-
        Constituents %>%
        arrange(date) %>% # order by date, as lags and rolling windows are used
        mutate(Return = pmax(pmin(Return, Ret_Cap), -Ret_Cap)) %>% # winsorise extreme daily returns, which are mostly data errors
        group_by(Tickers) %>% # roll within each stock
        mutate(W_Start = lag(weight), # the weight at the start of the day, so that today's return is weighted by what was held going into it
               Roll_Ret = RcppRoll::roll_prod(1 + Return, n = NSel, fill = NA, align = "right") - 1, # the compounded return over the past NSel trading days
               Roll_W = RcppRoll::roll_mean(W_Start, n = NSel, fill = NA, align = "right"), # the average weight over the same window
               Span = as.numeric(date - lag(date, NSel - 1))) %>% # how many calendar days the window covers
        ungroup() %>% # remove the grouping again
        filter(!is.na(Roll_Ret), !is.na(Roll_W), Span <= Max_Gap_Days) # keep only complete, unbroken windows, so that a stock that left and rejoined the index is not compounded across the gap

    Rolled %>%
        group_by(date) %>% # one dispersion number per day
        mutate(Roll_W = Roll_W / sum(Roll_W)) %>% # rescale the weights over the stocks with a complete window
        summarise(N = n(), # the number of stocks in the calculation
                  Index_Ret = sum(Roll_W * Roll_Ret), # the weighted average return, which is the benchmark return over the window
                  Dispersion_W = sqrt(sum(Roll_W * (Roll_Ret - Index_Ret)^2)), # weighted cross-sectional volatility around the benchmark return
                  Dispersion_U = sd(Roll_Ret), # equally weighted cross-sectional volatility
                  .groups = "drop")

}
