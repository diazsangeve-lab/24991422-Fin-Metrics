# Calculates the autocorrelation of returns and of squared returns for each series, with the Ljung-Box test of no autocorrelation, following the volatility clustering checks in Practical 6.
Acf_Calc <- function(df, Series, Lags = 25, Ret_Col = "dlogret"){

    df %>%
        filter(Name %in% Series, !is.na(.data[[Ret_Col]])) %>% # the chosen series with their returns
        group_by(Name) %>% # one set of autocorrelations per series
        group_modify(function(x, key){ # repeat for each series
            r <- x[[Ret_Col]] - mean(x[[Ret_Col]]) # demeaned returns
            map_df(c(Returns = "Returns", `Squared returns` = "Squared returns"), function(type){ # repeat for the returns and their squares
                y <- if( type == "Returns" ) r else r^2 # the series whose memory is measured
                tibble(Type = type, # which series this is
                       Lag = 1:Lags, # the lags
                       ACF = as.numeric(stats::acf(y, lag.max = Lags, plot = FALSE)$acf)[-1], # the autocorrelation at each lag, without lag zero
                       Bound = 1.96 / sqrt(length(y)), # the 95 percent band for no autocorrelation
                       LB_Stat = as.numeric(stats::Box.test(y, lag = 10, type = "Ljung-Box")$statistic), # the Ljung-Box statistic over ten lags
                       LB_p = stats::Box.test(y, lag = 10, type = "Ljung-Box")$p.value) # its p-value, where a small value rejects no autocorrelation
            })
        }) %>%
        ungroup() # remove the grouping again

}
