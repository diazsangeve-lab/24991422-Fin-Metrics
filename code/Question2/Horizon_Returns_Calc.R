# Calculates annualised returns over trailing horizons ending on the last date, following the annualised return comparison in Practical 1.
Horizon_Returns_Calc <- function(Monthly, Horizons = c(`6 months` = 6, `1 year` = 12, `3 years` = 36, `5 years` = 60, `10 years` = 120)){ # define Horizon_Returns_Calc, with inputs Monthly and Horizons

    End <- max(Monthly$date) # the last month in the data

    imap_dfr(Horizons, function(m, h){ # repeat for each horizon
        Monthly %>% # start from the monthly returns
            arrange(date) %>% # order by date
            group_by(Name) %>% # one value per series
            slice_tail(n = m) %>% # the last m months
            filter(n() == m, max(date) == End) %>% # only series with the full horizon that run to the last month
            summarise(Ret = if( m <= 12 ) prod(1 + Returns) - 1 else prod(1 + Returns)^(12 / m) - 1, .groups = "drop") %>% # cumulative for a year or less and annualised beyond
            mutate(Horizon = h, Months = m) # label the horizon
    }) %>% # close the inner function and pass its results on
        mutate(Horizon = factor(Horizon, levels = names(Horizons))) # keep the horizons in order

}
