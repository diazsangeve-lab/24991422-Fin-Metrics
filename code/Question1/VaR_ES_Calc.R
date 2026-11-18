# Calculates daily Value at Risk and Expected Shortfall for each portfolio by the historical, Gaussian and modified methods, following the risk section of Practical 2.
VaR_ES_Calc <- function(df, p = 0.95){ # define VaR_ES_Calc, with inputs df and p

    df %>% # start from the input data
        group_by(Name) %>% # one set of measures per portfolio
        summarise(across(Returns, list( # the six measures, all reported as positive losses
            `Historical VaR` = ~-as.numeric(PerformanceAnalytics::VaR(.x, p = p, method = "historical")), # VaR read from the empirical return distribution, shown as a positive loss
            `Gaussian VaR` = ~-as.numeric(PerformanceAnalytics::VaR(.x, p = p, method = "gaussian")), # VaR under a normal distribution
            `Modified VaR` = ~-as.numeric(PerformanceAnalytics::VaR(.x, p = p, method = "modified")), # VaR adjusted for skewness and kurtosis through the Cornish-Fisher expansion
            `Historical ES` = ~-as.numeric(PerformanceAnalytics::ES(.x, p = p, method = "historical")), # the average loss beyond the historical VaR
            `Gaussian ES` = ~-as.numeric(PerformanceAnalytics::ES(.x, p = p, method = "gaussian")), # the average loss beyond the normal VaR
            `Modified ES` = ~-as.numeric(PerformanceAnalytics::ES(.x, p = p, method = "modified"))), .names = "{.fn}"), .groups = "drop") %>% # the average loss beyond the modified VaR, with each measure named after its function
        gather(Measure, Loss, -Name) %>% # long format, one row per measure
        mutate(Method = stringr::word(Measure, 1), Type = stringr::word(Measure, 2)) # split the method from the type of measure

}
