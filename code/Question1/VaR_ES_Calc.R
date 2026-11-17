# Calculates daily Value at Risk and Expected Shortfall for each portfolio by the historical, Gaussian and modified methods, following the risk section of Practical 2.
VaR_ES_Calc <- function(df, p = 0.95){

    df %>%
        group_by(Name) %>% # one set of measures per portfolio
        summarise(across(Returns, list( # the six measures, all reported as positive losses
            `Historical VaR` = ~-as.numeric(PerformanceAnalytics::VaR(.x, p = p, method = "historical")),
            `Gaussian VaR` = ~-as.numeric(PerformanceAnalytics::VaR(.x, p = p, method = "gaussian")),
            `Modified VaR` = ~-as.numeric(PerformanceAnalytics::VaR(.x, p = p, method = "modified")),
            `Historical ES` = ~-as.numeric(PerformanceAnalytics::ES(.x, p = p, method = "historical")),
            `Gaussian ES` = ~-as.numeric(PerformanceAnalytics::ES(.x, p = p, method = "gaussian")),
            `Modified ES` = ~-as.numeric(PerformanceAnalytics::ES(.x, p = p, method = "modified"))), .names = "{.fn}"), .groups = "drop") %>%
        gather(Measure, Loss, -Name) %>% # long format, one row per measure
        mutate(Method = stringr::word(Measure, 1), Type = stringr::word(Measure, 2)) # split the method from the type of measure

}
