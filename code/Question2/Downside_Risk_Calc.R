# Calculates downside risk measures from monthly returns for every series, following the downside risk table of Practical 2.
Downside_Risk_Calc <- function(Monthly){

    Pick <- function(Tab, Row) as.numeric(Tab[[1]])[rownames(Tab) == Row] # one value from the table by its row name, since the table's column has no name

    Monthly %>%
        group_by(Name) %>% # one set of measures per series
        summarise(R = list(xts::xts(Returns, order.by = date)), .groups = "drop") %>% # each series as an xts object
        mutate(Tab = map(R, ~PerformanceAnalytics::table.DownsideRisk(.x, scale = 12, p = 0.95))) %>% # the practical's downside risk table for each series
        mutate(`Downside deviation (ann.)` = map_dbl(Tab, Pick, Row = "Downside Deviation (0%)") * sqrt(12), # the volatility of returns below zero, annualised
               `Semi deviation (ann.)` = map_dbl(Tab, Pick, Row = "Semi Deviation") * sqrt(12), # the volatility of returns below the mean, annualised
               `Monthly historical VaR (95%)` = -map_dbl(Tab, Pick, Row = "Historical VaR (95%)"), # the loss exceeded in one month in twenty
               `Monthly historical ES (95%)` = -map_dbl(Tab, Pick, Row = "Historical ES (95%)"), # the average loss in those months
               `Maximum drawdown` = map_dbl(Tab, Pick, Row = "Maximum Drawdown")) %>% # the deepest peak to trough loss
        select(-R, -Tab) %>% # drop the working columns
        gather(Measure, Value, -Name) # long format, one row per measure

}
