# Summarises each portfolio rule's backtest into return, risk, tail risk, turnover and diversification measures.
Strategy_Stats_Calc <- function(BT, W){ # define Strategy_Stats_Calc, with inputs BT and W

    Perf <- BT$Returns %>% group_by(Strategy) %>% summarise(Ret = prod(1 + Returns)^(12 / n()) - 1, # the annualised compounded return
                                                            Vol = sd(Returns) * sqrt(12), # the annualised volatility
                                                            MaxDD = max(1 - cumprod(1 + Returns) / cummax(cumprod(1 + Returns))), # the deepest peak to trough loss
                                                            CVaR = -mean(sort(Returns)[1:ceiling(0.05 * n())]), # the average of the worst five percent of months, as a positive loss
                                                            .groups = "drop") %>% mutate(Ratio = Ret / Vol) # return per unit of volatility

    Div <- W %>% group_by(Strategy, date) %>% summarise(Eff_N = 1 / sum(Weight^2), Eff_Risk = 1 / sum(pmax(Risk_Share, 0)^2), .groups = "drop") %>% group_by(Strategy) %>% summarise(Eff_N = mean(Eff_N), Eff_Risk = mean(Eff_Risk), .groups = "drop") # the effective number of holdings and of risk sources, averaged over rebalance dates

    Perf %>% left_join(Div, by = "Strategy") %>% left_join(BT$Turnover %>% group_by(Strategy) %>% summarise(Turnover = mean(Turnover), .groups = "drop"), by = "Strategy") # one row per rule

}
