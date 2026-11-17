# Fits a GARCH model to one series and forecasts its volatility forward, returning the recent fitted volatility, the forecast path and the long run level it converges to, following the forecasting section of Practical 6.
Garch_Forecast_Calc <- function(df, Series, Model = "gjrGARCH", N_Ahead = 126, Show_From = lubridate::ymd(20250101), Ann = 252){

    x <- df %>% filter(Name == Series, !is.na(dlogret)) %>% arrange(date) # the chosen series in date order
    xts_rtn <- xts::xts(x$dlogret, order.by = x$date) # its log returns as an xts series

    Spec <- rugarch::ugarchspec(variance.model = list(model = Model, garchOrder = c(1, 1)), mean.model = list(armaOrder = c(1, 0), include.mean = TRUE), distribution.model = "std") # the same specification as the model comparison
    Fit <- rugarch::ugarchfit(spec = Spec, data = xts_rtn) # fit the model to the full sample
    Fc <- rugarch::ugarchforecast(Fit, n.ahead = N_Ahead) # forecast volatility N_Ahead trading days forward

    Fc_Dates <- seq(max(x$date) + 1, by = "day", length.out = N_Ahead * 2) %>% .[!weekdays(.) %in% c("Saturday", "Sunday")] %>% head(N_Ahead) # the next N_Ahead weekdays
    Long_Run <- sqrt(rugarch::uncvariance(Fit)) * sqrt(Ann) # the unconditional volatility the forecast converges to

    bind_rows(tibble(date = as.Date(zoo::index(rugarch::sigma(Fit))), Sigma = as.numeric(rugarch::sigma(Fit)) * sqrt(Ann), Part = "Fitted") %>% filter(date >= Show_From), # the recent fitted volatility
              tibble(date = Fc_Dates, Sigma = as.numeric(rugarch::sigma(Fc)) * sqrt(Ann), Part = "Forecast")) %>% # the forecast path
        mutate(Name = Series, Model = Model, Long_Run = Long_Run, Half_Life = log(0.5) / log(rugarch::persistence(Fit))) # the series, its model, long run level and the days it takes a shock to halve

}
