# Calculates return, risk and market exposure statistics for every fund over the months in which the benchmark is available, for funds with a minimum track record.
Fund_Stats_Calc <- function(df, Min_Months = 36){

    if( !all(c("Name", "Group", "Returns", "BM") %in% names(df)) ) stop("... for Fund_Stats_Calc to work, first run Fund_Universe_Prep") # stop early if the universe has not been prepared

    df %>%
        filter(!is.na(BM)) %>% # only the months in which the benchmark is available
        group_by(Name, Group) %>% # one row per fund
        filter(n() >= Min_Months) %>% # funds need a minimum track record for the statistics to mean something
        summarise(Months = n(), # the length of the track record
                  Ret = prod(1 + Returns)^(12 / n()) - 1, # the annualised compounded return
                  BM_Ret = prod(1 + BM)^(12 / n()) - 1, # the benchmark's annualised return over the same months
                  Vol = sd(Returns) * sqrt(12), # the annualised volatility
                  Beta = coef(lm(Returns ~ BM))[2], # the sensitivity to the benchmark
                  Alpha = coef(lm(Returns ~ BM))[1] * 12, # the annualised return not explained by the benchmark
                  R2 = cor(Returns, BM)^2, # the share of return variance explained by the benchmark
                  TE = sd(Returns - BM) * sqrt(12), # the annualised tracking error
                  IR = mean(Returns - BM) * 12 / TE, # the information ratio, active return per unit of tracking error
                  MaxDD = max(1 - cumprod(c(1, 1 + Returns)) / cummax(cumprod(c(1, 1 + Returns)))), # the deepest peak to trough loss
                  .groups = "drop") %>%
        mutate(Excess = Ret - BM_Ret) # the annualised return above or below the benchmark

}
