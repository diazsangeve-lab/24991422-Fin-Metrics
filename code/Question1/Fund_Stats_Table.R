# Builds a table of return, risk and relative performance statistics for one or more funds against a benchmark, using monthly returns.
Fund_Stats_Table <- function(df, BM_Name = "Capped SWIX", Rf_df = NULL){ # define Fund_Stats_Table, with inputs df, BM_Name and Rf_df

    # For stepping through uncomment:
    # df <- Port_Daily ; BM_Name <- "Capped SWIX" ; Rf_df <- STeFI

    if( !BM_Name %in% unique(df$Name) ) stop(glue::glue("... {BM_Name} is not a Name in the data frame provided")) # stop early if the benchmark is missing

    Monthly <- # build Monthly through the steps below
        df %>% # start from the input data
        mutate(YM = format(date, "%Y%m")) %>% # a year and month label
        group_by(Name, YM) %>% # one number per series per month
        summarise(date = max(date), Returns = prod(1 + Returns) - 1, .groups = "drop") # chain the daily returns into a monthly return

    Monthly_xts <- Monthly %>% select(date, Name, Returns) %>% tbl2xts::tbl_xts(cols_to_xts = Returns, spread_by = Name, Colnames_Exact = T) # wide monthly returns
    BM_xts <- Monthly_xts[, BM_Name] # the benchmark column
    Fund_xts <- Monthly_xts[, setdiff(colnames(Monthly_xts), BM_Name), drop = FALSE] # every other column

    Rf <- 0 # default risk free rate if no cash index is given
    if( !is.null(Rf_df) ) Rf <- Rf_df %>% mutate(YM = format(date, "%Y%m")) %>% group_by(YM) %>% summarise(date = max(date), Rf = prod(1 + Returns) - 1, .groups = "drop") %>% filter(YM %in% Monthly$YM) %>% tbl2xts::tbl_xts(cols_to_xts = Rf) # monthly cash returns, used for the Sharpe ratio

    Stats_One <- function(x, name){ # define Stats_One, with inputs x and name
        Rf_use <- if(is.numeric(Rf)) 0 else Rf[zoo::index(x)] # align the cash returns to the fund's months
        tibble(Name = name, # the series name
               `Return (Ann.)` = as.numeric(PerformanceAnalytics::Return.annualized(x, scale = 12)), # geometric annualised return
               `SD (Ann.)` = as.numeric(PerformanceAnalytics::sd.annualized(x, scale = 12)), # annualised volatility
               `Sharpe Ratio` = as.numeric(PerformanceAnalytics::SharpeRatio.annualized(x, Rf = Rf_use, scale = 12)), # excess return per unit of volatility, using cash as the risk free rate
               `Max Drawdown` = as.numeric(PerformanceAnalytics::maxDrawdown(x)), # the largest peak to trough loss
               `Modified CVaR (95%)` = as.numeric(PerformanceAnalytics::ES(x, p = 0.95, method = "modified")), # expected monthly loss in the worst 5 percent of months, adjusted for skewness and kurtosis
               Beta = as.numeric(PerformanceAnalytics::CAPM.beta(x, BM_xts)), # sensitivity to the benchmark
               `Beta Bull` = as.numeric(PerformanceAnalytics::CAPM.beta.bull(x, BM_xts)), # sensitivity in months when the benchmark rises
               `Beta Bear` = as.numeric(PerformanceAnalytics::CAPM.beta.bear(x, BM_xts)), # sensitivity in months when the benchmark falls
               `Up Capture` = as.numeric(PerformanceAnalytics::UpDownRatios(x, BM_xts, method = "Capture", side = "Up", geometric = FALSE)), # average fund return in benchmark up months relative to the benchmark, using the arithmetic version because the geometric default compounds every up month together and overstates the ratio
               `Down Capture` = as.numeric(PerformanceAnalytics::UpDownRatios(x, BM_xts, method = "Capture", side = "Down", geometric = FALSE)), # average fund return in benchmark down months relative to the benchmark, where below one is protective
               `Tracking Error` = as.numeric(fmxdat::Safe_TE(x, BM_xts, scale = 12)), # annualised volatility of returns relative to the benchmark
               `Information Ratio` = as.numeric(PerformanceAnalytics::InformationRatio(x, BM_xts, scale = 12))) # active return per unit of tracking error
    }

    bind_rows(Stats_One(BM_xts, BM_Name), # the benchmark first, so the reader has a reference row
              map_df(colnames(Fund_xts), ~Stats_One(Fund_xts[, .x], .x))) %>% # then every fund
        mutate(across(c(Beta, `Beta Bull`, `Beta Bear`, `Up Capture`, `Down Capture`, `Tracking Error`, `Information Ratio`), ~ifelse(Name == BM_Name, NA, .))) # relative measures are meaningless for the benchmark against itself

}
