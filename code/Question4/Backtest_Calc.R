# Calculates the monthly returns of each portfolio rule from its rebalance weights, letting weights drift between quarterly rebalances, and the turnover on each rebalance date.
Backtest_Calc <- function(R_Long, Weights){

    R_xts <- R_Long %>% select(date, Asset, Returns) %>% tbl2xts::tbl_xts(cols_to_xts = Returns, spread_by = Asset, Colnames_Exact = T) # wide monthly returns
    R_xts[is.na(R_xts)] <- 0 # a missing return only occurs for assets with no weight, so zero is harmless

    map(unique(Weights$Strategy), function(s) { # repeat for each portfolio rule
        W_xts <- Weights %>% filter(Strategy == s) %>% select(date, Asset, Weight) %>% tbl2xts::tbl_xts(cols_to_xts = Weight, spread_by = Asset, Colnames_Exact = T) # wide rebalance weights
        W_xts[is.na(W_xts)] <- 0 # an asset that is not eligible on a date has a zero weight
        R_s <- R_xts[zoo::index(R_xts) > min(zoo::index(W_xts)), colnames(W_xts)] # returns after the first rebalance, for the same assets
        Port <- rmsfuns::Safe_Return.portfolio(R = R_s, weights = W_xts, lag_weights = FALSE, geometric = TRUE, verbose = TRUE) # monthly portfolio returns, with weights drifting between rebalances, where weights set at a month end apply from the following month, so lagging them a day would wrongly let them earn the month in which they were chosen

        EOP <- Port$EOP.Weight %>% tbl2xts::xts_tbl() %>% set_names(c("date", colnames(W_xts))) %>% gather(Asset, Drift, -date) # the drifted weights at each month end, with the asset names restored since the conversion alters names that contain spaces
        Turn <- Weights %>% filter(Strategy == s) %>% left_join(EOP, by = c("date", "Asset")) %>% group_by(date) %>% summarise(Turnover = 0.5 * sum(abs(Weight - coalesce(Drift, 0))), .groups = "drop") %>% filter(date > min(date)) # half the absolute change from drifted to new weights

        list(Returns = Port$returns %>% tbl2xts::xts_tbl() %>% rename(Returns = 2) %>% mutate(Strategy = s), Turnover = Turn %>% mutate(Strategy = s)) # the two outputs for this rule
    }) %>%
        transpose() %>% # group the outputs by type
        map(bind_rows) # stack the rules

}
