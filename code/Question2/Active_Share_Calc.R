# Calculates the active share of each index relative to a benchmark on every month end, as half the sum of absolute weight differences.
Active_Share_Calc <- function(Holdings, BM = "Capped SWIX"){ # define Active_Share_Calc, with inputs Holdings and BM

    if( !BM %in% unique(as.character(Holdings$Fund)) ) stop(glue::glue("... {BM} is not one of the indexes in the holdings provided")) # stop early if the benchmark is missing

    BM_W <- Holdings %>% filter(Fund == BM) %>% select(date, Tickers, BM_Weight = Weight) # the benchmark weights

    Holdings %>% # start from the holdings
        filter(Fund != BM) %>% # every index other than the benchmark
        select(Fund, date, Tickers, Weight) %>% # keep only what the calculation needs
        group_split(Fund) %>% # treat each index on its own
        map_df(~full_join(.x %>% select(date, Tickers, Weight), BM_W, by = c("date", "Tickers")) %>% # match the index and benchmark weights, keeping stocks held by either
                   filter(date %in% unique(.x$date)) %>% # only months in which the index exists
                   mutate(across(c(Weight, BM_Weight), ~coalesce(., 0))) %>% # a stock missing from one side has a zero weight there
                   group_by(date) %>% # one number per month
                   summarise(Active_Share = 0.5 * sum(abs(Weight - BM_Weight)), .groups = "drop") %>% # half the sum of absolute weight differences, which is zero for an exact copy and one for no overlap
                   mutate(Fund = unique(.x$Fund))) # label the index

}
