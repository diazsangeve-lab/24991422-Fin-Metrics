# Calculates rolling annualised returns for every fund and summarises the peer group into percentile bands, alongside the client's fund, its percentile rank and the benchmark.
Peer_Percentile_Calc <- function(df, BM_Monthly, Fund_Label = "ABC Fund", NSel = 36, Probs = c(0.05, 0.25, 0.5, 0.75, 0.95)){ # define Peer_Percentile_Calc, with inputs df, BM_Monthly, Fund_Label, NSel and Probs

    Rolled <- # build Rolled through the steps below
        df %>% # start from the input data
        select(date, Name, Group, Returns) %>% # the fund returns, without the benchmark column
        bind_rows(BM_Monthly %>% transmute(date, Name = "Capped SWIX", Group = "Benchmark", Returns = BM)) %>% # the benchmark as one more series
        arrange(date) %>% # order by date
        group_by(Name) %>% # work within each series
        mutate(Span = as.numeric(date - lag(date, NSel - 1))) %>% # the number of days each window covers
        ungroup() %>% # remove the grouping again
        Roll_Return_Calc(NSel = NSel) %>% # rolling annualised returns, from the question 2 function
        filter(Span < NSel * 31) # drop windows that jump over a gap in a fund's history

    Bands <- # build Bands through the steps below
        Rolled %>% # start from Rolled, built above
        filter(Group == "Peer") %>% # only the peer group forms the bands
        group_by(date) %>% # one set of bands per month
        reframe(N_Peers = n(), Band = paste0("P", sprintf("%02d", Probs * 100)), Value = quantile(RollRet, Probs)) %>% # the chosen percentiles of peer returns, one row per percentile
        spread(Band, Value) # one column per percentile

    Fund_Rank <- # build Fund_Rank through the steps below
        Rolled %>% # start from Rolled, built above
        filter(Group %in% c("Peer", Fund_Label)) %>% # the fund and its peers
        group_by(date) %>% # rank month by month
        summarise(Fund_Ret = RollRet[Group == Fund_Label][1], # the fund's rolling return
                  Fund_Pct = mean(RollRet[Group == "Peer"] < Fund_Ret), # the share of peers the fund beat
                  .groups = "drop") # drop the grouping once the summary is done

    Bands %>% # start from Bands, built above
        left_join(Fund_Rank, by = "date") %>% # attach the fund
        left_join(Rolled %>% filter(Group == "Benchmark") %>% select(date, BM_Ret = RollRet), by = "date") # attach the benchmark

}
