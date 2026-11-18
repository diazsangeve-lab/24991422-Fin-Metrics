# Splits each fund's return variance into the part explained by the benchmark, the part shared with the rest of the peer group and the part unique to the fund.
Return_Drivers_Calc <- function(df, Min_Months = 60){ # define Return_Drivers_Calc, with inputs df and Min_Months

    if( !all(c("YM", "Name", "Group", "Returns", "BM") %in% names(df)) ) stop("... for Return_Drivers_Calc to work, first run Fund_Universe_Prep") # stop early if the universe has not been prepared

    Resids <- # build Resids through the steps below
        df %>% # start from the input data
        filter(!is.na(BM)) %>% # only the months in which the benchmark is available
        group_by(Name) %>% # one regression per fund
        filter(n() >= Min_Months) %>% # funds need a long enough record for a stable split
        mutate(Resid = resid(lm(Returns ~ BM))) %>% # the part of each month's return the benchmark does not explain
        ungroup() # remove the grouping again

    Resids %>% # start from Resids, built above
        group_by(YM) %>% # work month by month
        mutate(Herd = map_dbl(seq_along(Resid), ~median(Resid[setdiff(which(Group == "Peer"), .x)]))) %>% # the median non-market return of the other peers, which leaves the fund itself out so that it is not compared with itself
        group_by(Name, Group) %>% # one split per fund
        summarise(Market = summary(lm(Returns ~ BM))$r.squared, # the share explained by the benchmark alone
                  Full = summary(lm(Returns ~ BM + Herd))$r.squared, # the share explained once the peer group's common bets are added
                  .groups = "drop") %>% # drop the grouping once the summary is done
        mutate(Shared = Full - Market, # the extra share explained by moving with the peer group
               Unique = 1 - Full) %>% # what is left is unique to the fund
        select(Name, Group, Market, Shared, Unique) # keep the three parts

}
