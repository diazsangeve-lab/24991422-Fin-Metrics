# Stacks the peer funds, the client's fund and the benchmark into one tidy monthly data frame, with the benchmark return of the same month alongside every fund return, and drops any peer that is a copy of the client's fund.
Fund_Universe_Prep <- function(Peers, Fund, BM_Monthly, Fund_Label = "ABC Fund", End_Date = NULL, Clone_Tol = 1e-8){ # define Fund_Universe_Prep, with inputs Peers, Fund, BM_Monthly, Fund_Label, End_Date and Clone_Tol

    # For stepping through uncomment:
    # Peers <- Active_Manager_Peers ; Fund <- ABC_Fund ; BM_Monthly <- BM_Monthly ; Fund_Label <- "ABC Fund" ; Clone_Tol <- 1e-8

    if( !all(c("date", "Name", "Returns") %in% names(Peers)) ) stop("... Peers must have columns called 'date', 'Name' and 'Returns'") # stop early if the peer data is not in the expected shape
    if( !all(c("YM", "BM") %in% names(BM_Monthly)) ) stop("... BM_Monthly must have columns called 'YM' and 'BM'") # stop early if the benchmark is not in the expected shape

    Clones <- # build Clones through the steps below
        Peers %>% # start from the peer returns
        inner_join(Fund %>% select(date, Fund_Ret = Returns), by = "date") %>% # line each peer up with the client's fund month by month
        group_by(Name) %>% # one check per peer
        summarise(Overlap = n(), Max_Diff = max(abs(Returns - Fund_Ret)), .groups = "drop") %>% # the shared months and the largest difference in returns
        filter(Overlap >= 12, Max_Diff < Clone_Tol) %>% # a peer with identical returns over at least a year is the client's fund under another name
        pull(Name) # the names of any such copies

    if( length(Clones) > 0 ) message(glue::glue("... dropping {paste(Clones, collapse = ', ')} from the peers, as its returns are identical to {Fund_Label}")) # say which peers were removed

    df <- # build df through the steps below
        bind_rows(Peers %>% filter(!Name %in% Clones) %>% mutate(Group = "Peer"), # every active manager in the peer group, without copies of the client's fund
                  Fund %>% mutate(Name = Fund_Label, Group = Fund_Label)) %>% # the client's fund, with a readable name
        mutate(YM = format(date, "%Y%B")) %>% # a year and month label that matches the benchmark
        left_join(BM_Monthly %>% select(YM, BM), by = "YM") %>% # the benchmark return of the same month, missing before the benchmark starts
        arrange(Name, date) # order each fund in time

    if( !is.null(End_Date) ) df <- df %>% filter(date <= End_Date) # stop at the last month that every series covers

    attr(df, "Clones") <- Clones # keep the names of any dropped copies, so that they can be reported
    df # return the prepared data

}
