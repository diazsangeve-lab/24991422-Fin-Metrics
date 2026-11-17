# Calculates, for every month, the share of active managers who beat the benchmark over a trailing window and the spread of their returns, following the dispersion practical.
Manager_Hit_Rate <- function(Managers, BM_Monthly, NSel = 12){

    # For stepping through uncomment:
    # Managers <- Active_Manager_Peers ; BM_Monthly <- BM_M ; NSel <- 12

    if( !all(c("date", "Name", "Returns") %in% names(Managers)) ) stop("... Managers must have columns called 'date', 'Name' and 'Returns'") # stop early if the manager data is not in the expected shape
    if( !all(c("YM", "BM") %in% names(BM_Monthly)) ) stop("... BM_Monthly must have columns called 'YM' and 'BM'") # stop early if the benchmark data is not in the expected shape

    Managers %>%
        filter(!is.na(Returns)) %>% # drop missing manager returns
        mutate(YM = format(date, "%Y%m")) %>% # a year and month label, so that month ends that fall on different days still match
        left_join(BM_Monthly %>% select(YM, BM), by = "YM") %>% # attach the benchmark return of the same month
        filter(!is.na(BM)) %>% # only months in which the benchmark is known
        arrange(date) %>% # order by date before rolling
        group_by(Name) %>% # roll within each manager
        mutate(Roll_Fund = RcppRoll::roll_prod(1 + Returns, n = NSel, fill = NA, align = "right") - 1, # the manager's compounded return over the window
               Roll_BM = RcppRoll::roll_prod(1 + BM, n = NSel, fill = NA, align = "right") - 1) %>% # the benchmark's compounded return over the same months
        ungroup() %>% # remove the grouping again
        filter(!is.na(Roll_Fund)) %>% # keep only complete windows
        group_by(YM) %>% # one set of numbers per month
        summarise(date = max(date), # the month end
                  N = n(), # the number of managers with a full window
                  Share_Outperform = mean(Roll_Fund > Roll_BM), # the share of managers ahead of the benchmark over the window
                  Median_Excess = median(Roll_Fund - Roll_BM), # the typical manager's return relative to the benchmark
                  Mgr_Spread = sd(Roll_Fund - Roll_BM), # how far apart managers' relative returns are, a measure of how differently they are positioned
                  .groups = "drop")

}
