# Measures how concentrated each index is in its largest stocks and sectors on every month end.
Concentration_Calc <- function(Holdings, Top_N = 10){

    if( !all(c("date", "Fund", "Tickers", "Sector", "Weight") %in% names(Holdings)) ) stop("... first run Size_Holdings_Prep on the holdings") # stop early if the holdings are not prepared

    Sector_HHI <-
        Holdings %>%
        group_by(Fund, date, Sector) %>% # one weight per sector per index and month
        summarise(W = sum(Weight), .groups = "drop") %>% # the sector's total weight
        group_by(Fund, date) %>% # one number per index and month
        summarise(Sector_HHI = sum(W^2), .groups = "drop") # the Herfindahl index of sector weights, which is one when an index holds a single sector

    Holdings %>%
        group_by(Fund, date) %>% # one set of numbers per index and month
        summarise(N = n(), # the number of constituents
                  HHI = sum(Weight^2), # the Herfindahl index of stock weights
                  Eff_N = 1 / sum(Weight^2), # the effective number of stocks, the number of equally weighted stocks that would give the same concentration
                  Top_Share = sum(sort(Weight, decreasing = TRUE)[1:min(Top_N, n())]), # the weight held in the largest Top_N stocks
                  .groups = "drop") %>%
        left_join(Sector_HHI, by = c("Fund", "date")) %>% # attach the sector concentration
        mutate(Eff_Sectors = 1 / Sector_HHI) # the effective number of sectors

}
