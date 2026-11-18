# Cleans monthly index holdings by labelling missing sectors, giving the indexes readable names and rescaling weights to sum to one on every date.
Size_Holdings_Prep <- function(Holdings, Fund_Names = c(CappedSWIX = "Capped SWIX", LargeCaps = "Large Caps", MidCaps = "Mid Caps", SmallCaps = "Small Caps")){ # define Size_Holdings_Prep, with inputs Holdings and Fund_Names

    if( !all(c("date", "Tickers", "Sector", "Weight", "Fund") %in% names(Holdings)) ) stop("... Holdings must have columns called 'date', 'Tickers', 'Sector', 'Weight' and 'Fund'") # stop early if the holdings are not in the expected shape

    Holdings %>% # start from the holdings
        mutate(Sector = coalesce(Sector, "Unclassified"), # give stocks with no sector a label
               Fund = factor(recode(Fund, !!!Fund_Names), levels = unname(Fund_Names))) %>% # readable index names in a fixed order
        group_by(Fund, date) %>% # rescale within each index and month
        mutate(Weight = Weight / sum(Weight, na.rm = T)) %>% # the reported weights do not always sum to one, so they are rescaled
        ungroup() # remove the grouping again

}
